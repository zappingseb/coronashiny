per_country_data <- function(covid_data) {
  covid_data %>%
    mutate(Country.Region = str_replace(Country.Region, "\\*", "")) %>% select(-Province.State, -Lat, -Long) %>% group_by(Country.Region) %>%
    summarise_all(funs(sum))
}

new_data_gen <- function(covid_data, countries) {
  
  start_date <- str_replace(names(covid_data)[2], "X", "0") %>%
    as.Date(format="%m.%d.%y")
  end_date <- str_replace(names(covid_data)[length(names(covid_data))], "X", "0") %>%
    as.Date(format="%m.%d.%y")
  
  dates_covid_19_confirmed <- seq.Date(start_date, to = end_date, by = 1)
  
  stopifnot((ncol(covid_data) - 1) == length(dates_covid_19_confirmed))
  
  covid_data_selected <- covid_data %>%
    filter(Country.Region %in% countries)
  
  covid_data_selected <- as.data.frame(t(covid_data_selected))
  covid_data_selected$dates <- c("Country", dates_covid_19_confirmed)
  colnames(covid_data_selected) <- c(covid_data_selected[1, 1:length(countries)] %>% unlist() %>% as.character(), "country")
  covid_data_selected <- covid_data_selected[-1,]
  covid_data_selected$country <- dates_covid_19_confirmed
  selected_data <- if (is.na(colnames(covid_data_selected))) {
    covid_data_selected[, -which(is.na(colnames(covid_data_selected)))]
  } else {
    covid_data_selected
  }
  country_data <- gather(selected_data, key = country) %>%
    filter(!is.na(country))
  
  return(
    cbind(
      add_growth_rate(country_data),
      data.frame(date = rep(dates_covid_19_confirmed, length(countries)))
    )
  )
}

add_rel_data <- function(covid_data, pop_data) {
  
  
  pop_data <- pop_data %>% filter(Country %in% c(countries))
  
  covid_data %>% rowwise() %>%
    mutate(
      value_rel = 100 * as.numeric(value) / pop_data %>%
        filter(Country == country) %>%
        pull()
    ) %>%
    rbind()
}

add_growth_rate <- function(covid_data_long) {
  covid_data_long %>%
    group_by(country) %>%
    mutate(
      doubling_days = ifelse(
        lag(as.numeric(value), n = 4) > 15,
        4 * log(2)/log(as.numeric(value)/as.numeric(lag(value, n = 4))),
        Inf
      ),
      growth.factor = ifelse(
        lag(as.numeric(value), n = 4) > 15,
        (as.numeric(value) - as.numeric(lag(value, n = 4)))
             /
               (as.numeric(lag(value, n = 4)) - as.numeric(lag(value, n = 8))),
        0
      )
    ) %>%
    mutate(
      growth.factor = ifelse(growth.factor > 5, 5, ifelse(growth.factor < 0, 0, growth.factor)),
      doubling_days = ifelse(doubling_days > 1000, Inf, doubling_days)
    ) %>%
    replace_na(list(growth.factor = 0, doubling_days = 0)) %>%
    # Check if there was exponential growth
    mutate(is_exponential = case_when(
      doubling_days > 0 & doubling_days < 6.5 ~ 1,
      TRUE ~ 0
    )) %>%
    add_doubling_rates() %>%
    ungroup() 
}

add_doubling_rates <- function(covid_data_long_grouped) {
  covid_data_long_grouped %>%
    mutate(id = row_number()) %>%
    # Calculate how many days in a row the current situation is on:
    # https://stackoverflow.com/questions/32994060/r-cumulative-sum-by-condition-with-reset
    group_by(group_rm = paste(country, cumsum(c(0, diff(is_exponential) != 0)))) %>%
    mutate(days_in_a_row = row_number()) %>%
    ungroup() %>%
    select(-group_rm)
}

breaks_colors <- function(vec, reverse = FALSE) {
  brks <- quantile(vec, probs = seq(.05, .95, .05), na.rm = TRUE)
  # clrs <- rev(round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
  # {paste0("rgb(255,", ., ",", ., ")")})
  clrs_hex <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, "OrRd"))(length(brks) + 1)
  
  clrs <- apply(
    if(reverse) col2rgb(rev(clrs_hex)) else col2rgb(clrs_hex),
    2,
    function(x) paste0("rgb(", x["red"],",",x["green"],",",x["blue"],")")
  )
  list(brks = brks, clrs = clrs)
}

key_factors <- function(covid_data_long, population_data) {
  max_exp_data <- covid_data_long %>%
    group_by(country) %>%
    filter(is_exponential == 1) %>%
    summarize(max_exponential_time = max(days_in_a_row)) %>%
    ungroup()
  
  factor_data <- bind_cols(max_exp_data,
            covid_data_long %>%
              filter(country %in% max_exp_data$country) %>%
              group_by(country) %>% 
              filter(date == max(date)) %>%
              mutate(
                doubling_days = round(doubling_days, 2),
                still_exponential = ifelse(is_exponential == 1, "yes", "no")
              ) %>%
              select(doubling_days, still_exponential, value)
            ) %>%
    select(-country1) %>%
    rename(Country = country) %>%
    dplyr::arrange(doubling_days)
  
  factor_data <- full_join(factor_data, population_data) %>% 
    filter(!is.na(value)) %>%
    rename( population = Year_2016) %>%
    mutate(per_100000 = round(as.numeric(value)/(population/100000), 1),
           population = round(population/1000000, 2)) %>%
    select(-Country_Code)
}
