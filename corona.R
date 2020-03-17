library(ggplot2)
library(rlang)
library(dplyr)
library(stringr)
library(tidyverse)
population_data <- read.csv("~/population-figures-by-country-csv_csv.csv")
population_data_short <- rbind(population_data %>% select(Country, Country_Code, Year_2016) %>%
                                 mutate(Country = str_replace(Country, "Korea, Rep.", "Korea, South")),
                               data.frame(Country = "Taiwan", Country_Code = "TAI", Year_2016 = 23780000)
)

setwd("~/COVID-19")
system("git pull")
setwd("~")
covid19_confirmed <- read.csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
  mutate(Country.Region = str_replace(Country.Region, "\\*", ""))
covid19_recovered <-  read.csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")




country_selection <- c("Germany", "Korea, South", "Taiwan", "Switzerland")

new_data_gen <- function(covid_data, countries) {
  
  start_date <- str_replace(names(covid_data)[5], "X", "0") %>%
    as.Date(format="%m.%d.%y")
  end_date <- str_replace(names(covid_data)[length(names(covid_data))], "X", "0") %>%
    as.Date(format="%m.%d.%y")
  
  dates_covid_19_confirmed <- seq.Date(start_date, to = end_date, by = 1)
  
  stopifnot((ncol(covid_data) - 4) == length(dates_covid_19_confirmed))
  
  covid_data_selected <- covid_data %>%
    filter(Country.Region %in% countries) %>%
    select(-Lat, -Long, -Province.State)
  
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
      country_data,
      data.frame(date = rep(dates_covid_19_confirmed, length(countries)))
    )
  )
}




ggplotdata <- new_data_gen(covid19_confirmed, country_selection, population_data_short) %>%
  mutate(status = "confirmed")

ggplot_data_recovered <- cbind(
  new_data_gen(covid19_recovered, country_selection, population_data_short),
  data.frame(date = rep(dates_covid_19_confirmed, length(country_selection)))
) %>% mutate(status = "recovered")

ggplot(data = rbind(ggplotdata), aes(x = date)) + 
  geom_line(aes(y = as.numeric(value), color = country, linetype = status)) +
  geom_point(aes(y = value_rel * 500000, color = country, linetype = status)) +
  scale_y_log10(
    "Absolut cases", 
    sec.axis = sec_axis(~ . / 500000, name = "Relative cases [%]")
  )



plot_ly(
  data = ggplotdata,
  hoverinfo = "",
  type = "scatter",
  mode = "lines",
  transforms = list(
    list(
      type = 'groupby',
      groups = ggplotdata$country,
      styles = lapply(seq_along(unique(ggplotdata$country)), function(x){
        palette_col <- viridisLite::viridis(n = length(unique(ggplotdata$country)))
        list(target = unique(ggplotdata$country)[x], value = list(line = list(color = palette_col[x])))
      })
    )
  )
) %>%
  add_trace(
    x = ~date,
    y = ~value,
    name = "Total cases",
    legendgroup = "group 1"
  ) %>%
  add_trace(
    x = ~date,
    y = ~value_rel,
    name = "Relative cases",
    line = list(dash = 'dot'),
    yaxis = "y2",
    legendgroup = "group 2"
  ) %>%
  layout(
    xaxis = list(
      title = "Date"
    ),
    yaxis = list(
      title = "Total cases",
      range = c(0, max(as.numeric(ggplotdata$value)) + 1)
      
    ),
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      range = c(0, max(ggplotdata$value_rel)),
      title = "Realtive cases / inhabitant",
      zeroline = FALSE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = FALSE
    )
  )