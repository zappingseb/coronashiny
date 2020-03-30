source("./shiny/data_gen.R")

library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(DT)



population_data <- read.csv("./shiny/population-figures-by-country-csv_csv.csv")
population_data_short <- rbind(
  population_data %>% select(Country, Country_Code, Year_2016) %>%
    mutate(Country = str_replace(Country, "Korea, Rep.", "Korea, South")) %>%
    mutate(Country = str_replace(Country, "Czech Republic", "Czechia")) %>%
    mutate(Country = str_replace(Country, "Russian Federation", "Russia")) %>%
    mutate(Country = str_replace(Country, "United States", "US")) %>%
    mutate(Country = str_replace(Country, "Iran, Islamic Rep.", "Iran")),
  data.frame(Country = "Taiwan", Country_Code = "TAI", Year_2016 = 23780000)
)
setwd("shiny")
if (dir.exists("COVID-19")) {
  setwd("COVID-19")
  system("git pull")
  setwd("..")
} else {
  system("git clone https://github.com/CSSEGISandData/COVID-19.git", timeout = 1000)
}
covid_global_confirmed <- read.csv("./COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

leaflet(covid_global_confirmed %>% select(Lat, Long, X3.24.20) %>% head %>% rename(val = X3.24.20)) %>%  addTiles() %>%
  addWebGLHeatmap(lng=~Long, lat=~Lat, size = 60000)
leaflet(covid_global_confirmed %>% select(Lat, Long, X3.24.20) %>% head %>% rename(val = X3.24.20)) %>%
  addProviderTiles(providers$Thunderforest.TransportDark) %>%
  addWebGLHeatmap(lng=~Long, lat=~Lat, intensity = ~val)

covid_data <- per_country_data(covid_global_confirmed)
covid_data_deaths <- read.csv("./COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

default_countries <- covid_data$Country.Region



confirmed <- new_data_gen(covid_data, default_countries)
deaths <- new_data_gen(per_country_data(covid_data_deaths), default_countries, FALSE) %>% rename(deaths = value)
recovered <- generate_from_daily()

# recovered <- new_data_gen(per_country_data(read.csv("./COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")),
                          # default_countries, FALSE) %>% rename(recovered = value)

merged_data <- left_join(
  left_join(confirmed, deaths, by = c("country", "date")),
  recovered, by = c("country", "date")
)


plot_ly(
  data = merged_data,
  hoverinfo = "",
  type = "scatter",
  transforms = list(
    list(
      type = 'groupby',
      groups = merged_data$country,
      styles = lapply(seq_along(unique(merged_data$country)), function(x){
        palette_col <- viridisLite::viridis(n = length(unique(merged_data$country)))
        list(target = unique(merged_data$country)[x], value = list(line = list(color = palette_col[x]), 
                                                                         marker = list(color = alpha(palette_col[x], 0.6))))
      })
    )
  )
) %>% add_trace(
  x = ~date,
  y = ~value,
  name = "Total cases",
  type = "scatter",
  mode = "lines"
) %>%
  layout(
    xaxis = list(
      title = "Date"
    ),
    yaxis = list(
      title = "Total cases",
      range = c(0, max(as.numeric(merged_data$value), na.rm = TRUE) + 1)
    )
  )

df <- key_factors(merged_data, population_data_short)

brks_clrs_doubling_days <- breaks_colors(df$doubling_days, reverse = TRUE)
brks_clrs_max_exponential_time <- breaks_colors(df$max_exponential_time)
datatable(df,
          rownames= FALSE,
          extensions = c("FixedHeader"),
          colnames = c(
            "Country",
            "Maximum time of exponential growth in a row (since Jan 1st)",
            "Days to double infections (from today)",
            "Exponential growth today?",
            "Confirmed Cases (Johns Hopkins CSSE)",
            "Deaths (Johns Hopkins CSSE)",
            "Population (in Mio)",
            "Confirmed Cases on 100,000 inhabitants",
            "mortality Rate (%)"
          ),
          options = list(
            pageLength = 200,
            fixedHeader = TRUE
          )
) %>%
  formatStyle("doubling_days",
              backgroundColor = styleInterval(brks_clrs_doubling_days$brks, brks_clrs_doubling_days$clrs),
              color = styleInterval(brks_clrs_doubling_days$brks,
                                    c(
                                      rep("white", floor(1 * length(brks_clrs_doubling_days$clrs) / 4)),
                                      rep("black", ceiling(3 * length(brks_clrs_doubling_days$clrs) / 4))
                                    )
              )
  ) %>%
  formatStyle("still_exponential",
              backgroundColor = styleEqual(c("no", "yes"), c("rgb(249,249,249)", "rgb(127,0,0)")),
              color = styleEqual(c("no", "yes"), c("rgb(0,0,0)", "rgb(255,255,255)"))
  ) %>%
  formatStyle("max_exponential_time",
              backgroundColor = styleInterval(brks_clrs_max_exponential_time$brks, brks_clrs_max_exponential_time$clrs),
              color = styleInterval(brks_clrs_max_exponential_time$brks,
                                    c(
                                      rep("black", floor(1 * length(brks_clrs_max_exponential_time$clrs) / 4)),
                                      rep("white", ceiling(3 * length(brks_clrs_max_exponential_time$clrs) / 4))
                                    )
              )
  )
