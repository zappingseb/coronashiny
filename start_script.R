source("./shiny/data_gen.R")

library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(RColorBrewer)

default_countries <- c("Switzerland", "Korea, South", "Italy", "China (only Hubei)")

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

covid_data <- read.csv("./COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
covid_data_deaths <- read.csv("./COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

data <- new_data_gen(per_country_data(covid_data), default_countries)
deaths <- new_data_gen(per_country_data(covid_data_deaths), default_countries, FALSE) %>% rename(deaths = value)

merged_data <- dplyr::left_join(data, deaths, by = c("country", "date")) %>%
  add_mortality

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
