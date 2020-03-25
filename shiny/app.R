#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(stringr)
library(shiny)
library(shinymaterial)
library(plotly)
library(viridisLite)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(RColorBrewer)
library(shinyjs)
library(covid19italy)

source("data_gen.R")
source("about.R")
source("timeline_charts.R")
source("dt_table.R")
source("italy.R")
source("fun.R")

#---- population data ----
population_data <- read.csv("./population-figures-by-country-csv_csv.csv")
population_data_short <- rbind(
  population_data %>% select(Country, Country_Code, Year_2016) %>%
    mutate(Country = str_replace(Country, "Korea, Rep.", "Korea, South")) %>%
    mutate(Country = str_replace(Country, "Czech Republic", "Czechia")) %>%
    mutate(Country = str_replace(Country, "Russian Federation", "Russia")) %>%
    mutate(Country = str_replace(Country, "United States", "US")) %>%
    mutate(Country = str_replace(Country, "Iran, Islamic Rep.", "Iran")),
  data.frame(Country = c("Taiwan", "China (only Hubei)"), Country_Code = c("TAI", "XXX"), Year_2016 = c(23780000, 58500000))
)

#---- !! UI !! ----
# Define UI for application that draws a histogram
ui <- material_page(
  nav_bar_color = "blue",
  
  title = "COVID-19 data",
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(type = "text/javascript", src = "jquery_browser.js")
  ),
  
  #---- material_side_nav ----
  material_side_nav(
    fixed = TRUE, 
    background_color = "white",
    image_source = "img/corona-4938929_1920.jpg",
    tags$a(class="sidenav-close", "data-target"="slide-out", href="#", "x"),
    # Place side-nav tabs within side-nav
    material_side_nav_tabs(
      side_nav_tabs = c(
        "All Countries Table" = "all_countries",
        "Timeline Charts" = "housing_prices",
        "Italy Charts" = "italy",
        "Fun facts" = "fun",
        "About" = "about"
      ),
      icons = c("format_list_bulleted", "insert_chart", "local_pizza", "toys", "info_outline")
    ),
    div(style="height:50px"),
    material_card(
      
          htmlOutput("last_modified")
    )
  ),
  #---- UI objects ----
  material_side_nav_tab_content(
    side_nav_tab_id = "all_countries",
    dt_tableUI("dt_table_module")
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "housing_prices",
    timeline_chartsUI("timeline_charts_module")
        
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "italy",
    italyUI("italy_module")
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "fun",
    funUI("fun_module")
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "about",
    aboutUI("about_module")
  ),
  tags$script("
              $('a.waves-effect').on('click', function (e) {
  if($('.shiny-material-side-nav-tab.active').attr('id') == 'all_countries_tab_id'){
    $('#dt_table_module-MaxDoublingTime thead').show()
  } else {
    $('#dt_table_module-MaxDoublingTime thead').hide()
  };

});

// $('.sidenav-close').click(function() {
//     $('.sidenav').css('transform','');
//    $('.sidenav-overlay').hide();
//  })
              ")
 
)

#---- !! server !! ----
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #---- git pull ----
  git_pull <- reactive({
    if (dir.exists("COVID-19")) {
      setwd("COVID-19")
      system("git pull")
      setwd("..")
    } else {
      
      system("git clone https://github.com/CSSEGISandData/COVID-19.git", timeout = 1000)
    }
  })
  
  git_pull_italy <- reactive({
    if (dir.exists("covid19Italy")) {
      setwd("covid19Italy")
      system("git pull")
      setwd("..")
    } else {
      
      system("git clone https://github.com/RamiKrispin/covid19Italy.git", timeout = 1000)
    }
  })
  #---- Italy data ----
  italy_data <- reactive({
    git_pull_italy()
    list(
      total = read.csv("covid19Italy/csv/italy_total.csv"),
      region = read.csv("covid19Italy/csv/italy_region.csv"),
      province = read.csv("covid19Italy/csv/italy_province.csv")
    )
  })
  #---- Fun data ----
  fun_data <- reactive({
    list(
      toilet_paper = read.csv("toilet_paper.csv"),
      pasta = read.csv("pasta.csv")
    )
  })
  #---- CSSE data ----
  data_confirmed <- reactive({
    git_pull()
    per_country_data(read.csv("./COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
  })

  data_death <- reactive({
    git_pull()
    per_country_data(read.csv("./COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
  })
  data_recovered <- reactive({
    git_pull()
    generate_from_daily("./COVID-19/csse_covid_19_data/csse_covid_19_daily_reports")
  })
  
  all_data <- reactive({
    confirmed <- new_data_gen(data_confirmed(), data_confirmed()$Country.Region)
    deaths <- new_data_gen(data_death(), data_death()$Country.Region, FALSE) %>%
      rename(deaths = value)
    recovered <- data_recovered()
      # %>% replace_na(list("recovered" = max(recovered, na.rm = TRUE)))
    left_join(
      left_join(confirmed, deaths, by = c("country", "date")),
      recovered, by = c("country", "date")
    )
  })
 
  #---- last mod ----
  output$last_modified <- renderUI({
    git_pull()
    div(style = "font-size: 0.5em; text-align:center",
        tags$p("Data last modified:"),
        tags$p(file.info("COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")["mtime"][1,1]),
        tags$br(),
        HTML(
          "<p>&copy; <a target='_new' href='https://mail-wolf.de/?page_id=1292'>zappingseb</a></p>"
        )
    )
  })
  callModule(about, "about_module")
  callModule(timeline_charts, "timeline_charts_module", data_death = data_death, data_confirmed = data_confirmed, data_recovered = data_recovered)
  callModule(dt_table, "dt_table_module", all_data = all_data, population_data_short = population_data_short)
  callModule(italy, "italy_module", italy_data = italy_data)
  callModule(fun, "fun_module", fun_data = fun_data)
}

# Run the application 
shinyApp(ui = ui, server = server)
