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
source("data_gen.R")


default_countries <- c("Switzerland", "Korea, South", "Italy")

population_data <- read.csv("./population-figures-by-country-csv_csv.csv")
population_data_short <- rbind(
  population_data %>% select(Country, Country_Code, Year_2016) %>%
    mutate(Country = str_replace(Country, "Korea, Rep.", "Korea, South")) %>%
    mutate(Country = str_replace(Country, "Czech Republic", "Czechia")) %>%
    mutate(Country = str_replace(Country, "Russian Federation", "Russia")) %>%
    mutate(Country = str_replace(Country, "United States", "US")) %>%
    mutate(Country = str_replace(Country, "Iran, Islamic Rep.", "Iran")),
  data.frame(Country = "Taiwan", Country_Code = "TAI", Year_2016 = 23780000)
)


# Define UI for application that draws a histogram
ui <- material_page(
  nav_bar_color = "green",
  
  title = "COVID-19 data",
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(type = "text/javascript", src = "jquery_browser.js")
  ),
  
  material_side_nav(
    fixed = TRUE, 
    background_color = "white",
    image_source = "img/material.png",
    # Place side-nav tabs within side-nav
    material_side_nav_tabs(
      side_nav_tabs = c(
        "Country Charts" = "housing_prices",
        "All Countries Table" = "all_countries",
        "About" = "about"
      ),
      icons = c("insert_chart", "format_list_bulleted", "info_outline")
    )
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "housing_prices",
    
        material_row(
          material_column(
            width = 12,
            uiOutput("selector")
          )
        ),
        material_row(
          material_card(title = "Confirmed Cases:",
                        tagList(
                          p("The graph below shows the aggregated number of confirmed cases for the countries selected.
                            You can limit to a specific country by double-clicking its name inside the legend."),
                          p("The confirmed cases do contain cases that were already recovered.")
                        )
          )
        ),
        material_row(
          material_column(
            width = 12,
            plotlyOutput(outputId = "distPlot", width = "100%")
          )
        ),
        material_row(
          material_card(title = "Doubling days:",
                        tagList(
                          p("For each day the number of days it would take to double the number of confirmed cases is shown."),
                          p("This means, the higher the number, the slower people get infected."),
                          p("The doubling time is calculated by estimating exponential growth over at least the last three days.")
                        )
          )
        ),
        material_row(
          material_column(
            width = 12,
            plotlyOutput(outputId = "DoublingDays", width = "100%")
          )
        ),
        material_row(
          material_card(title = "Growth rate:",
                        tagList(
                          p("The growth rate shows how many people get infected compared to the day before. A growth rate > 1
                            shows exponential growth. If the growth rate goes below 1 the growth should be over the tipping
                            point for a logistic curve."),
                          p("Please watch ", a(href="https://www.youtube.com/watch?v=Kas0tIxDvrg", "this video"), "for further explanation.")
                        )
          )
        ),
        material_row(
              material_column(
                width = 12,
                plotlyOutput(outputId = "grouwthFactor", width = "100%")
              )
        )
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "all_countries",
        material_row(
          material_card(
            title = "Overview table",
            p("This table lists all countries that showed exponential growth (doubling of infections within 6.5 days or less)
              for at least one day. The columns show the following:"),
            tags$ul(
              tags$li(tags$b("Country:"), "name of the country"),
              tags$li(tags$b("Maximum time of exponential growth in a row:"), "The number of days a country showed exponential growth
                      (doubling of infections in short time) in a row. This means there was no phase of slow growth or decrease in between."),
              tags$li(tags$b("Days to double infections:"), "This gives the time it took until today to double the number of infections. A
                      higher number is better, because it takes longer to infect more people"),
              tags$li(tags$b("Exponential growth today:"), "Whether the countries number of infections is still exponentially growing"),
              tags$li(tags$b("Confirmed cases:"), "Confirmed cases today due to the Johns Hopkins CSSE data set"),
              tags$li(tags$b("Population:"), "Number of people living inside the country"),
              tags$li(tags$b("Confirmed cases on 100,000 inhabitants:"), "How many people have been infected if you would randomely choose
                      100,000 people from this country.")
            )
          )
        ),
        material_row(
          material_column(width=12,
                          DT::dataTableOutput("MaxDoublingTime")
          )
        )
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "about",
    material_row(
      material_column(width = 6,
        h2("The data"),
      img(src="./img/enterprise-medicine.logo.small.horizontal.white.581be190.png", width=120, style="background-color:#002d72;padding: 0.8em;
"),
        p("All data is taken from Johns Hopkins CSSE dataset on github"),
        a(href="https://github.com/CSSEGISandData/COVID-19", "Johns Hopkins Github Repository")
      ),
      material_column(width = 6,
        h2("The author"),
        img(src="./img/zappingseb.jfif", width=60),
        p(a(href="https://www.mail-wolf.de", "Sebastian Engel-Wolf"), " is a freelance scientific software developer developing R-shiny apps in
          a pharmaceutical environment"),
        p(a(href="https://github.com/zappingseb/coronashiny", "All code for this project"), "can be found on github")
      )
    )
  )
 
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  data_confirmed <- reactive({
    
    material_spinner_show(session, output_id = "housing_prices") 
    if (dir.exists("COVID-19")) {
      setwd("COVID-19")
      system("git pull")
      setwd("..")
    } else {
      
      system("git clone https://github.com/CSSEGISandData/COVID-19.git", timeout = 1000)
    }
    
    per_country_data(read.csv("./COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
  })
  
  output$selector = renderUI({
    tagList(
      selectInput(inputId = 'countries', 'Select Countries you want to add:', sort(unique(data_confirmed()$Country.Region)),
                  selected = default_countries, multiple = TRUE),
      div(style="clear:both;height:20px;")
    )
  })
  
  plot_data <- reactive({
    if (!is.null(input$countries)) {
      
      new_data_gen(data_confirmed(), input$countries)
    } else {
      new_data_gen(data_confirmed(), default_countries)
    }
  })
  
  all_data <- reactive({
    new_data_gen(data_confirmed(), data_confirmed()$Country.Region)
  })
  
  plotly_group <- reactive({
    
    plot_data_intern2 <- plot_data()
    # generate bins based on input$bins from ui.R
    
    material_spinner_hide(session, output_id = "housing_prices")
    plot_ly(
      data = plot_data_intern2,
      hoverinfo = "",
      type = "scatter",
      transforms = list(
        list(
          type = 'groupby',
          groups = plot_data_intern2$country,
          styles = lapply(seq_along(unique(plot_data_intern2$country)), function(x){
            palette_col <- viridisLite::viridis(n = length(unique(plot_data_intern2$country)))
            list(target = unique(plot_data_intern2$country)[x], value = list(line = list(color = palette_col[x]), 
                                                                             marker = list(color = alpha(palette_col[x], 0.6))))
          })
        )
      )
    )
  })
  
  output$distPlot <- renderPlotly({
      
    plotly_group() %>%
        add_trace(
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
            range = c(0, max(as.numeric(plot_data()$value), na.rm = TRUE) + 1)
          )
        )
    
  })
  output$grouwthFactor <- renderPlotly({
      
      # generate bins based on input$bins from ui.R
      plotly_group() %>%
        add_trace(
          type = "scatter",
          mode = "lines",
          x = ~date,
          y = ~growth.factor,
          name = "Linear growth Rate over 3 days",
          line = list(shape = "spline")
        ) %>%
        layout(
          xaxis = list(
            title = "Date"
          ),
          yaxis = list(
            title = "Growth Rate",
            range = c(0, max(as.numeric(plot_data()$growth.factor), na.rm = T) + 0.5)
          )
        )
    
  })
  
  output$DoublingDays <- renderPlotly({
      
    plotly_group() %>%
        add_trace(x = ~date,
                        y = ~doubling_days,
                        type = 'bar',
                        name = 'Days to double') %>%
        layout(
          xaxis = list(
            title = "Date"
          ),
          yaxis = list(
            type = "log",
            title = "Doubling Days",
            range = c(0, max(as.numeric(plot_data()$doubling_days), na.rm = T) + 0.5)
          )
        )
    
  })
  
  output$MaxDoublingTime <- DT::renderDataTable({
    
    
    df <- key_factors(all_data(), population_data_short)
    
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
                "Population (in Mio)",
                "Confirmed Cases on 100,000 inhabitants"
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
                  backgroundColor = styleEqual(c("no", "yes"), c("rgb(249,249,249)", "rgb(127,0,0)"))
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
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
