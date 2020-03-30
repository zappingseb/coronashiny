mapUI <- function(id){
  ns <- NS(id)
  tagList(
    tags$script('$("#DataTables_Table_0").show()'),
    material_row(
      material_card(
        title = "Overview map",
        tags$p("Please note: On 2019-03-23 the level of details changed for the USA. The explosion in cases you will see is due to changes
               in geographical details in ", tags$a(href = "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data", "CSSE Date", target="_new"), " not to a real explosion of cases."),
        uiOutput(ns("slider"))
      )
    ),
    material_row(
      material_column(width=12,
                      leafletOutput(outputId = ns("outmap"))
      )
    )
  )
}

map <- function(input, output, session, all_dates = NULL, map_data = NULL) {
  
  session$userData$showEx <- reactiveVal(TRUE)
  
  output$slider <- shiny::renderUI({
    div(style="width:85%;padding-left:7%;padding-right:7%",
      shiny::sliderInput(inputId = session$ns("datum"), min = as.POSIXct("2020-02-01"), max = max(all_dates()), value = max(all_dates()),
                         label = "Date", timeFormat="%Y-%m-%d", width = "100%", step = 1)
    )
  })
  
  output$outmap <- renderLeaflet({
    material_spinner_show(session, session$ns("outmap"))
    full_data <- map_data()
    
    date_to_choose <- if (is.null(input$datum)) {
      as.character(Sys.Date() - 1)
    } else {
      as.character(input$datum)
    }
    
    only_numeric <- sort(as.numeric(unique(full_data$active)))
    
    max_val <- max(only_numeric, na.rm = T)
    
    data_for_display <- full_data %>%
      filter(date == as.character(date_to_choose)) %>%
      select(Lat, Long, active, date, Combined_Key) %>%
      filter(active > 0) %>%
      filter(!is.na(Long) & !is.na(Lat))
    mapout <- leaflet(data_for_display) %>% addProviderTiles(providers$ CartoDB.Positron) %>%
      setView(0,0, 1) %>%
      addHeatmap(lng = ~Long, lat = ~Lat, intensity = ~active,
                 minOpacity = 0.3,
                 blur = 1, max = max_val, radius = 3, cellSize = 0.05,
                 gradient = viridisLite::viridis(60)
      )
    material_spinner_hide(session, session$ns("outmap"))
    return(mapout)
  })
}