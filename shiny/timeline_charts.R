timeline_chartsUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    material_row(
      material_column(
        width = 12,
        uiOutput(ns("selector"))
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
        plotlyOutput(outputId = ns("distPlot"), width = "100%")
      )
    ),
    material_row(
      material_card(title = "Active cases:",
                    tagList(
                      p("This shows the patients that are still in hospitals or in isolation due to the disease.")
                    )
      )
    ),
    material_row(
      material_column(
        width = 12,
        plotlyOutput(outputId = ns("active"), width = "100%")
      )
    ),
    material_row(
      material_card(title = "Active per running day:",
                    tagList(
                      p("After having 200 patients a country starts to appear in this chart. After this day at each day the active patients
                        are counted.")
                    )
      )
    ),
    material_row(
      material_column(
        width = 12,
        plotlyOutput(outputId = ns("running"), width = "100%")
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
        plotlyOutput(outputId = ns("DoublingDays"), width = "100%")
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
        plotlyOutput(outputId = ns("grouwthFactor"), width = "100%")
      )
    ),
    material_row(
      material_card(title = "Mortality:",
                    tagList(
                      p("This shows the percentage of lethal cases per country over time."),
                      p(tags$em("Hovering shows total death under the braces."))
                    )
      )
    ),
    material_row(
      material_column(
        width = 12,
        plotlyOutput(outputId = ns("mortality"), width = "100%")
      )
    ),
    material_row(
      material_card(title = "Recovery:",
                    tagList(
                      p("This shows the patients that are recovered from the disease.")
                    )
      )
    ),
    material_row(
      material_column(
        width = 12,
        plotlyOutput(outputId = ns("recovery"), width = "100%")
      )
    )
  )
}

timeline_charts <- function(input, output, session, data_death = NULL, data_confirmed = NULL, data_recovered = NULL, map_data = NULL) {
  
  default_countries <- c("Switzerland", "Korea, South", "Italy", "China (only Hubei)", "US")
  
  output$selector = renderUI({
    tagList(
      selectInput(inputId = session$ns('countries'),
                  'Select Countries you want to add:', sort(unique(data_confirmed()$Country.Region)),
                  selected = default_countries, multiple = TRUE),
      div(style="clear:both;height:20px;")
    )
  })
  
  # ---- data plot ----
  plot_data <- reactive({
    if (!is.null(input$countries)) {
      confirmed_cases <- new_data_gen(data_confirmed(), input$countries)
      deaths <- new_data_gen(data_death(), input$countries, FALSE) %>% rename(deaths = value)
      recovered <- data_recovered() %>% filter(country %in% input$countries)
    } else {
      confirmed_cases <- new_data_gen(data_confirmed(), default_countries)
      deaths <- new_data_gen(data_death(), default_countries, FALSE) %>% rename(deaths = value)
      recovered <- data_recovered() %>% filter(country %in% default_countries)
    }
    
    merged_data <- left_join(
      left_join(confirmed_cases, deaths, by = c("country", "date")),
      recovered, by = c("country", "date")
    ) %>% add_mortality
    return(merged_data)
  })
  # ---- data running ----
  running_day_data <- reactive({
    
    country_names <- if(is.null(input$countries)){
      default_countries
    } else {
      input$countries
    }
    plot_data()  %>%
      mutate(
        date_greater_200 = case_when(
          as.numeric(value) > 200 ~ 1,
          TRUE ~ 0
        )
      ) %>%
      filter(date_greater_200 == 1) %>%
      group_by(country) %>%
      mutate(running_day = row_number()) %>%
      ungroup()
  })
  
  
  # ---- plotLyGroup ----
  plotly_group <- reactive({
    
    plot_data_intern2 <- plot_data()
    # generate bins based on input$bins from ui.R
    
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
  # ---- confirmed ----
  output$distPlot <- renderPlotly({
    
    plt_out <- plotly_group() %>%
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
    material_spinner_hide(session, output_id = "distPlot")
    return(plt_out)
  })
  # ---- growthFactor ----
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
  # ---- Brachart Doubling ----
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
  # ---- mortality ----
  output$mortality <- renderPlotly({
    
    plotly_group() %>%
      add_trace(x = ~date,
                y = ~as.numeric(mortality),
                type = 'scatter',
                text=~deaths,
                mode = "lines") %>%
      layout(
        xaxis = list(
          title = "Date"
        ),
        yaxis = list(
          title = "mortality (%)",
          range = c(0, max(plot_data()$mortality, na.rm=TRUE) + 0.3)
        )
      )
    
  })
  # ---- active ----
  output$active <- renderPlotly({
    plotly_group() %>%
      add_trace(x = ~date,
                y = ~as.numeric(active),
                type = 'scatter',
                text="",
                mode = "lines") %>%
      layout(
        xaxis = list(
          title = "Date"
        ),
        yaxis = list(
          title = "Active (Total cases)",
          range = c(0, max(as.numeric(plot_data()$active), na.rm=TRUE) + 0.3)
        )
      )
    
  })
  # ---- recovery ----
  output$recovery <- renderPlotly({
    plotly_group() %>%
      add_trace(x = ~date,
                y = ~as.numeric(recovered),
                type = 'scatter',
                text="",
                mode = "lines") %>%
      layout(
        xaxis = list(
          title = "Date"
        ),
        yaxis = list(
          title = "recovered (Total cases)",
          range = c(0, max(as.numeric(plot_data()$recovered), na.rm=TRUE) + 0.3)
        )
      )
    
  })
  
  # ---- running ----
  output$running <- renderPlotly({
    
    plot_data_intern2 <- running_day_data()
    
    data_for_country <- spread(plot_data_intern2, key = country, value = active)
    palette_col <- viridisLite::viridis(n = length(unique(plot_data_intern2$country)))
    
    plot_first <- plot_ly(
      data = plot_data_intern2,
      hoverinfo = "",
      type = "scatter",
      mode = "lines"
    ) 
    
    for (country_name in unique(plot_data_intern2$country)) {
      simple_data <- data_for_country[, c("running_day", country_name)]
      simple_data <- simple_data[!is.na(simple_data[, country_name]), ]
      names(simple_data)[which(names(simple_data) == country_name)] <- "active"
      plot_first <- plot_first %>% add_trace(
        data = simple_data,
        x = ~as.numeric(running_day),
        y = ~as.numeric(active),
        name = country_name,
        type = 'scatter',
        text="",
        mode = "lines",
        line = list(color = palette_col[which(unique(plot_data_intern2$country) == country_name)])
      )
      
    }
    
    plot_first %>%
      layout(
        xaxis = list(
          title = "Running Day"
        ),
        yaxis = list(
          title = "Active (Total cases)",
          range = c(0, max(as.numeric(plot_data_intern2$value), na.rm=TRUE) + 0.3)
        )
      )
    
  })
}