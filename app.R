library(bslib)
library(shiny)
library(plotly)
library(thematic)
library(tidyverse, warn.conflicts = FALSE)

od_data <- readxl::read_excel("OD_Measurements.xlsx") %>%
  pivot_longer(!Wavelength, names_to = "Lens", values_to = "Level of Protection (OD rating)")

thematic_shiny(font = "auto")
# Define UI for application that draws a histogram
ui <- page_fluid(
theme= bs_theme(version =5,
                bg = "white",
                fg = "black",
                primary = "#f70202",
                base_font = font_google("Karla")),
card(
      card_header(strong("Search IO's Optical Density Scans"),
               layout_column_wrap(width = 5,
               numericInput("wl1", "Wavelength 1", value = 755, step = 5),
               numericInput("wl2", "Wavelength 2", value = NULL, step = 5),
               numericInput("wl3", "Wavelength 3", value = NULL, step = 5),
               numericInput("wl4", "Wavelength 4", value = NULL, step = 5),
               numericInput("wl5", "Wavelength 5", value = NULL, step = 5)),
               layout_column_wrap(width = 5,
               numericInput("od1", "Optical Density (OD) 1", value = 7, step = 0.5, min = 0, max = 10),
               numericInput("od2", "Optical Density (OD) 2", value = NULL, step = 0.5, min = 0, max = 10),
               numericInput("od3", "Optical Density (OD) 3", value = NULL, step = 0.5, min = 0, max = 10),
               numericInput("od4", "Optical Density (OD) 4", value = NULL, step = 0.5, min = 0, max = 10),
               numericInput("od5", "Optical Density (OD) 5", value = NULL, step = 0.5, min = 0, max = 10)),
               br(),
               actionButton("search_od", "Search"),
               strong("Note: Search must be pressed in order to generate new results.")),
               card_body(layout_column_wrap(width =2,
                 plotlyOutput("plot")
                 ,tableOutput("table"))
               )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  od_new <- eventReactive(input$search_od,{
    od_a <- od_data %>%
      filter(Wavelength==input$wl1,
             `Level of Protection (OD rating)` >= input$od1)
    od_b <- od_data %>%
      filter(Wavelength==input$wl2,
             `Level of Protection (OD rating)` >= input$od2)
    od_c <- od_data %>%
      filter(Wavelength==input$wl3,
             `Level of Protection (OD rating)` >= input$od3)
    od_d <- od_data %>%
      filter(Wavelength==input$wl4,
             `Level of Protection (OD rating)` >= input$od4)
    od_e <- od_data %>% 
      filter(Wavelength==input$wl5,
             `Level of Protection (OD rating)` >= input$od5)
    od_new <- full_join(od_a,od_b)
    od_new <- full_join(od_new, od_c)
    od_new <- full_join(od_new, od_d)
    od_new <- full_join(od_new, od_e) %>% 
      mutate("Level of Protection (OD rating)" = if_else(`Level of Protection (OD rating)` >= 10, 10, `Level of Protection (OD rating)`))
  })
  
  # Filter and Plot
  output$plot <- renderPlotly({
    
    od_plot <- ggplot(od_new(), aes(Wavelength, `Level of Protection (OD rating)`, color = Lens))+
      geom_line()+
      geom_point(size = 2.5)+
      scale_y_continuous(limits = c(floor(min(od_new()$`Level of Protection (OD rating)`)), 10),
                         breaks = seq(floor(min(od_new()$`Level of Protection (OD rating)`)), 10, 1))+
      scale_x_continuous(limits = c(min(od_new()$Wavelength), max(od_new()$Wavelength)),
                         breaks = od_new()$Wavelength)
    id <- showNotification(
      "Generating graph...", 
      duration = NULL, 
      closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)
    
    ggplotly(od_plot)
  })
  output$table <- renderTable({
    od_new() %>% 
      pivot_wider(names_from = Wavelength,
                  values_from = c(`Level of Protection (OD rating)`),
                  names_glue = "OD @ {Wavelength}") %>% 
      arrange(pick(starts_with("OD")))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
