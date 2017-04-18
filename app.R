rm(list = ls())
cat("\014")

library(shiny)
library(ggplot2)
library(ggvis)
library(dplyr)

pop <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2.csv", header = TRUE, stringsAsFactors = FALSE)
fert <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", header = TRUE, stringsAsFactors = FALSE, skip = 4)
life <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", header = TRUE, stringsAsFactors = FALSE, skip = 4)
meta <- read.csv("Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", stringsAsFactors = FALSE, header = TRUE)
pop <- pop[pop$Country.Name != "Not classified",]
fert <- fert[fert$Country.Name != "Not classified",]
life <- life[life$Country.Name != "Not classified",]
palette(c("#3466cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477"))


ui <- fluidPage(
  fluidRow(
    column(12, headerPanel("Fertility Rate vs. Life Expectancy"))
    ),
  fluidRow(
    column(10,
           ggvisOutput("ggvis_ui"),
           ggvisOutput("ggvis")),
    column(2, textOutput("txt"), checkboxGroupInput("checkGroup", label = h3("Region"), 
                                 choices = list("All" = "All",
                                                "East Asia & Pacific" = "East Asia & Pacific",
                                                "Europe & Central Asia" = "Europe & Central Asia",
                                                "Latin America & Caribbean" = "Latin America & Caribbean",
                                                "Middle East & North Africa" = "Middle East & North Africa",
                                                "North America" = "North America",
                                                "South Asia" = "South Asia",
                                                "Sub-Saharan Africa" = "Sub-Saharan Africa"),
                                 selected = "All"))
    ),
  fluidRow(
  column(10, "Year", sliderInput("years", NULL, width="100%", min = 1960, max = 2014, value = 1960, sep = "",
                        animate=animationOptions(interval=500), step=1, ticks=FALSE)),
  column(2, "Population Scaler", sliderInput("pop", NULL, min = 0, max = 1, value = 0.5, step = 0.01, ticks=FALSE))
  )
)
  

server <- function(input, output) {

  x <- reactive({
    life[paste0("X", input$years)]
  })
  
  y <- reactive({
    fert[paste0("X", input$years)]
  })
  
  z <- reactive({
    pop[paste0("X", input$years)]
  })
  
  checkregion <- reactive({
    input$checkGroup
  })

  tempdf <- reactive({
    tempdf <- data.frame(x(), y(), z(), meta[c("Region", "TableName")], 1:nrow(x()), rep(1, nrow(x())))
    names(tempdf) <- c("life", "fert", "pop", "Region", "TableName", "id", "opac")
    tempdf <- tempdf[(tempdf$Region != "" & !is.na(tempdf$life) & !is.na(tempdf$fert)),]
    
    if(is.null(checkregion())){
      tempdf$opac <- rep(1, nrow(tempdf))
    } else if(checkregion() == "All"){
      tempdf$opac <- rep(1, nrow(tempdf))
    } else {
      tempdf[!(tempdf$Region %in% checkregion()), "opac"] <- 0.1
    }
    tempdf
  })

  input_pop <- reactive(input$pop)
  input_check <- reactive(input$checkGroup)
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- tempdf()[tempdf()$id == x$id,]
    paste(row$TableName)
  }
  
  reactive({
  tempdf() %>%
    ggvis(x = ~life, y = ~fert, key := ~id) %>%
    add_tooltip(all_values, "hover") %>%
    layer_points(size := ~pop^((0.4*input$pop+1.5)*1.001 - 1.001)/10000 + 25, fill = ~factor(Region), stroke := "black", opacity := ~opac) %>%
    scale_numeric("x", domain = c(0, 100), nice = FALSE) %>%
    scale_numeric("y", domain = c(0, 10), nice = FALSE) %>%
    add_axis("x", title = "Life Expectancy") %>%
    add_axis("y", title = "Fertility Rate") %>%
    scale_ordinal("fill", range =c("#3466cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")) %>%
    set_options(width = "auto", height = "auto") %>%
    hide_legend("size") 
    }) %>% bind_shiny("ggvis", "ggvis_ui")
  
}

shinyApp(ui = ui, server = server)

