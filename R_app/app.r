library(shiny)
library(leaflet) # devtools::install_github('rstudio/leaflet')
library(highcharter) # devtools::install_github('jbkunst/highcharter')
library(plotly) # devtools::install_github('ropensci/plotly')
library(ggplot2) # devtools::install_github('hadley/ggplot2')
library(sp)
library(dplyr)
library(rgeos)
library(mapproj)
library(maptools)
library(readr)
library(ggthemes)
library(RCurl)
library(shinydashboard)
library(rsconnect)

# Case data
numHeadCols = 4
confirmed <- read.csv(text=getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
confirmed$type <- 'confirmed'
confirmed$plotColor <- 'orange'

deaths <- read.csv(text=getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"))
deaths$type <- 'deaths'
deaths$plotColor <- 'red'

recovered <- read.csv(text=getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"))
recovered$plotColor <- 'green'
recovered$type <- 'recovered'

combined <- rbind(confirmed, deaths, recovered)


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                style="z-index:500;", # legend over my map (map z = 400)
                tags$h3("map"), 
                sliderInput("day", label = "Days since outbreak",
                            min = 1, max = ncol(combined)[1]-numHeadCols, value = ncol(combined)[1]-numHeadCols, step = 1,
                            animate = animationOptions(interval = 300, loop = FALSE)
                )
  )
)

server <- function(input, output, session) {
  
  # reactive filtering data from UI
  
  filteredData <- reactive({
    data.frame(
      'lat' = combined$Lat,
      'long' = combined$Long,
      'caseCnt'= combined %>% select(colnames(combined)[input$day + numHeadCols]) %>% pull(),
      'type' = combined$type,
      'plotColor' = combined$plotColor
    )
  })
  
  #static background map
  output$map <- renderLeaflet({
    leaflet(data.frame(
      'lat' = combined$Lat,
      'long' = combined$Long,
      'caseCnt'= combined %>% select(colnames(combined)[1]) %>% pull(),
      'type' = combined$type,
      'plotColor' = combined$plotColor
      )) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      addCircleMarkers(radius = ~3*(log10(as.numeric(caseCnt))),
                             stroke = FALSE,
                             fillOpacity = ~(caseCnt)*.5,
                             #fillOpacity = 0.3,
                             color = ~plotColor)
  })  
  
  # reactive circles map
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircleMarkers(data=filteredData(),
                       radius = ~2*(log10(as.numeric(caseCnt))),
                       stroke = FALSE,
                       fillOpacity = ~(caseCnt)*.5,
                       #fillOpacity = 0.5,
                       color = ~plotColor)
  })
}

shinyApp(ui, server)