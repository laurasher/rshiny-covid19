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
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")


# Case data
numHeadCols = 4
numTailCols = 2
covid_alpha = 0.5
deaths_col = "#cc4c02"
#covid_col = "#D98D09"
covid_col = "#a20f0f"
recovered_col = "#70a801"

confirmed <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
confirmed <- confirmed[,1:ncol(confirmed)-1]
confirmed$type <- 'confirmed'
confirmed$plotColor <- covid_col

deaths <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
deaths <- deaths[,1:ncol(deaths)-1]
deaths$type <- 'deaths'
deaths$plotColor <- deaths_col

recovered <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
colnames(recovered) <- c("Province/State", "Country/Region", "Lat", "Long", "1/22/20", "1/23/20", "1/24/20", "1/25/20", "1/26/20", "1/27/20", "1/28/20", "1/29/20", "1/30/20", "1/31/20", "2/1/20", "2/2/20", "2/3/20", "2/4/20", "2/5/20", "2/6/20", "2/7/20", "2/8/20", "2/9/20", "2/10/20", "2/11/20", "2/12/20", "2/13/20", "2/14/20", "2/15/20", "2/16/20", "2/17/20", "2/18/20", "2/19/20", "2/20/20", "2/21/20", "2/22/20", "2/23/20", "2/24/20", "2/25/20", "2/26/20", "2/27/20", "2/28/20", "2/29/20", "3/1/20" ,"3/2/20", "3/3/20", "3/4/20", "3/5/20", "3/6/20", "3/7/20", "3/8/20", "3/9/20", "3/10/20", "3/11/20", "3/12/20", "3/13/20", "3/14/20", "3/15/20", "3/16/20", "3/17/20", "3/18/20", "3/19/20", "3/20/20", "3/21/20", "3/22/20", "3/23/20", "3/24/20")
recovered$type <- 'recovered'
recovered$plotColor <- recovered_col
combined <- rbind(confirmed, deaths, recovered)
#combined <- rbind(confirmed, deaths, recovered)

cases <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/archived_data/data/cases.csv"))

ui <- fluidPage(theme = shinytheme("slate"),
  fluidRow(tags$head(includeCSS("styles.css")),
           tags$head(tags$style(".leaflet-control-zoom { display: none; }")),
           tags$head(tags$style(".leaflet-control-attribution { display: none; }")),
    column(2,
           span(h3("Total Confirmed", align = "center"), style="color:white"),
           span(h1(textOutput("reactive_case_count"), align = "center"), style="color:#e60000"),
           #plotOutput("countPlot", height="430px", width="100%"),
           pickerInput("outcome_select", width="60%",
                       choices = c("Confirmed cases", "Deaths", "Recovered"), 
                       selected = c("Cases"),
                       multiple = FALSE),
           sliderInput("day", label = "Days since outbreak", min = 1, max = ncol(combined)[1]-(numHeadCols+numTailCols),
                       value = ncol(combined)[1]-(numHeadCols+numTailCols)-1, step = 1, width = "100%",
                       animate = animationOptions(interval = 700, loop = FALSE))
    ),
    column(6,
           leafletOutput("COVID19", height="800px")
    ),
    column(2,
           span(h3("Total Deaths", align = "center"), style="color:white"),
           span(h1(textOutput("reactive_death_count"), align = "center"), style="color:white")),
    column(2,
           span(h3("Total Recovered", align = "center"), style="color:white"),
           span(h1(textOutput("reactive_recovered_count"), align = "center"), style="color:#70a801"))
  )
)

server <- function(input, output, session) {
  
  # reactive filtering data from UI
  filteredData <- reactive({
    x = data.frame(
      'country' = combined$`Country/Region`,
      'lat' = combined$Lat,
      'long' = combined$Long,
      'caseCnt'= combined %>% select(colnames(combined)[input$day + numHeadCols]) %>% pull(),
      'type' = combined$type,
      'plotColor' = combined$plotColor
    )
    if (input$outcome_select=="Confirmed cases") { 
      x <- x  %>%  filter(type == 'confirmed')
    }
    if (input$outcome_select=="Deaths") { 
      x <- x %>% filter(type == 'deaths')
    }
    if (input$outcome_select=="Recovered") { 
      x <- x %>% filter(type == 'recovered')
    }
    
    x <- x %>% filter(caseCnt > 0)
  })
  
  #static background map
  output$COVID19 <- renderLeaflet({
    leaflet(data.frame(
      'lat' = combined$Lat,
      'long' = combined$Long,
      'caseCnt'= combined %>% select(colnames(combined)[ncol(combined)[1]-2]) %>% pull(),
      'type' = combined$type,
      'plotColor' = combined$plotColor
      )) %>%
      addProviderTiles(providers$CartoDB.DarkMatter, options = providerTileOptions(noWrap = TRUE)) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% setView(0, 0, zoom = 2) %>%
      addCircleMarkers(radius = ~3*(log10(as.numeric(caseCnt))),
                             stroke = FALSE,
                             #fillOpacity = ~(caseCnt)*.5,
                             fillOpacity = covid_alpha,
                             color = ~plotColor)
  })

  output$countPlot <- renderPlot({
    grouped_combined <- filteredData()  %>% group_by(country) %>% summarise_if(is.numeric, sum)
    ggplot(data.frame(
      'country' = grouped_combined$country,
      'caseCnt' = grouped_combined %>% select(colnames(grouped_combined)[length(colnames(grouped_combined))]) %>% pull()
    ),aes_string(x='country', y='caseCnt')) + 
      geom_bar(stat = "identity") + theme(axis.text.x=element_blank()) + theme_minimal() + coord_flip() + scale_y_log10() + labs(x = "", y = "")
  })
  
  #Reactive total case count
  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(combined %>% filter(type=='confirmed') %>% select(colnames(combined)[input$day + numHeadCols]) %>% pull()), big.mark=","))
  })
  output$reactive_death_count <- renderText({
    paste0(prettyNum(sum(combined %>% filter(type=='deaths') %>% select(colnames(combined)[input$day + numHeadCols] ) %>% pull()), big.mark=","))
  })
  output$reactive_recovered_count <- renderText({
    paste0(prettyNum(sum(combined %>% filter(type=='recovered') %>% select(colnames(combined)[input$day + numHeadCols] ) %>% pull()), big.mark=","))
  })
  output$reactive_date <- renderText({
    paste0(prettyNum(colnames(combined)[input$day + numHeadCols]))
  })
  # reactive circles map
  observe({
    leafletProxy("COVID19", data = filteredData()) %>%
      clearShapes() %>% clearMarkers() %>%
      addCircleMarkers(data=filteredData(),
                       radius = ~3*(log10(as.numeric(caseCnt))),
                       #stroke = ~plotColor,
                       stroke = FALSE,
                       fillOpacity = covid_alpha,
                       color = ~plotColor)
  })
}

shinyApp(ui, server)