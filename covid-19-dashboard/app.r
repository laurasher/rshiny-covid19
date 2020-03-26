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
covid_col = "#D98D09"
recovered_col = "#0A5E2F"

confirmed <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
confirmed$type <- 'confirmed'
confirmed$plotColor <- covid_col

deaths <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
deaths$type <- 'deaths'
deaths$plotColor <- deaths_col

recovered <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
recovered$type <- 'recovered'
recovered$plotColor <- recovered_col

combined <- rbind(confirmed, deaths, recovered)
#combined <- rbind(confirmed, deaths)
#combined <- rbind(confirmed)


ui <- navbarPage(theme = shinytheme("yeti"), collapsible = TRUE,
                 "COVID-19 Pandemic", id="nav",
                 tabPanel("Map",
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              tags$head(tags$style(".leaflet-control-zoom { display: none; }")),
                              leafletOutput("COVID19", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            bottom = 60, left = 20, width = 450, fixed=TRUE,
                                            draggable = TRUE, height = "85%",
                                            
                                            span(h3(textOutput("reactive_case_count"), align = "right"), style="color:#D98D09"),
                                            span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#0A5E2F"),
                                            span(h4(textOutput("reactive_death_count"), align = "right"), style="color:#cc4c02"),
                                            span(h4(textOutput("reactive_date"), align = "right"), style="color:black"),
                                            #tags$i(h6("Updated once daily. For more regular updates, refer to: ", tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins COVID-19 dashboard"))),
                                            plotOutput("countPlot", height="430px", width="100%"),
                                            #plotOutput("cumulative_plot", height="130px", width="100%"),
                                            pickerInput("outcome_select", width="60%",
                                                        choices = c("Confirmed cases", "Deaths", "Recovered"), 
                                                        selected = c("Cases"),
                                                        multiple = FALSE),
                                            sliderInput("day", label = "Days since outbreak", min = 1, max = ncol(combined)[1]-(numHeadCols+numTailCols),
                                                        value = ncol(combined)[1]-(numHeadCols+numTailCols)-1, step = 1, width = "100%",
                                                        animate = animationOptions(interval = 700, loop = FALSE))
                              ))
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
      addProviderTiles(providers$Stamen.TonerLite) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
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
    paste0(prettyNum(sum(combined %>% filter(type=='confirmed') %>% select(colnames(combined)[input$day + numHeadCols]) %>% pull()), big.mark=","), " cases")
  })
  output$reactive_death_count <- renderText({
    paste0(prettyNum(sum(combined %>% filter(type=='deaths') %>% select(colnames(combined)[input$day + numHeadCols] ) %>% pull()), big.mark=","), " deaths")
  })
  output$reactive_recovered_count <- renderText({
    paste0(prettyNum(sum(combined %>% filter(type=='recovered') %>% select(colnames(combined)[input$day + numHeadCols] ) %>% pull()), big.mark=","), " recovered")
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