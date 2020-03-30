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
library(DT) # if (!require("DT")) install.packages('DT')

# Case data
numHeadCols = 4
numTailCols = 2
covid_alpha = 0.7
deaths_col = "#cc4c02"
#covid_col = "#D98D09"
covid_col = "#a20f0f"
recovered_col = "#70a801"

geocode <- read_csv('countries_csv.csv')

confirmed <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_19-covid-Confirmed_archived_0325.csv"))
#confirmed <- confirmed[,1:ncol(confirmed)-1]
confirmed$type <- 'confirmed'
confirmed$plotColor <- covid_col
grouped_by_country <- confirmed %>% group_by(`Country/Region`) %>% summarise_if(is.numeric, sum)
grouped_by_country <- data.frame("Country"= grouped_by_country$`Country/Region`, "Confirmed" = grouped_by_country[ncol(grouped_by_country)] %>% pull())
grouped_by_country[order(-grouped_by_country$Confirmed),]

deaths <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_19-covid-Deaths_archived_0325.csv"))
grouped_by_country_deaths <- deaths %>% group_by(`Country/Region`) %>% summarise_if(is.numeric, sum)
grouped_by_country_deaths <- data.frame("Country"= grouped_by_country_deaths$`Country/Region`, "Deaths" = grouped_by_country_deaths[ncol(grouped_by_country_deaths)] %>% pull())
#deaths <- deaths[,1:ncol(deaths)-1]
deaths$type <- 'deaths'
deaths$plotColor <- deaths_col

recovered <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_19-covid-Recovered_archived_0325.csv"))
#colnames(recovered) <- c("Province/State", "Country/Region", "Lat", "Long", "1/22/20", "1/23/20", "1/24/20", "1/25/20", "1/26/20", "1/27/20", "1/28/20", "1/29/20", "1/30/20", "1/31/20", "2/1/20", "2/2/20", "2/3/20", "2/4/20", "2/5/20", "2/6/20", "2/7/20", "2/8/20", "2/9/20", "2/10/20", "2/11/20", "2/12/20", "2/13/20", "2/14/20", "2/15/20", "2/16/20", "2/17/20", "2/18/20", "2/19/20", "2/20/20", "2/21/20", "2/22/20", "2/23/20", "2/24/20", "2/25/20", "2/26/20", "2/27/20", "2/28/20", "2/29/20", "3/1/20" ,"3/2/20", "3/3/20", "3/4/20", "3/5/20", "3/6/20", "3/7/20", "3/8/20", "3/9/20", "3/10/20", "3/11/20", "3/12/20", "3/13/20", "3/14/20", "3/15/20", "3/16/20", "3/17/20", "3/18/20", "3/19/20", "3/20/20", "3/21/20", "3/22/20", "3/23/20", "3/24/20")
recovered$type <- 'recovered'
recovered$plotColor <- recovered_col
combined <- rbind(confirmed, deaths, recovered)
#combined <- rbind(confirmed, deaths, recovered)

cases <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/archived_data/data/cases.csv"))

ui <- fluidPage(
  span(p("COVID-19 Tracker", align = "left", style="margin-bottom:2%"), style="font-weight:500;font-size:36px;text-align:center;color:black;"),
  fluidRow(tags$head(includeCSS("styles.css")),
           tags$head(tags$style(".leaflet-control-zoom { display: none; }")),
           tags$head(tags$style(".leaflet-control-attribution { display: none; }")),
      column(2,
             span(p("Total Confirmed", align = "center"), style="font-size:20px;text-align:center;color:black"),
             span(p(textOutput("reactive_case_count"), align = "center"), style="font-weight:500;font-size:36px;color:#e60000;text-align:center;"),
             DT::dataTableOutput('table'),
             #plotOutput("countPlot", height="430px", width="100%"),
             pickerInput("outcome_select", width="80%",
                         choices = c("Confirmed cases", "Deaths", "Recovered"), 
                         selected = c("Cases"),
                         multiple = FALSE),
             sliderInput("day", label = "Days since outbreak", min = 1, max = ncol(combined)[1]-(numHeadCols+numTailCols),
                         value = ncol(combined)[1]-(numHeadCols+numTailCols)-1, step = 1, width = "100%",
                         animate = animationOptions(interval = 700, loop = FALSE))
      ),
      column(6,
             leafletOutput("COVID19", height="750px")
      ),
      fluidRow(
        column(4,
          column(4,
                 span(p("Total Deaths", align = "center"), style="font-size:20px;text-align:center;color:black"),
                 span(p(textOutput("reactive_death_count"), align = "center"), style="font-weight:500;font-size:36px;text-align:center;color:black"),
                 tableOutput('deaths_table')),
          column(4,offset = 2,
                 span(p("Total Recovered", align = "center"), style="font-size:20px;text-align:center;color:black"),
                 span(p(textOutput("reactive_recovered_count"), align = "center"), style="font-weight:500;font-size:36px;text-align:center;color:#70a801"),
                 tableOutput('recovered_table'))
          ),
        column(4, span(p("Cumulative Global Confirmed Cases", align = "left", style="margin-left:15px;"), style="font-size:14px;text-align:center;color:black;margin-left:15px;"),
               span(plotOutput("epiCurve", height="300px", width="450px"), style="margin-left:15px;"))
      )
))

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
    init_map_df <- data.frame(
      'lat' = combined$Lat,
      'long' = combined$Long,
      'caseCnt'= combined %>% select(colnames(combined)[ncol(combined)[1]-2]) %>% pull(),
      'type' = combined$type,
      'plotColor' = combined$plotColor
    )
    leaflet(init_map_df) %>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE)) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% setView(0, 0, zoom = 2.1) %>%
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
  
  output$epiCurve <- renderPlot({
    ggplot(data = data.frame("date" = as.Date(colnames(confirmed[8:ncol(confirmed)-(numTailCols+1)]), format='%m/%d/%y'), 
                             "y" = data.frame(colSums(confirmed[8:ncol(confirmed)-(numTailCols+1)])) %>% pull() ), aes(x=date, y=y)) + 
      geom_line(colour="#ffaa00", size=1.0) +
      geom_point(colour="#ffaa00", size=3, shape=21, fill="#ffaa00") + labs(x = "", y = "") +
      theme_minimal() + theme(panel.grid.minor = element_blank(), 
                              panel.grid.major = element_blank(),
                              panel.background = element_blank(),
                              plot.background = element_blank()
      )}, bg="transparent")
  
  output$table <- DT::renderDataTable(
    DT::datatable(
      grouped_by_country,
      selection = 'single',
      options = list(
        order = list(2, 'desc'),
        searching = FALSE,
        lengthChange = FALSE,
        pageLength = 12),
      callback = JS("
            var format = function(d) {
              console.log(d)
            };
            table.on('click', 'td.details-control', function() {
             console.log()
            });")
  ))
  
  #output$table <- renderTable({
  #  grouped_by_country <- confirmed %>% group_by(`Country/Region`) %>% summarise_if(is.numeric, sum)
  #  grouped_by_country <- data.frame("Country"= grouped_by_country$`Country/Region`, "Confirmed" = grouped_by_country[ncol(grouped_by_country)] %>% pull())
  #  grouped_by_country[order(-grouped_by_country$Confirmed),]
  #})
  
  #output$deaths_table <- DT::renderDataTable(
  #  DT::datatable(
  #    grouped_by_country_deaths,
  #    options = list(
  #      order = list(2, 'desc'),
  #      searching = FALSE,
  #      lengthChange = FALSE,
  #      pageLength = 6,
  #      columnDefs = list(list(width = '10px'))),
  #    callback = JS("
  #          var format = function(d) {
  #            console.log(d)
  #          };
  #          table.on('click', 'td.details-control', function() {
  #           console.log()
  #          });")
  #  ))
  
  output$deaths_table <- renderTable({
    grouped_by_country <- deaths %>% group_by(`Country/Region`) %>% summarise_if(is.numeric, sum)
    grouped_by_country <- data.frame("Country"= grouped_by_country$`Country/Region`, "Deaths" = grouped_by_country[ncol(grouped_by_country)] %>% pull())
    grouped_by_country[order(-grouped_by_country$Deaths),]
  })
  
  output$recovered_table <- renderTable({
    grouped_by_country <- recovered %>% group_by(`Country/Region`) %>% summarise_if(is.numeric, sum)
    grouped_by_country <- data.frame("Country"= grouped_by_country$`Country/Region`, "Recovered" = grouped_by_country[ncol(grouped_by_country)] %>% pull())
    grouped_by_country[order(-grouped_by_country$Recovered),]
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
    s = input$table_rows_selected
    #print(s[1])
    tmp <- s[1]
    print(tmp)
    if (is.null(tmp)){
      tmp = 1
      cur_country_lat = 0
      cur_country_lon = 0
      zoom = 2.1
    } else {
      country_name <- grouped_by_country$Country[tmp]
      cur_country_lat <- geocode %>% filter(name==country_name) %>% select(latitude) %>% pull()
      cur_country_lon <- geocode %>% filter(name==country_name) %>% select(longitude) %>% pull()
      print(cur_country_lat)
      print(cur_country_lon)
      zoom = 5
    }
    content <- paste(sep = "<br/>",
                     "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
                     "606 5th Ave. S",
                     "Seattle, WA 98138"
    )
    leafletProxy("COVID19", data = filteredData()) %>%
      clearShapes() %>% clearMarkers() %>%
      addCircleMarkers(data=filteredData(),
                       radius = ~3*(log10(as.numeric(caseCnt))),
                       #stroke = ~plotColor,
                       stroke = FALSE,
                       fillOpacity = covid_alpha,
                       color = ~plotColor,
                       label = ~lapply(paste0("Total: ", as.list(caseCnt)), HTML))  %>% 
      setView(lng = cur_country_lon, lat = cur_country_lat, zoom = zoom)
  })
}

shinyApp(ui, server)