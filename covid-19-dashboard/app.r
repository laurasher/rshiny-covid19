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
library(stringr)

# Case data
numHeadCols = 4
numTailCols = 2
covid_alpha = 0.3
deaths_col = "#cc4c02"
#covid_col = "#D98D09"
covid_col = "#a20f0f"
recovered_col = "#70a801"

geocode <- read_csv('countries_csv.csv')

confirmed <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
latest_date <- colnames(confirmed)[length(colnames(confirmed))]
confirmed_timeseries <- confirmed
confirmed <- confirmed %>% select("Province/State", "Country/Region", "Lat", "Long", colnames(confirmed)[length(colnames(confirmed))]) %>%
  rename('latest_cases' = colnames(confirmed)[length(colnames(confirmed))]) %>% filter(`Country/Region`!='US')

confirmed_US <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
confirmed_US <- confirmed_US %>% rename('Province/State' = 'Combined_Key') %>% 
  rename('Country/Region' = 'Country_Region') %>% rename('Long' = 'Long_')  %>% 
  select(-iso2, -iso3, -UID, -code3, -FIPS, -Admin2, -Province_State) %>%
  filter(!is.na(Lat))
#confirmed_US$Province_State <- str_replace(confirmed_US$Province_State, "US", "")
confirmed_US <- confirmed_US %>% select("Province/State", "Country/Region", "Lat", "Long", colnames(confirmed_US)[length(colnames(confirmed_US))]) %>%
  rename('latest_cases' = colnames(confirmed_US)[length(colnames(confirmed_US))])
confirmed <- rbind(confirmed, confirmed_US)

confirmed$type <- 'confirmed'
confirmed$plotColor <- covid_col
confirmed_timeseries$type <- 'confirmed'
confirmed_timeseries$plotColor <- covid_col
grouped_by_country <- confirmed %>% group_by(`Country/Region`) %>% summarise_if(is.numeric, sum)
grouped_by_country <- data.frame("Country"= grouped_by_country$`Country/Region`, "Confirmed" = grouped_by_country[ncol(grouped_by_country)] %>% pull())
grouped_by_country[order(grouped_by_country$Country),]
rownames(grouped_by_country) <- grouped_by_country$Country

deaths <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
deaths <- deaths %>% select("Province/State", "Country/Region", "Lat", "Long", colnames(deaths)[length(colnames(deaths))]) %>%
  rename('latest_cases' = colnames(deaths)[length(colnames(deaths))])
grouped_by_country_deaths <- deaths %>% group_by(`Country/Region`) %>% summarise_if(is.numeric, sum)
grouped_by_country_deaths <- data.frame("Country"= grouped_by_country_deaths$`Country/Region`, "Deaths" = grouped_by_country_deaths[ncol(grouped_by_country_deaths)] %>% pull())
#deaths <- deaths[,1:ncol(deaths)-1]
deaths$type <- 'deaths'
deaths$plotColor <- deaths_col

recovered <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
recovered <- recovered %>% select("Province/State", "Country/Region", "Lat", "Long", colnames(recovered)[length(colnames(recovered))]) %>%
  rename('latest_cases' = colnames(recovered)[length(colnames(recovered))])
grouped_by_country_recovered <- recovered %>% group_by(`Country/Region`) %>% summarise_if(is.numeric, sum)
grouped_by_country_recovered <- data.frame("Country"= grouped_by_country_recovered$`Country/Region`, "Recovered" = grouped_by_country_recovered[ncol(grouped_by_country_recovered)] %>% pull())
recovered$type <- 'recovered'
recovered$plotColor <- recovered_col

combined <- rbind(confirmed, deaths, recovered)

ui <- fluidPage(
  span(p(paste0("COVID-19 Tracker ", latest_date), align = "left"), style="margin-bottom:2%;font-weight:500;font-size:36px;text-align:center;color:black;"),
  fluidRow(tags$head(includeCSS("styles.css")),
           tags$head(tags$style(".leaflet-control-zoom { display: none; }")),
           tags$head(tags$style(".leaflet-control-attribution { display: none; }")),
      column(2,
             span(p("Total Confirmed", align = "center"), style="font-size:20px;text-align:center;color:black"),
             span(p(textOutput("reactive_case_count"), align = "center"), style="font-weight:500;font-size:36px;color:#a20f0f;text-align:center;"),
             dataTableOutput('table'),
             pickerInput("outcome_select", width="80%",
                         choices = c("Confirmed cases", "Deaths", "Recovered"), 
                         selected = c("Cases"),
                         multiple = FALSE),
             sliderInput("day", label = "Days since outbreak", min = 1, max = ncol(combined)[1]-(numHeadCols+numTailCols),
                         value = ncol(combined)[1]-(numHeadCols+numTailCols)-1, step = 1, width = "100%",
                         animate = animationOptions(interval = 700, loop = FALSE))
      ),
      column(5,
             leafletOutput("COVID19", height="650px")
      ),
      fluidRow(
        column(4,
          column(4,
                 span(p("Total Deaths", align = "center"), style="font-size:20px;text-align:center;color:black"),
                 span(p(textOutput("reactive_death_count"), align = "center"), style="font-weight:500;font-size:36px;text-align:center;color:black"),
                 tableOutput('deaths_table')),
          column(4,offset = 3,
                 span(p("Total Recovered", align = "center"), style="font-size:20px;text-align:center;color:black"),
                 span(p(textOutput("reactive_recovered_count"), align = "center"), style="font-weight:500;font-size:36px;text-align:center;color:#70a801"),
                 tableOutput('recovered_table'))
          ),
        column(4, span(p("Cumulative Global Confirmed Cases", align = "left", style="margin-left:15px;"), style="font-size:14px;text-align:center;color:black;margin-left:15px;"),
               span(plotOutput("epiCurve", height="200px", width="450px"), style="margin-left:15px;"))
      )
))

server <- function(input, output, session) {
  
  # reactive filtering data from UI
  filteredData <- reactive({
    x = data.frame(
      'country' = combined$`Country/Region`,
      'province' = combined$`Province/State`,
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
    print(x)
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
    ggplot(data = data.frame("date" = as.Date(colnames(confirmed_timeseries[8:ncol(confirmed_timeseries)-(numTailCols+1)]), format='%m/%d/%y'), 
                             "y" = data.frame(colSums(confirmed_timeseries[8:ncol(confirmed_timeseries)-(numTailCols+1)])) %>% pull() ), aes(x=date, y=y)) + 
      geom_line(colour="#ffaa00", size=1.0) +
      geom_point(colour="#ffaa00", size=3, shape=21, fill="#ffaa00") + labs(x = "", y = "") +
      theme_minimal() + theme(panel.grid.minor = element_blank(), 
                              panel.grid.major = element_blank(),
                              panel.background = element_blank(),
                              plot.background = element_blank()
      )}, bg="transparent")
  
  output$table <- renderDataTable(
    datatable(
      grouped_by_country,
      selection = 'single',
      rownames=FALSE,
      options = list(
        searching = FALSE,
        lengthChange = FALSE,
        pageLength = 12)))
  
  output$deaths_table <- renderTable({
    grouped_by_country <- deaths %>% group_by(`Country/Region`) %>% summarise_if(is.numeric, sum)
    grouped_by_country <- data.frame("Country"= grouped_by_country$`Country/Region`, "Deaths" = grouped_by_country[ncol(grouped_by_country)] %>% pull())
    grouped_by_country[order(grouped_by_country$Country),]
  })
  
  output$recovered_table <- renderTable({
    grouped_by_country <- recovered %>% group_by(`Country/Region`) %>% summarise_if(is.numeric, sum)
    grouped_by_country <- data.frame("Country"= grouped_by_country$`Country/Region`, "Recovered" = grouped_by_country[ncol(grouped_by_country)] %>% pull())
    grouped_by_country[order(grouped_by_country$Country),]
  })
  
  #Reactive total case count
  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(combined %>% filter(type=='confirmed') %>% select(colnames(combined)[input$day + numHeadCols]) %>% pull()), big.mark=","))
  })
  output$reactive_death_count <- renderText({
    paste0(prettyNum(sum(grouped_by_country_deaths %>% select(Deaths) %>% pull()), big.mark=","))
  })
  output$reactive_recovered_count <- renderText({
    paste0(prettyNum(sum(grouped_by_country_recovered %>% select(Recovered) %>% pull()), big.mark=","))
  })
  output$reactive_date <- renderText({
    paste0(prettyNum(colnames(combined)[input$day + numHeadCols]))
  })
  # reactive circles map
  observe({
    s = input$table_rows_selected
    tmp <- s[1]
    if (is.null(tmp)){
      tmp = 1
      cur_country_lat = 0
      cur_country_lon = 0
      zoom = 2.1
    } else {
      country_name <- grouped_by_country$Country[tmp]
      cur_country_lat <- geocode %>% filter(name==country_name | country==country_name) %>% select(latitude) %>% pull()
      cur_country_lon <- geocode %>% filter(name==country_name | country==country_name) %>% select(longitude) %>% pull()
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
                       label = ~lapply(paste0(province, " ", country, ": ", as.list(caseCnt)), HTML))  %>% 
      setView(lng = cur_country_lon, lat = cur_country_lat, zoom = zoom)
  })
}

shinyApp(ui, server)