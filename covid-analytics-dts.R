
# Load packages -----------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(httr)
library(tidyverse)
library(lubridate)
library(dygraphs)
library(xts)
library(prophet)
library(vroom)
library(curl)
library(plotly)
library(sf)

# Configuration -----------------------------------------------------------

# Options for Spinner
options(spinner.color="#3C8DBC", spinner.color.background="#ffffff", spinner.size=1)

# Data Preparation - Cases Historical -------------------------------------

province_metadata <- vroom("data/province_metadata.csv", col_types = "cc")

resp <- GET("https://data.covid19.go.id/public/api/update.json")
status_code(resp)
resp_content <- content(resp, as = "parsed", simplifyVector = TRUE)

id_cases_data <-
  resp_content$update$harian %>%
  as_tibble() %>%
  select(key_as_string, jumlah_positif) %>%
  transmute(
    value = jumlah_positif$value,
    time = as.Date(ymd_hms(key_as_string)),
  ) %>%
  filter(time > "2020-12-31")

get_province_cases <- function(province) {
  province <- str_to_upper(province)
  province <- str_replace_all(province, " ", "_")

  province_url <- str_glue("https://data.covid19.go.id/public/api/prov_detail_{province}.json")
  resp <- GET(province_url)
  status_code(resp)

  res <-
    resp %>%
    content(as = "parsed", simplifyVector = TRUE) %>%
    pluck("list_perkembangan") %>%
    as_tibble() %>%
    transmute(
      time = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
      time = as.Date(time),
      value = KASUS
    ) %>%
    filter(time > "2020-12-31")
  return(res)
}

# Data Preparation - Mobility Historical ---------------------------------

mobility_url <-
  "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"

mobility_path <-
  file.path("data", basename(mobility_url))

curl_download(
  url = mobility_url,
  destfile = mobility_path,
  quiet = FALSE
)

id_mobility <- vroom(
  file = unz(mobility_path, "2021_ID_Region_Mobility_Report.csv"),
  col_types = "ccccccccDdddddd",
  altrep = TRUE
)

id_mobility <- id_mobility %>%
  as_tibble() %>%
  rename(
    grocery_and_pharmacy = grocery_and_pharmacy_percent_change_from_baseline,
    retail_and_recreation = retail_and_recreation_percent_change_from_baseline,
    parks = parks_percent_change_from_baseline,
    transit_stations = transit_stations_percent_change_from_baseline,
    workplaces = workplaces_percent_change_from_baseline,
    residential = residential_percent_change_from_baseline
  ) %>%
  mutate(
     across(c('retail_and_recreation','grocery_and_pharmacy','parks','residential', 'workplaces', 'transit_stations'), function(x) x/100.0)
  ) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  left_join(province_metadata, by = c("sub_region_1" = "province_name_en"))

# Geo-map Preparation ---------------------------------------------------

id_shp <- read_sf("data/IDN_adm1_new/IDN_adm1_new.shp")

mobility_province <- id_mobility %>%
  filter(!is.na(sub_region_1)) %>%
  filter(date == "2021-10-12") %>%
  select(date, province_name, retail_and_recreation, grocery_and_pharmacy, parks, residential, workplaces, transit_stations) %>%
  add_column(value = apply(mobility_province[3:8], 1, sd) ) %>%
  mutate(province_name =  if_else(province_name == "DKI Jakarta", "Jakarta Raya",
                          if_else(province_name == "Daerah Istimewa Yogyakarta", "Yogyakarta",
                          if_else(province_name == "Kepulauan Bangka Belitung", "Bangka-Belitung",
                          if_else(province_name == "Papua Barat", "Irian Jaya Barat",
                                  province_name)))))

id_shp <- id_shp %>%
  left_join(mobility_province, by = c("NAME_1" = "province_name"))

# Color Palette

pal <- colorRampPalette(c("lightblue", "navy"))

# Data Preparation - Stay-put Historical ----------------------------------

id_stayput <- vroom(
  file = "data/stayput_country.csv",
  col_types = "Ddd",
  altrep = TRUE
) %>%
  select(date, stayput) %>%
  rename(
    time = date,
    value = stayput
  )

province_stayput <-
  vroom(
    file = "data/stayput_province.csv",
    col_types = "ccDdd",
    altrep = TRUE
  ) %>%
    select(province_name, date, stayput) %>%
    rename(
      time = date,
      value = stayput
    )

# Forecast Function ------------------------------------------------------

get_data_forecast <- function(df) {
  data <- df %>%
    rename(
      y = value,
      ds = time
    )

  last_date = tail(data, n=1)$ds
  periods = as.integer(Sys.Date() - last_date) - 1

  model <- prophet(data)

  future <- make_future_dataframe(model, periods = periods)
  forecast <- predict(model, future)

  forecast <- forecast %>%
    select(ds, yhat) %>%
    rename(
      value = yhat,
      time = ds
    )
  return(forecast)
}

# Unit Test
# id_cases_forecast <- get_data_forecast(id_cases_data %>% filter( time < Sys.Date()-30))

# Run Service ------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(
    title = strong(uiOutput(outputId = "header")),
    uiOutput(outputId = "dts")
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(width = 6,
         h1(strong(textOutput(outputId = "title")))
      ),
      column(width = 3,
         selectInput("province", "Choose a Province", c('Indonesia', province_metadata$province_name))
      ),
      column(width = 3,
         dateInput("date", strong("Policies Date"), value = Sys.Date()-30, format = "dd MM yyyy"),
      )
    ),
    fluidRow(
      column(width = 6,
         fluidRow(
           tabBox(width = 12,
             id = "tabset1",
             side = "right",
             title = strong("Mobility"),
             tabPanel("Chart",
                selectInput("category",label = NULL,
                    choices = c("Grocery and Pharmacy" = "grocery_and_pharmacy",
                                "Retail and Recreation" = "retail_and_recreation",
                                "Parks" = "parks",
                                "Transit Stations" = "transit_stations",
                                "Workplaces" = "workplaces",
                                "Residential" = "residential" )),
                withSpinner(dygraphOutput("mobilityplot", height = "180px", width = "95%"), type=1),
                br()
              ),
             tabPanel("Description", "Tab content 2")
           )
         ),
         fluidRow(
           tabBox(width = 12,
                  id = "tabset1",
                  side = "right",
                  title = strong("Stay at Home"),
                  tabPanel("Chart",
                     withSpinner(dygraphOutput("stayputplot", height = "210px", width = "95%"), type=1),
                     br()
                  ),
                  tabPanel("Description", "Tab content 2")
           )
         ),
         fluidRow(
           tabBox(width = 12,
                  id = "tabset1",
                  side = "right",
                  title = strong("Cases"),
                  tabPanel("Chart",
                     withSpinner(dygraphOutput("casesplot", height = "200px", width = "95%")),
                     br()
                  ),
                  tabPanel("Description", "Tab content 2")
           )
         )
      ),
      column(width = 6,
         fluidRow(
           valueBoxOutput("grocery_and_pharmacy_box"),
           valueBoxOutput("retail_and_recreation_box"),
           valueBoxOutput("parks_box"),
           valueBoxOutput("transit_stations_box"),
           valueBoxOutput("workplaces_box"),
           valueBoxOutput("residential_box")
         ),
         fluidRow(
           tabBox(width = 12,
              id = "tabset1",
              side = "right",
              title = strong("Mobility Maps"),
              tabPanel("Maps",
                 withSpinner(plotlyOutput('mobility_maps', height = "315px", width = "100%", inline = TRUE))
              ),
              tabPanel("Description", "Tab content 2")
           )
         ),
      )
    )
  )
)

server <- function(input, output) {

  output$header <- renderUI({
    HTML('<div class="flex"><i class="fa fa-virus role="presentation"></i> COVID-19</div>')
  })

  output$title <- renderText({
    input$province
  })

  output$dts <- renderUI({
    HTML('<div style="background-color:#ecf0f4; padding:5px;padding-left:20px;border-top-left-radius: 20px; padding-right:10px"><img src="https://digitalent.kominfo.go.id/assets/@images/logo.png" alt="logo DTS" height="40px" /></div>')
  })

  cases <- reactive({
    if (input$province == 'Indonesia') {
      return(id_cases_data)
    } else {
      print('get province cases')
      return(get_province_cases(input$province))
    }
  })

  case_predict <- reactive({
    get_data_forecast(cases() %>% filter( time < input$date))
  })

  mobilities <- reactive({
    if (input$province == 'Indonesia') {
      id_mobility %>%
        filter(is.na(sub_region_1)) %>%
        select(date, input$category ) %>%
        rename(
          time = date,
          value = input$category
        )
    } else {
      id_mobility %>%
        filter(province_name == input$province) %>%
        select(date, input$category ) %>%
        rename(
          time = date,
          value = input$category
        )
    }
  })

  status_mobilities <- reactive({
    if (input$province == 'Indonesia') {
      id_mobility %>%
        filter(is.na(sub_region_1)) %>%
        filter(row_number()==n())
    } else {
      id_mobility %>%
        filter(province_name == input$province) %>%
        filter(row_number()==n())
    }
  })

  mobility_predict <- reactive({
    get_data_forecast(mobilities() %>% filter( time < input$date))
  })

  stayputs <- reactive({
    if (input$province == 'Indonesia') {
      id_stayput
    } else {
      province_stayput %>%
        filter(province_name == input$province)
    }
  })

  stayput_predict <- reactive({
    get_data_forecast(stayputs() %>% filter( time < input$date))
  })

  output$casesplot <- renderDygraph({
    cases_data <- xts(x = cases()$value, order.by = cases()$time)
    cases_forecast <- xts(x = case_predict()$value, order.by = case_predict()$time)
    data <- cbind(cases_data, cases_forecast)
    dygraph(data)%>%
      dySeries("cases_data", fillGraph = TRUE, stepPlot = TRUE, color = "dark") %>%
      dySeries("cases_forecast", color = "red", strokePattern = "dashed") %>%
      dyRangeSelector(height = 20, strokeColor = "dark", dateWindow = c(input$date - 3, Sys.Date())) %>%
      dyEvent(input$date, "Policy Started", labelLoc = "bottom") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>%
      dyLegend(show = "follow") %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y", label = "Cases Per Day")
  })

  output$grocery_and_pharmacy_box <- renderValueBox({
    valueBox(
      paste0(status_mobilities()$grocery_and_pharmacy*100, "%"), "Grocery & Pharmacy", icon = icon("pills"), color = "teal"
    )
  })

  output$retail_and_recreation_box <- renderValueBox({
    valueBox(
      paste0(status_mobilities()$retail_and_recreation*100, "%"), "Retail & Recreation", icon = icon("store"), color = "green"
    )
  })

  output$parks_box <- renderValueBox({
    valueBox(
      paste0(status_mobilities()$parks*100, "%"), "Parks", icon = icon("tree"), color = "olive"
    )
  })

  output$transit_stations_box <- renderValueBox({
    valueBox(
      paste0(status_mobilities()$transit_stations*100, "%"), "Transit Station", icon = icon("subway"), color = "aqua"
    )
  })

  output$workplaces_box <- renderValueBox({
    valueBox(
      paste0(status_mobilities()$workplaces*100, "%"), "Workplaces", icon = icon("building"), color = "light-blue"
    )
  })

  output$residential_box <- renderValueBox({
    valueBox(
      paste0(status_mobilities()$residential*100, "%"), "Residential", icon = icon("home"), color = "blue"
    )
  })

  output$mobilityplot <- renderDygraph({
    mobilities_data <- xts(x = mobilities()$value, order.by = mobilities()$time)
    mobilities_forecast <- xts(x = mobility_predict()$value, order.by = mobility_predict()$time)
    data <- cbind(mobilities_data, mobilities_forecast)
    dygraph(data)%>%
      dySeries("mobilities_data", fillGraph = TRUE, color = "navy") %>%
      dySeries("mobilities_forecast", color = "red", strokePattern = "dashed") %>%
      dyRangeSelector(height = 20, strokeColor = "blue", dateWindow = c(input$date - 3, Sys.Date())) %>%
      dyEvent(input$date, "Policy Started", labelLoc = "bottom") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>%
      dyLegend(show = "follow") %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y", label = input$category)
  })

  output$mobility_maps <- renderPlotly({
    plot_ly(id_shp, color = ~value, split = ~NAME_1, span = I(1),
            text = ~paste0("<b>", NAME_1, "</b><br>",
                          "Grocery & Pharmacy <b>", grocery_and_pharmacy*100, "%</b><br>",
                          "Retail & Recreation <b>", retail_and_recreation*100, "%</b> <br>",
                          "Parks <b>", parks*100, "%</b> <br>",
                          "Transit Stations <b>", transit_stations*100, "%</b><br>",
                          "Workplaces <b>", workplaces*100, "%</b><br>",
                          "Residentials <b>", residential*100, "%</b>"),
            showlegend = FALSE, hoveron = "fills", hoverinfo = "text",
            colors=pal(20), hoverlabel=list(bgcolor="white", bordercolor="navy")) %>%
      colorbar(len=0.5, thickness=10, tickfont=list(size=8))

  })

  output$stayputplot <- renderDygraph({
    stayputs_data <- xts(x = stayputs()$value, order.by = stayputs()$time)
    stayputs_forecast <- xts(x = stayput_predict()$value, order.by = stayput_predict()$time)
    data <- cbind(stayputs_data, stayputs_forecast)
    dygraph(data)%>%
      dySeries("stayputs_data", fillGraph = TRUE, color = "green") %>%
      dySeries("stayputs_forecast", color = "red", strokePattern = "dashed") %>%
      dyRangeSelector(height = 20, strokeColor = "green", dateWindow = c(input$date - 3, Sys.Date())) %>%
      dyEvent(input$date, "Policy Started", labelLoc = "bottom") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>%
      dyLegend(show = "follow") %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y", label = "Stayput Index")
  })
}

shinyApp(ui, server)
