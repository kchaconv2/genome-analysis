#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(maps)
library(mapdata)
library(wesanderson)
library(RColorBrewer)
library(viridis)

### Preparing the times series data

#times series data for confirmed
time_series_confirmed_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
  rename(Province_State = "Province/State", Country_Region = "Country/Region")  %>% 
  pivot_longer(-c(Province_State, Country_Region, Lat, Long),
               names_to = "Date", values_to = "Confirmed") 
# times series data for deaths
time_series_deaths_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
  rename(Province_State = "Province/State", Country_Region = "Country/Region")  %>% 
  pivot_longer(-c(Province_State, Country_Region, Lat, Long),
               names_to = "Date", values_to = "Deaths")
# times series data for recovered
time_series_recovered_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")) %>%
  rename(Province_State = "Province/State", Country_Region = "Country/Region") %>% 
  pivot_longer(-c(Province_State, Country_Region, Lat, Long),
               names_to = "Date", values_to = "Recovered")
# Create Keys 
time_series_confirmed_long <- time_series_confirmed_long %>% 
  unite(Key, Province_State, Country_Region, Date, sep = ".", remove = FALSE)
time_series_deaths_long <- time_series_deaths_long %>% 
  unite(Key, Province_State, Country_Region, Date, sep = ".") %>% 
  select(Key, Deaths)
time_series_recovered_long <- time_series_recovered_long %>% 
  unite(Key, Province_State, Country_Region, Date, sep = ".") %>% 
  select(Key, Recovered)
# Join tables
time_series_long_joined <- full_join(time_series_confirmed_long,
                                     time_series_deaths_long, by = c("Key"))
time_series_long_joined <- full_join(time_series_long_joined,
                                     time_series_recovered_long, by = c("Key")) %>% 
  select(-Key)
# Reformat the data 
time_series_long_joined$Date <- mdy(time_series_long_joined$Date)

# rename the data
global_time_series <- time_series_long_joined

# Get first and last date for graph ***There are NA in the date field to consider
first_date = min(global_time_series$Date, na.rm = TRUE)
last_date = max(global_time_series$Date, na.rm = TRUE)

# Defining reporting types
Report_Type = c("Confirmed", "Deaths", "Recovered")

# load the world map data
world <- map_data("world")
#Latin countries
some.latin <- c("Colombia","Brazil","Peru","Ecuador","Chile","Venezuela","Bolivia","Argentina","Paraguay","Uruguay")
#retrieve map data
some.latin <- map_data("world", region = some.latin)

# Create list of countries
Countries.latin <- global_time_series %>% 
  select(Country_Region) %>% 
  filter (Country_Region %in% c("Colombia","Brazil","Peru","Ecuador","Chile","Venezuela","Bolivia","Argentina","Paraguay","Uruguay"))
#Extract data from countries
global_time_series.latin <- global_time_series %>% 
  filter (Country_Region %in% c("Colombia","Brazil","Peru","Ecuador","Chile","Venezuela","Bolivia","Argentina","Paraguay","Uruguay"))


# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("COVID-19 reporting data"),
  p("This application is updated daily with data from the Johns Hopkins Center for Systems Science and Engineering, for more updated information go to",
    tags$a("GitHub Repository", href="https://github.com/CSSEGISandData"),
    tags$a("JHU COVID-19 dashboard", href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6")
  ),
  tags$br(),
  tags$hr(),  # Adds line to page
  
  sidebarLayout(
    sidebarPanel(
      # Select Reporting type
      selectInput("select_type", 
                  label = "Report Type", 
                  choices = Report_Type, selected = "Confirmed"),
      # Select Date 
      sliderInput("slider_date", label = "Report Date", min = first_date, 
                  max = last_date, value = first_date, step = 7)
    ),
    
    # Show a plots
    mainPanel(
      plotOutput("covidPlot")
    )
  )
)

# Define server logic required to make the plot
server <- function(input, output) {
  
  output$covidPlot <- renderPlot({
    # develop data set to graph
    pick_date <- global_time_series.latin %>% 
      # Fix mapping to map_data of US != USA  
      #mutate(Country_Region = recode(Country_Region, US = "USA")) %>% 
      # *** This is where the slider input with the date goes
      filter(Date == input$slider_date) %>% 
      group_by(Country_Region) %>% 
      summarise_at(c("Confirmed", "Deaths", "Recovered"), sum)

    # We need to join the us map data with our daily report to make one data frame/tibble
    country_join <- left_join(some.latin, pick_date, by = c("region" = "Country_Region"))
    
    # plot world map
    ggplot(data = some.latin, mapping = aes(x = long, y = lat, group = group)) + 
      coord_fixed(1.5) + 
      # Add data layer
      geom_polygon(data = country_join, aes_string(fill = input$select_type), color = "black") +
      scale_fill_gradientn(colours = 
                             wes_palette("Zissou1", 100, type = "continuous"),
                           trans = "log10") +
      
      theme(
        legend.position = "bottom",
        text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "#ffffff", color = NA), 
        panel.background = element_rect(fill = "#ffffff", color = NA), 
        legend.background = element_rect(fill = "#ffffff", color = NA)
      )+
      ggtitle("JHU COVID-19 data for ", input$select_type)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)