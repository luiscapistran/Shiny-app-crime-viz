library(tidyverse)
library(sf)
library(rnaturalearth)
library(shiny)
library(plotly)
library(rsconnect)

# Get the data ready
# setwd("~/Desktop/Data and Programming for PP II/PS 2/homework-2-luiscapistran/")


chicago_shape_ward <- st_read(("chicago_shape.shp"))

crimes <- read_csv("2020_crimes.csv") %>%
  filter(!is.na(LOCATION))

battery_2020 <- crimes %>%
  filter(`PRIMARY DESCRIPTION` == "BATTERY") %>%
  dplyr::select(5,6,11,17) %>%
  group_by(`WARD`) %>%
  summarise(cases = n()) %>%
  mutate(proportion_of_cases = cases/sum(cases))  %>%
  filter(!is.na(WARD)) %>%
  rename(ward = "WARD") %>%
  mutate(year = 2020, type_of_crime = "Battery")

theft_2020 <- crimes %>%
  filter(`PRIMARY DESCRIPTION` == "THEFT") %>%
  dplyr::select(5,6,11,17) %>%
  group_by(`WARD`) %>%
  summarise(cases = n()) %>%
  mutate(proportion_of_cases = cases/sum(cases))  %>%
  filter(!is.na(WARD)) %>%
  rename(ward = "WARD") %>%
  mutate(year = 2020, type_of_crime = "Theft")

battery_2005 <- read_csv("Crimes_-_2005.csv") %>%
  filter(`Primary Type` == "BATTERY") %>%
  dplyr::select(5,11,15) %>%
  group_by(`Ward`) %>%
  summarise(cases = n()) %>%
  mutate(proportion_of_cases = cases/sum(cases)) %>%
  rename(ward = "Ward") %>%
  mutate(year = 2005, type_of_crime = "Battery")

theft_2005 <- read_csv("Crimes_-_2005.csv") %>%
  filter(`Primary Type` == "THEFT") %>%
  dplyr::select(5,11,15) %>%
  group_by(`Ward`) %>%
  summarise(cases = n()) %>%
  mutate(proportion_of_cases = cases/sum(cases)) %>%
  rename(ward = "Ward") %>%
  mutate(year = 2005, type_of_crime = "Theft")

crimes_df <- rbind(battery_2020,theft_2020, battery_2005, theft_2005)
chicago_shape_ward$ward <- as.integer(chicago_shape_ward$ward)
crimes_df$ward <- as.integer(crimes_df$ward)
crimes_shape <- left_join(chicago_shape_ward, crimes_df, by = "ward")

remove(crimes, battery_2020, battery_2005, theft_2020, theft_2005, crimes_df)

##################
## Map Generator## 
#################

ui <- fluidPage(
  fluidRow(
    column(width = 3,
           tags$img(src = "https://d11jve6usk2wa9.cloudfront.net/platform/10747/assets/logo.png",
                    height = 100,
                    width = 190)
    ),
    column(width = 6,
           align = "center",
           tags$h1("Proportion of Crimes in the City of Chicago"),
           tags$hr()
    )
  ),
  fluidRow(
    column(width = 4,
           offset = 2,
           align = "center",
           selectInput(inputId = "type_of_crime",
                       label = "Choose a Type of Crime",
                       choices = c("Battery", "Theft")),
           ),
    column(width = 4,
           align = "center",
           selectInput(inputId = "year",
                       label = "Select the Year to Display",
                       choices = c("2020", "2005")))
    ),
  fluidRow(
    column(width = 10,
           align = "center",
           offset = 2,
           plotlyOutput("map")
           ),
    )
  )

server <- function(input, output) {
  
  data <- reactive({
    crimes_shape %>%
      filter(type_of_crime == input$type_of_crime & 
             year == as.numeric(input$year))
  })
  
  output$map <- renderPlotly({
    plt <- ggplot(data = data()) +
      geom_sf(aes(fill = proportion_of_cases))
      
  })
}

shinyApp(ui = ui, server = server)


# Shiny App: https://luiscapistran.shinyapps.io/ChicagoCrimeMaps/






