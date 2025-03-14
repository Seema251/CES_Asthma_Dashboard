# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(sf)
library(tidyverse)
library(DT)
library(viridis)
library(tigris)

load(file.path("data", "CES_Datasets.RData"))
ces_4 <- readRDS(file.path("data", "ces_4.rds"))
ces_demo <- readRDS(file.path("data", "ces_demo.rds"))



# Rename columns to match dataset
ces_4.1 <- ces_4 %>%
  rename(
    census_tract = `Census Tract`,
    total_population = `Total Population`,
    california_county = `California County`,
    zip = ZIP,
    longitude = Longitude,
    latitude = Latitude,
    ces_4_score = `CES 4.0 Score`,
    pm2_5 = `PM2.5`,
    pollution_burden_score = `Pollution Burden Score`,
    asthma_pctl = `Asthma Pctl`
  )

ces_demo.1 <- ces_demo %>%
  rename(
    ces_4_score = `ces_4.0_score`,
    hispanic = `hispanic_(%)`,
    white = `white_(%)`,
    african_american = `african_american_(%)`,
    asian_american = `asian_american_(%)`,
    other_multiple = `other/multiple_(%)`,
    native_american = `native_american_(%)`
  )

# Load California county shapefile
ca_counties <- counties(state = "CA", cb = TRUE) %>%
  st_transform(crs = 4326) %>%
  select(NAME, geometry) %>%
  rename(california_county = NAME)

# Prepare GIS dataset
ces_4_gis <- ces_4.1 %>%
  select(california_county, longitude, latitude, ces_4_score, pollution_burden_score, total_population) %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  group_by(california_county) %>%
  summarise(
    mean_ces = mean(ces_4_score, na.rm = TRUE),
    total_population = sum(total_population, na.rm = TRUE),
    mean_pollution = mean(pollution_burden_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_ces = round(mean_ces, 2),
    mean_pollution = round(mean_pollution, 2),
    total_population = round(total_population, 0)
  )

# Merge GIS dataset with county shapefile
ces_gis <- left_join(ca_counties, ces_4_gis, by = "california_county") %>%
  st_as_sf()

# Prepare PM2.5 vs. Asthma dataset
ces_asthma <- ces_4.1 %>%
  select(california_county, total_population, pm2_5, asthma_pctl) %>%
  group_by(california_county) %>%
  summarise(
    pm2_5 = median(pm2_5, na.rm = TRUE),
    mean_asthma_pctl = mean(asthma_pctl, na.rm = TRUE),
    total_population = sum(total_population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pm2_5 = round(pm2_5, 2),
    mean_asthma_pctl = round(mean_asthma_pctl, 2),
    total_population = round(total_population, 0)
  )

# Prepare race distribution dataset
ces_race <- ces_demo.1 %>%
  select(california_county, ces_4_score, hispanic, white, african_american, asian_american, other_multiple, native_american) %>%
  group_by(california_county) %>%
  summarise(
    asian_american = mean(asian_american, na.rm = TRUE),
    african_american = mean(african_american, na.rm = TRUE),
    hispanic = mean(hispanic, na.rm = TRUE),
    white = mean(white, na.rm = TRUE),
    mixed_race = mean(c(native_american, other_multiple), na.rm = TRUE),
    mean_ces = mean(ces_4_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate_if(is.numeric, round, 2)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "California Environmental & Health Dashboard"),
  
  dashboardSidebar(
    selectInput("county", "Select County:", 
                choices = unique(ces_demo.1$california_county), 
                selected = "Marin")
  ),
  
  dashboardBody(
    # Scientific Report Section
    fluidRow(
      box(title = "Scientific Report", width = 12, solidHeader = TRUE, status = "primary",
          h4("Environment, Social Determinants of Health, and Asthma in California in 2024"),
          p("This dashboard explores the relationship between CES 4.0, PM2.5, race, and county-level population in California."),
          p("Asthma affects ~5.2 million residents, with environmental exposures playing a key role. We analyze geospatial CES scores, racial disparities, and pollution effects.")
      )
    ),
    
    # Dashboard Visualization Components
    fluidRow(
      box(title = "Choropleth Map of CES Score", width = 7, 
          plotlyOutput("ces_map", height = "500px")),  # Increased height to prevent shrinking
      box(title = "Race Distribution in Selected Counties", width = 5, 
          plotOutput("race_plot", height = "500px"))
    ),
    
    fluidRow(
      box(title = "PM2.5 vs. Asthma Prevalence", width = 12, plotlyOutput("bubble_chart", height = "500px"))
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Print working directory and list files for debugging
  print(paste("Current Working Directory:", getwd()))
  print("Files available in the working directory:")
  print(list.files())
  
 
  # Render Choropleth Map with Proper Sizing
  output$ces_map <- renderPlotly({
    p <- ggplot(ces_gis) +  
      geom_sf(aes(fill = mean_ces, 
                  text = paste(
                    "County: ", california_county,
                    "<br>CES 4.0: ", mean_ces,  
                    "<br>Total Population: ", total_population,
                    "<br>Pollution Burden: ", mean_pollution
                  )), 
              color = "white", size = 0.3) +    
      scale_fill_viridis_c(direction = -1, name = "CES 4.0") +
      theme_minimal() +
      labs(title = "Choropleth Map of CES Score") +
      coord_sf(expand = FALSE)  # Ensures aspect ratio is maintained
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        autosize = FALSE,   
        width = 700,        
        height = 500,       
        legend = list(orientation = "v", x = 1, y = 1)  # Ensure legend appears
      ) %>%
      config(displayModeBar = FALSE)  # Hide plotly mode bar to avoid accidental edits
  })
  
  
  # Race Distribution Plot
  output$race_plot <- renderPlot({
    ces_race_filtered <- ces_race %>%
      filter(california_county == input$county) %>%
      pivot_longer(cols = c(asian_american, african_american, hispanic, white, mixed_race),
                   names_to = "race",
                   values_to = "percentage") %>%
      filter(!is.na(percentage))
    
    ggplot(ces_race_filtered, aes(x = race, y = percentage, fill = race)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = paste("Race Distribution in", input$county)) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))  # Adjusted angle for clarity
  })
  
  # PM2.5 vs. Asthma Bubble Chart
  output$bubble_chart <- renderPlotly({
    plot_ly(
      data = ces_asthma,
      x = ~pm2_5,
      y = ~mean_asthma_pctl,
      size = ~total_population * 0.0001,
      color = ~california_county, 
      text = ~paste(
        "County: ", california_county,
        "<br>PM2.5: ", pm2_5, 
        "<br>Asthma Prevalence: ", mean_asthma_pctl
      ),
      type = 'scatter',
      mode = 'markers',
      marker = list(sizemode = 'diameter')
    ) %>%
      layout(
        title = "PM2.5 vs. Asthma Prevalence",
        xaxis = list(title = "PM2.5 Concentration (µg/m³)"),
        yaxis = list(title = "Mean Asthma Percentile"),
        showlegend = TRUE,
        legend = list(
          orientation = "v",  # Vertical legend
          x = 1.05,  # Move legend to the right
          y = 0.5,  # Center it vertically
          font = list(size = 9),
          traceorder = "reversed"  # Helps in managing long legends
        )
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
# rsconnect::deployApp(appFiles = c("app.R",  "data/CES_Datasets.RData", 
#                                     "data/ces_4.rds", 
#                                      "data/ces_demo.rds"),
#                 forceUpdate = TRUE)
