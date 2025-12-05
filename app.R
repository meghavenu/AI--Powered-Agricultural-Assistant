# ------------------------------
# Load Libraries
# ------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)
library(randomForest)

# ------------------------------
# Load Data
# ------------------------------
merged <- read.csv("output/merged_data.csv")

# ------------------------------
# Load trained Random Forest model and metadata
# ------------------------------
rf_model <- readRDS("models/yield_rf.rds")
meta     <- readRDS("models/meta.rds")

# ------------------------------
# Crop Recommendation Function
# ------------------------------
recommend_crop <- function(state_input, rainfall_input){
  df <- merged %>% filter(state == state_input)
  if(nrow(df) == 0){
    return(data.frame(Message = "State not found in dataset"))
  }
  df_summary <- df %>%
    group_by(crop_name) %>%
    summarise(
      avg_yield = mean(yield, na.rm = TRUE),
      avg_rainfall = mean(annual_rainfall, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      deviation = abs(avg_rainfall - rainfall_input)/avg_rainfall*100,
      suitability = case_when(
        deviation <= 20 ~ "Best",
        deviation <= 40 ~ "Moderate",
        TRUE ~ "Not Recommended"
      )
    ) %>%
    arrange(desc(avg_yield))
  return(df_summary)
}

# ------------------------------
# Seasonal Planting Calendar
# ------------------------------
planting_calendar <- data.frame(
  Crop = c("Rice", "Wheat", "Maize", "Millet"),
  Sowing = c("June-July", "November", "June", "July"),
  Harvest = c("October-November", "April", "September", "October")
)

# ------------------------------
# UI
# ------------------------------
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "YieldWise🌾"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Visualizations", tabName = "visuals", icon = icon("chart-line")),
      menuItem("Crop Recommendation", tabName = "recommend", icon = icon("seedling")),
      menuItem("Planting Calendar", tabName = "calendar", icon = icon("calendar")),
      menuItem("Rainfall Alert", tabName = "rainfall", icon = icon("cloud-rain")),
      menuItem("Interactive Map", tabName = "map", icon = icon("map")),
      menuItem("Tips & Notes", tabName = "tips", icon = icon("lightbulb")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      menuItem("AI Assistant", tabName = "ai", icon = icon("robot"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body, .content-wrapper { background-color: #f0fff0 !important; }
        .box { border-radius: 12px; box-shadow: 2px 2px 6px #ccc; }
        .skin-green .main-header .logo { background-color: #228B22 !important; color: white; font-weight: bold; }
        .skin-green .main-header .navbar { background-color: #2E8B57 !important; }
      "))
    ),
    tabItems(
      # ------------------------------ Overview ------------------------------
      tabItem(tabName = "overview",
              fluidRow(
                box(width = 12, status = "success", solidHeader = TRUE, title = "Welcome to YieldWise",
                    tags$div(
                      style = "font-size:16px; line-height:1.6;",
                      p(strong("Smart Crop & Climate Insights for Farmers")),
                      p("YieldWise integrates historical crop data and climate factors to help farmers make informed decisions."),
                      tags$h4("Key Features:"),
                      tags$ul(
                        tags$li(icon("chart-line"), " Interactive Visualizations"),
                        tags$li(icon("seedling"), " Crop Recommendations"),
                        tags$li(icon("calendar"), " Seasonal Planting Calendar"),
                        tags$li(icon("cloud-rain"), " Rainfall Alerts"),
                        tags$li(icon("map"), " Interactive Map"),
                        tags$li(icon("lightbulb"), " Agronomic Tips"),
                        tags$li(icon("robot"), " AI Assistant for personalized guidance")
                      )
                    )
                )
              )
      ),
      
      # ------------------------------ Visualizations ------------------------------
      tabItem(tabName = "visuals",
              fluidRow(
                box(width=4, selectInput("state", "Select State:", choices=unique(merged$state))),
                box(width=4, selectInput("crop", "Select Crop:", choices=unique(merged$crop_name))),
                box(width=4, sliderInput("year", "Select Year Range:", min=min(merged$year, na.rm=TRUE),
                                         max=max(merged$year, na.rm=TRUE), value=c(min(merged$year, na.rm=TRUE), max(merged$year, na.rm=TRUE)), sep=""))
              ),
              tabsetPanel(
                tabPanel("Yield Trend", plotlyOutput("yieldTrend")),
                tabPanel("Rainfall vs Yield", plotlyOutput("rainVsYield")),
                tabPanel("Regression", plotlyOutput("regPlot")),
                tabPanel("State-wise Comparison", plotlyOutput("barPlot")),
                tabPanel("Yield Distribution", plotlyOutput("boxPlot")),
                tabPanel("Correlation Heatmap", plotlyOutput("heatmap"))
              )
      ),
      
      # ------------------------------ Crop Recommendation ------------------------------
      tabItem(tabName = "recommend",
              fluidRow(
                box(width=4, selectInput("rec_state", "Select State:", choices=unique(merged$state))),
                box(width=4, numericInput("rec_rain", "Expected Rainfall (mm):", value=800, min=100, max=3000, step=50)),
                box(width=4, actionButton("goRec", "Get Recommendation", icon=icon("search")))
              ),
              fluidRow(
                box(width=12, status="success", solidHeader=TRUE, title="Recommended Crops 🌾", DTOutput("recTable"))
              )
      ),
      
      # ------------------------------ Planting Calendar ------------------------------
      tabItem(tabName = "calendar",
              fluidRow(box(width=12, status="info", solidHeader=TRUE, title="Seasonal Planting Calendar", DTOutput("calendarTable")))
      ),
      
      # ------------------------------ Rainfall Alert ------------------------------
      tabItem(tabName = "rainfall",
              fluidRow(
                box(width=12, status="warning", solidHeader=TRUE, title="Rainfall Alert",
                    selectInput("rec_crop_alert", "Select Crop for Alert:", choices=unique(merged$crop_name)),
                    numericInput("rec_rain_alert", "Expected Rainfall (mm):", value=800, min=100, max=3000),
                    textOutput("rainAlert")
                )
              )
      ),
      
      # ------------------------------ Interactive Map ------------------------------
      tabItem(tabName = "map",
              fluidRow(
                box(width=12, selectInput("crop_map", "Select Crop for Map:", choices=unique(merged$crop_name)),
                    leafletOutput("yieldMap"), height=600)
              )
      ),
      
      # ------------------------------ Tips & Notes ------------------------------
      tabItem(tabName = "tips",
              fluidRow(
                box(width=12, status="primary", solidHeader=TRUE, title="Farming Tips & Notes",
                    selectInput("tips_crop", "Select Crop:", choices = unique(merged$crop_name)),
                    selectInput("tips_state", "Select State:", choices = unique(merged$state)),
                    numericInput("tips_rain", "Expected Rainfall (mm):", value=800, min=100, max=3000, step=50),
                    uiOutput("agriTips")
                )
              )
      ),
      
      # ------------------------------ Data Explorer ------------------------------
      tabItem(tabName = "data",
              fluidRow(box(width=12, DTOutput("dataTable")))
      ),
      
      # ------------------------------ AI Assistant ------------------------------
      tabItem(tabName = "ai",
              fluidRow(
                box(width=12, status="success", solidHeader=TRUE, title="AI Farming Assistant 🤖",
                    selectInput("ai_crop", "Select Crop:", choices = meta$crop_levels),
                    selectInput("ai_state", "Select State:", choices = meta$state_levels),
                    numericInput("ai_rainfall", "Expected Annual Rainfall (mm):", value = NA, min=100, max=3000, step=50),
                    numericInput("ai_area", "Area (ha):", value = NA),
                    numericInput("ai_fert", "Fertilizer (kg):", value = NA),
                    numericInput("ai_pest", "Pesticide (kg):", value = NA),
                    actionButton("predict_yield", "Predict Yield", icon = icon("tractor")),
                    br(), br(),
                    verbatimTextOutput("ai_yield_output")
                )
              )
      )
    )
  )
)

# ------------------------------
# SERVER
# ------------------------------
server <- function(input, output, session){
  
  # ------------------- Filtered Data -------------------
  data_sub <- reactive({
    merged %>% filter(state == input$state, crop_name == input$crop,
                      year >= input$year[1], year <= input$year[2])
  })
  
  # ------------------- Visualizations -------------------
  output$yieldTrend <- renderPlotly({
    df <- data_sub()
    plot_ly(df, x = ~year, y = ~yield, type = "scatter", mode = "lines+markers",
            line = list(color = "forestgreen")) %>%
      layout(title="Crop Yield Trend Over Years", xaxis=list(title="Year"), yaxis=list(title="Yield (kg/ha)"))
  })
  
  output$rainVsYield <- renderPlotly({
    df <- data_sub()
    plot_ly(df, x = ~annual_rainfall, y = ~yield, type = "scatter", mode = "markers",
            marker = list(color = "darkgreen")) %>%
      layout(title="Rainfall vs Yield", xaxis=list(title="Annual Rainfall (mm)"), yaxis=list(title="Yield (kg/ha)"))
  })
  
  output$regPlot <- renderPlotly({
    df <- data_sub()
    fit <- lm(yield ~ annual_rainfall, data=df)
    df$pred <- predict(fit, df)
    plot_ly(df, x=~annual_rainfall, y=~yield, type="scatter", mode="markers", marker=list(color="green")) %>%
      add_lines(x=~annual_rainfall, y=~pred, name="Regression Line", line=list(color="red"))
  })
  
  output$barPlot <- renderPlotly({
    df <- merged %>% filter(crop_name == input$crop,
                            year >= input$year[1], year <= input$year[2]) %>%
      group_by(state) %>% summarise(avg_yield = mean(yield, na.rm=TRUE))
    plot_ly(df, x=~state, y=~avg_yield, type="bar", marker=list(color="seagreen"))
  })
  
  output$boxPlot <- renderPlotly({
    df <- merged %>% filter(crop_name == input$crop,
                            year >= input$year[1], year <= input$year[2])
    plot_ly(df, x=~state, y=~yield, type="box", marker=list(color="darkolivegreen"))
  })
  
  output$heatmap <- renderPlotly({
    num_vars <- merged %>% select(any_of(c("yield", "area", "production", "annual_rainfall", "fertilizer", "pesticide")))
    cor_mat <- round(cor(num_vars, use="complete.obs"),2)
    plot_ly(z=cor_mat, x=colnames(cor_mat), y=rownames(cor_mat), type="heatmap", colors="Greens")
  })
  
  # ------------------- Crop Recommendation -------------------
  observeEvent(input$goRec, {
    rec <- recommend_crop(input$rec_state, input$rec_rain)
    output$recTable <- renderDT({datatable(rec, options=list(pageLength=5), rownames=FALSE)})
  })
  
  # ------------------- Planting Calendar -------------------
  output$calendarTable <- renderDT({ datatable(planting_calendar, options=list(dom='t', pageLength=5)) })
  
  # ------------------- Rainfall Alert -------------------
  output$rainAlert <- renderText({
    req(input$rec_crop_alert, input$rec_rain_alert)
    state_rain <- merged %>% filter(state == input$rec_state, crop_name == input$rec_crop_alert) %>%
      summarise(avg_rain = mean(annual_rainfall, na.rm=TRUE))
    if(nrow(state_rain)==0) return("No data for this state/crop combination")
    diff <- input$rec_rain_alert - state_rain$avg_rain
    msg <- paste0("State: ", input$rec_state, ", Crop: ", input$rec_crop_alert,
                  ", Historical Avg Rainfall: ", round(state_rain$avg_rain,1),
                  " mm. Expected: ", input$rec_rain_alert, " mm. ")
    if(diff < -200) return(paste(msg, "⚠️ BELOW average! Consider drought-resistant crops."))
    if(diff > 200) return(paste(msg, "⚠️ ABOVE average! Consider water-logging tolerant crops."))
    return(paste(msg, "✅ Rainfall is near normal. Safe to plant."))
  })
  
  # ------------------- Interactive Map -------------------
  output$yieldMap <- renderLeaflet({
    req(input$crop_map)
    state_avg <- merged %>% filter(crop_name == input$crop_map) %>%
      group_by(state) %>% summarise(avg_yield=mean(yield, na.rm=TRUE))
    state_avg$lat <- runif(nrow(state_avg), 20, 30)
    state_avg$lng <- runif(nrow(state_avg), 70, 90)
    leaflet(state_avg) %>% addTiles() %>%
      addCircleMarkers(lng=~lng, lat=~lat, radius=~avg_yield/500, color="green",
                       fillOpacity=0.7, label=~paste(state, ": Avg Yield =", round(avg_yield,1)))
  })
  
  # ------------------- Data Explorer -------------------
  output$dataTable <- renderDT({ datatable(merged, options=list(pageLength=10, scrollX=TRUE)) })
  
  # ------------------- Tips -------------------
  output$agriTips <- renderUI({
    req(input$tips_crop, input$tips_state, input$tips_rain)
    crop <- input$tips_crop; state <- input$tips_state; rain <- input$tips_rain
    tips <- c()
    if(crop %in% c("Rice","Paddy")) tips <- c(tips, "Rice prefers well-irrigated fields.")
    else if(crop=="Wheat") tips <- c(tips, "Wheat grows best in cool, dry climates.")
    else if(crop=="Maize") tips <- c(tips, "Maize requires nitrogen-rich fertilizer.")
    else if(crop=="Millet") tips <- c(tips, "Millet is drought-tolerant.")
    else tips <- c(tips, paste("Follow practices for", crop))
    
    if(state %in% c("Punjab","Haryana")) tips <- c(tips, "Avoid overuse of fertilizer.")
    else if(state %in% c("Rajasthan","Gujarat")) tips <- c(tips, "Monitor soil moisture; consider irrigation.")
    else tips <- c(tips, paste("Check extension services for", state))
    
    if(rain<300) tips <- c(tips,"Rainfall is low; use drought-resistant varieties.")
    else if(rain>800) tips <- c(tips,"Rainfall is high; ensure drainage.")
    else tips <- c(tips,"Rainfall is near normal.")
    tags$ul(lapply(tips, tags$li))
  })
  
  # ------------------- AI Assistant -------------------
  observeEvent(input$predict_yield, {
    
    # Handle unknown levels
    crop_input <- ifelse(input$ai_crop %in% meta$crop_levels, input$ai_crop, "Other")
    state_input <- ifelse(input$ai_state %in% meta$state_levels, input$ai_state, "Other")
    
    # Fill medians if numeric inputs are NA
    area_val <- ifelse(is.na(input$ai_area), meta$medians["area"], input$ai_area)
    fert_val <- ifelse(is.na(input$ai_fert), meta$medians["fertilizer"], input$ai_fert)
    pest_val <- ifelse(is.na(input$ai_pest), meta$medians["pesticide"], input$ai_pest)
    rain_val <- ifelse(is.na(input$ai_rainfall), meta$medians["annual_rainfall"], input$ai_rainfall)
    
    new_data <- data.frame(
      annual_rainfall = rain_val,
      area           = area_val,
      fertilizer     = fert_val,
      pesticide      = pest_val,
      crop_name      = factor(crop_input, levels = meta$crop_levels),
      state          = factor(state_input, levels = meta$state_levels),
      year           = as.integer(format(Sys.Date(), "%Y"))
    )
    
    pred_yield <- predict(rf_model, new_data)
    
    output$ai_yield_output <- renderText({
      if(is.na(pred_yield)) {
        "⚠️ Prediction unavailable. Please select a valid crop/state combination."
      } else {
        paste0("🌾 Estimated Yield for ", crop_input, " in ", state_input,
               " with rainfall ", new_data$annual_rainfall, " mm: ", round(pred_yield, 2), " kg/ha")
      }
    })
  })
  
}

# ------------------------------
# Run App
# ------------------------------
shinyApp(ui, server)
