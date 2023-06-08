# Title: SSC/IA Dynamic Data
# Description: Web App for analyzing space data
# Author: Jackson Keane


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)


# ===============================================
# Import data
# ===============================================

dat <- read.delim(
  file = "space2.txt",
  colClasses = c(rep("character", 7), rep("NULL", 11), rep("character", 6), 
                 "NULL", "NULL", "character", rep("NULL", 40)),
  stringsAsFactors = FALSE,
)

colnames(dat) <- c(
     "Sat_Name",
     "Country_of_UN_Registry",
     "Country_of_Operator_Owner",
     "Org_of_Operation_Ownership",
     "Sector",
     "General_Purpose",
     "Detailed_Purpose",
     "Date_of_Launch",
     "Execpted_Lifetime",
     "Contractor",
     "Country_of_Contractor",
     "Launch_Site",
     "Launch_Vehicle",
     "Additional_Comments_Details"
)

# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Security Cooperations: Sovereign & Shared Space Assets"),
  fluidRow(
    selectInput(inputId = "select",
                label = "Select Security Cooperation Region",
                choices = c("EUCOM" = "EU",
                            "INDOPACOM" = "INDO"))),
  fluidRow(
    radioButtons(inputId = "radio", 
                label = "Select EUCOM entity of interest",
                choices = c("GBR" = "United Kingdom",
                            "DEU" = "Germany",
                            "FRA" = "France",
                            "LUX" = "Luxembourg",
                            "NOR" = "Norway",
                            "ITA" = "Italy",
                            "ESP" = "Spain",
                            "BEL" = "Belgium",
                            "POL" = "Poland",
                            "HUN" = "Hungary",
                            "EURO Space Agency" = "ESA",
                            "Multinational" = "Multinational"),
                selected = "United Kingdom",
                inline = TRUE),
    radioButtons(inputId = "rdo",
                label = "Select INDOPACOM entity of interest",
                choices = c("AUS" = "Australia",
                            "JPN" = "Japan",
                            "KOR" = "South Korea",
                            "NZL" = "New Zealand",
                            "SGP" = "Singapore",
                            "IND" = "India",
                            "PHL" = "Philippines",
                            "VNM" = "Vietnam",
                            "BGD" = "Bangladesh",
                            "MHL" = "Marshall Islands"),
                selected = "Australia",
                inline = TRUE)),
  checkboxInput(inputId = "stops",
                label = strong(paste("Include Partnered Assets", "(Default shows ONLY Sovereign Assets)")),
                value = FALSE),

  hr(),
  tabsetPanel(type = "tabs",
              tabPanel("General Info",
                       h3("Raw Data from Source"),
                       dataTableOutput("table1")),
              tabPanel("Specific Info", 
                       h3("Detailed Pulls from Source"),
                       dataTableOutput("table2"))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  key <- reactive({
    if (input$select == "INDO") {
      Key <- input$rdo
    } else if (input$select == "EU") {
      Key <- input$radio
    }
    
    Key
  })
  
  gen_tab <- reactive({
    if (input$stops) {
      vec <- unique(pull(dat, Country_of_Operator_Owner))
      filter_dat <- dat %>% filter(Country_of_Operator_Owner %in% vec[str_detect(vec, paste0(".*", key(), ".*"))]) %>% select(c(1:6, 10, 11))
      if (key() == "United Kingdom") {
        new_row <- dat %>% filter(Country_of_Operator_Owner == "Poland/UK") %>% select(c(1:6, 10, 11))
        filter_dat[nrow(filter_dat) + 1, ] <- as.character(new_row[1,])
      }
    } else {
      filter_dat <- dat %>% filter(Country_of_Operator_Owner == key()) %>% select(c(1:6, 10, 11))
    }
    
    colnames(filter_dat) <- c(
      "Sat Name",
      "Country of UN Registry",
      "Country of Operator/Owner",
      "Org of Operation Ownership",
      "Sector",
      "General Purpose",
      "Contractor",
      "Country of Contractor"
    )
    
    filter_dat
  })
  
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  output$table1 <- renderDataTable({
    gen_tab()
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  spec_tab <- reactive({
    if (input$stops) {
      vec <- unique(pull(dat, Country_of_Operator_Owner))
      fil_dat <- dat %>% filter(Country_of_Operator_Owner %in% vec[str_detect(vec, paste0(".*", key(), ".*"))]) %>% select(c(1, 7:9, 12:14))
      if (key() == "United Kingdom") {
        new_row <- dat %>% filter(Country_of_Operator_Owner == "Poland/UK") %>% select(c(1, 7:9, 12:14))
        fil_dat[nrow(fil_dat) + 1, ] <- as.character(new_row[1,])
      }
    } else {
      fil_dat <- dat %>% filter(Country_of_Operator_Owner == key()) %>% select(c(1, 7:9, 12:14))
    }
    
    colnames(fil_dat) <- c(
      "Sat Name",
      "Detailed Purpose",
      "Date of Launch",
      "Execpted Lifetime",
      "Launch Site",
      "Launch Vehicle",
      "Additional Comments Details"
    )
    
    fil_dat
    
  })
  
  output$table2 <- renderDataTable({
    spec_tab()
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)
