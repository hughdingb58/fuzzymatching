library(tidyverse)
library(shiny)
library(readxl)
library(openxlsx)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Fuzzy Logic"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      fileInput("bus_file", "BUS file (xlsx)"),
      numericInput("margin", "Margin of error for string matching",
                   2, 0, 100, 1),
      numericInput("sheet_num", "Sheet number",
                   1, 0, 999, 1), br(),
      actionButton("load_file", "Load file")
    ),
    
    mainPanel(
      tags$b("Beneficiaries with only one letter for a name:"),
      textOutput("one_letter_name"),
      tags$b("Beneficiaries with reversed first and middle name:"),
      textOutput("reversed_first_middle"),
      tags$b("Beneficiaries with identical names except for hyphens or caps:"),
      textOutput("hyphen_caps"), br(), br(),
      downloadButton("download_data", "Download data with fuzzy matches")),
  )
))