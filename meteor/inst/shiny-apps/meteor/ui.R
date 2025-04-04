# UI for the meteor Shiny application

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

# Define UI for application
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard header
  dashboardHeader(
    title = "METEOR",
    titleWidth = 230
  ),
  
  # Dashboard sidebar
  dashboardSidebar(
    width = 230,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table")),
      menuItem("Meta-Analysis", tabName = "meta", icon = icon("chart-bar")),
      menuItem("Visualization", tabName = "viz", icon = icon("chart-line")),
      menuItem("Compare Local Data", tabName = "compare", icon = icon("exchange-alt")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  # Dashboard body
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            title = "Welcome to METEOR",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            "METEOR (Meta-analysis Tool for Exploring antimicrobial resistance Outcomes across Realms) helps researchers analyze antimicrobial resistance data across human, animal, and environmental domains."
          )
        ),
        fluidRow(
          valueBoxOutput("total_studies_box", width = 4),
          valueBoxOutput("total_datapoints_box", width = 4),
          valueBoxOutput("avg_resistance_box", width = 4)
        ),
        fluidRow(
          box(
            title = "Resistance by Pathogen",
            width = 6,
            status = "primary",
            plotlyOutput("dashboard_pathogen_plot", height = 300)
          ),
          box(
            title = "Resistance by Antibiotic",
            width = 6,
            status = "primary",
            plotlyOutput("dashboard_antibiotic_plot", height = 300)
          )
        ),
        fluidRow(
          box(
            title = "Geographic Distribution",
            width = 12,
            status = "primary",
            plotlyOutput("dashboard_geo_plot", height = 400)
          )
        )
      ),
      
      # Data Explorer tab
      tabItem(
        tabName = "explorer",
        fluidRow(
          box(
            title = "Filter Data",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            fluidRow(
              column(
                width = 3,
                selectInput("explorer_pathogen", "Pathogen", choices = NULL, multiple = TRUE)
              ),
              column(
                width = 3,
                selectInput("explorer_antibiotic", "Antibiotic", choices = NULL, multiple = TRUE)
              ),
              column(
                width = 3,
                selectInput("explorer_location", "Location", choices = NULL, multiple = TRUE)
              ),
              column(
                width = 3,
                sliderInput("explorer_year_range", "Publication Year", min = 1990, max = 2023, value = c(1990, 2023))
              )
            ),
            fluidRow(
              column(
                width = 3,
                selectInput("explorer_population", "Population", choices = NULL, multiple = TRUE)
              ),
              column(
                width = 3,
                selectInput("explorer_study_type", "Study Type", choices = NULL, multiple = TRUE)
              ),
              column(
                width = 3,
                sliderInput("explorer_resistance_range", "Resistance Rate", min = 0, max = 1, value = c(0, 1), step = 0.1)
              ),
              column(
                width = 3,
                actionButton("explorer_filter", "Apply Filters", class = "btn-primary")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Data Table",
            width = 12,
            status = "primary",
            DTOutput("explorer_table")
          )
        ),
        fluidRow(
          box(
            title = "Data Summary",
            width = 12,
            status = "primary",
            plotlyOutput("explorer_summary_plot", height = 300)
          )
        )
      ),
      
      # Meta-Analysis tab
      tabItem(
        tabName = "meta",
        fluidRow(
          box(
            title = "Meta-Analysis Settings",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 4,
                selectInput("meta_group_by", "Group By", 
                           choices = c("pathogen", "antibiotic", "location", "population"), 
                           multiple = TRUE)
              ),
              column(
                width = 4,
                selectInput("meta_method", "Method", 
                           choices = c("random" = "random", "fixed" = "fixed"),
                           selected = "random")
              ),
              column(
                width = 4,
                actionButton("meta_run", "Run Meta-Analysis", class = "btn-primary")
              )
            )
          )
        ),
        fluidRow(
          tabBox(
            title = "Results",
            width = 12,
            tabPanel(
              "Forest Plot",
              plotlyOutput("meta_forest_plot", height = 500)
            ),
            tabPanel(
              "Summary Table",
              DTOutput("meta_summary_table")
            ),
            tabPanel(
              "Heterogeneity",
              verbatimTextOutput("meta_heterogeneity")
            )
          )
        )
      ),
      
      # Visualization tab
      tabItem(
        tabName = "viz",
        fluidRow(
          box(
            title = "Visualization Settings",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 3,
                selectInput("viz_type", "Plot Type", 
                           choices = c("Forest Plot" = "forest",
                                     "Heatmap" = "heatmap",
                                     "Geographic Map" = "geo",
                                     "Trend Plot" = "trend",
                                     "Comparison Plot" = "comparison"))
              ),
              column(
                width = 3,
                uiOutput("viz_options_1")
              ),
              column(
                width = 3,
                uiOutput("viz_options_2")
              ),
              column(
                width = 3,
                actionButton("viz_create", "Create Visualization", class = "btn-primary")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Visualization",
            width = 12,
            status = "primary",
            plotlyOutput("viz_plot", height = 600)
          )
        ),
        fluidRow(
          box(
            title = "Download Options",
            width = 12,
            status = "primary",
            fluidRow(
              column(
                width = 3,
                selectInput("viz_download_format", "Format", 
                           choices = c("PNG" = "png", "PDF" = "pdf", "HTML" = "html"))
              ),
              column(
                width = 3,
                numericInput("viz_download_width", "Width (px)", value = 800, min = 400, max = 3000)
              ),
              column(
                width = 3,
                numericInput("viz_download_height", "Height (px)", value = 600, min = 300, max = 3000)
              ),
              column(
                width = 3,
                downloadButton("viz_download", "Download", class = "btn-primary")
              )
            )
          )
        )
      ),
      
      # Compare Local Data tab
      tabItem(
        tabName = "compare",
        fluidRow(
          box(
            title = "Upload Local Data",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 6,
                fileInput("compare_file", "Upload CSV or Excel File",
                         accept = c(".csv", ".xls", ".xlsx")),
                checkboxInput("compare_header", "File has header", TRUE)
              ),
              column(
                width = 6,
                selectInput("compare_group_by", "Group By", 
                           choices = c("pathogen", "antibiotic"), 
                           multiple = TRUE),
                actionButton("compare_run", "Run Comparison", class = "btn-primary")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Column Mapping",
            width = 12,
            status = "primary",
            uiOutput("compare_mapping")
          )
        ),
        fluidRow(
          tabBox(
            title = "Comparison Results",
            width = 12,
            tabPanel(
              "Visual Comparison",
              plotlyOutput("compare_plot", height = 500)
            ),
            tabPanel(
              "Summary Table",
              DTOutput("compare_table")
            ),
            tabPanel(
              "Statistical Analysis",
              verbatimTextOutput("compare_stats")
            )
          )
        )
      ),
      
      # About tab
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About METEOR",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            "METEOR (Meta-analysis Tool for Exploring antimicrobial resistance Outcomes across Realms) is an R package for meta-analysis of antimicrobial resistance data across human, animal, and environmental domains.",
            br(), br(),
            "Version:", textOutput("about_version", inline = TRUE),
            br(),
            "Data Sources:",
            tags$ul(
              tags$li("Human AMR data: Literature review of studies from 1990-2023"),
              tags$li("Animal AMR data: Coming soon"),
              tags$li("Environmental AMR data: Coming soon")
            ),
            "References:",
            tags$ul(
              tags$li("Citation information will be listed here")
            )
          )
        )
      )
    )
  )
) 