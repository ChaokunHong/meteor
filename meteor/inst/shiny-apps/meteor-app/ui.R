# UI for the METEOR Shiny application

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

# Define header
header <- dashboardHeader(
  title = "METEOR",
  titleWidth = 300
)

# Define sidebar
sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    id = "tabs",
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Data", tabName = "data", icon = icon("database"),
            menuSubItem("Import Data", tabName = "import"),
            menuSubItem("View Data", tabName = "view_data"),
            menuSubItem("Quality Check", tabName = "quality")
    ),
    menuItem("Analysis", tabName = "analysis", icon = icon("calculator"),
            menuSubItem("Meta-Analysis", tabName = "meta"),
            menuSubItem("Subgroup Analysis", tabName = "subgroup"),
            menuSubItem("Sensitivity Analysis", tabName = "sensitivity"),
            menuSubItem("Publication Bias", tabName = "bias")
    ),
    menuItem("Visualization", tabName = "visualization", icon = icon("chart-line"),
            menuSubItem("Forest Plot", tabName = "forest"),
            menuSubItem("Geographic Map", tabName = "map"),
            menuSubItem("Resistance Heatmap", tabName = "heatmap")
    ),
    menuItem("Reports", tabName = "reports", icon = icon("file-alt")),
    menuItem("About", tabName = "about", icon = icon("info-circle"))
  )
)

# Define body
body <- dashboardBody(
  tabItems(
    # Home tab
    tabItem(
      tabName = "home",
      fluidRow(
        box(
          title = "Welcome to METEOR",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          h3("Meta-analysis Tool for Exploring antimicrobial resistance Outcomes across Realms"),
          p("METEOR is a comprehensive tool for analyzing antimicrobial resistance (AMR) data across different domains: human, animal, and environment."),
          p("Use the menu on the left to navigate through the different features of the application.")
        )
      ),
      fluidRow(
        box(
          title = "Quick Start",
          width = 6,
          status = "info",
          solidHeader = TRUE,
          p("To get started:"),
          tags$ol(
            tags$li("Import your AMR data using the 'Import Data' option"),
            tags$li("Review and clean the data in 'View Data'"),
            tags$li("Run meta-analyses in the 'Analysis' section"),
            tags$li("Create visualizations in the 'Visualization' section"),
            tags$li("Generate reports in the 'Reports' section")
          ),
          actionButton("goto_import", "Start by Importing Data", 
                      icon = icon("upload"), 
                      class = "btn-success")
        ),
        box(
          title = "Available Datasets",
          width = 6,
          status = "warning",
          solidHeader = TRUE,
          p("METEOR comes with pre-loaded datasets for demonstration:"),
          tags$ul(
            tags$li(actionLink("load_human", "Human AMR Data")),
            tags$li(actionLink("load_animal", "Animal AMR Data (Coming Soon)")),
            tags$li(actionLink("load_environment", "Environment AMR Data (Coming Soon)"))
          )
        )
      )
    ),
    
    # Import data tab
    tabItem(
      tabName = "import",
      fluidRow(
        box(
          title = "Import Data",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          tabsetPanel(
            tabPanel(
              title = "File Upload",
              br(),
              fileInput("dataFile", "Choose CSV or Excel file",
                       multiple = FALSE,
                       accept = c(".csv", ".xls", ".xlsx")),
              selectInput("dataType", "Data Domain:",
                         choices = c("Human" = "human", 
                                    "Animal" = "animal", 
                                    "Environment" = "environment")),
              actionButton("importData", "Import", icon = icon("upload"), class = "btn-primary")
            ),
            tabPanel(
              title = "Sample Data",
              br(),
              selectInput("sampleData", "Select Sample Dataset:",
                         choices = c("Human AMR Data" = "human", 
                                    "Animal AMR Data" = "animal", 
                                    "Environment AMR Data" = "environment")),
              actionButton("loadSample", "Load Sample Data", 
                          icon = icon("database"), class = "btn-primary")
            ),
            tabPanel(
              title = "Data Input Manual",
              br(),
              p("Instructions for preparing your data:"),
              tags$ul(
                tags$li("Data should be in CSV or Excel format"),
                tags$li("Required columns: study ID, firstauthor_last, year_published, location"),
                tags$li("Resistance data columns should be named with the pattern: r_ANTIBIOTIC_PATHOGEN"),
                tags$li("Corresponding numerator and denominator columns should follow the pattern: n_ANTIBIOTIC_PATHOGEN and d_ANTIBIOTIC_PATHOGEN")
              ),
              downloadButton("downloadTemplate", "Download Data Template")
            )
          )
        )
      ),
      fluidRow(
        box(
          title = "Data Preview",
          width = 12,
          status = "info",
          solidHeader = TRUE,
          DTOutput("dataPreview")
        )
      )
    ),
    
    # View data tab
    tabItem(
      tabName = "view_data",
      fluidRow(
        box(
          title = "Data Explorer",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          fluidRow(
            column(
              width = 3,
              selectInput("dataSet", "Select Dataset:",
                         choices = c("Raw Data" = "raw", 
                                    "Validated Data" = "validated", 
                                    "Standardized Data" = "standardized"))
            ),
            column(
              width = 3,
              conditionalPanel(
                condition = "input.dataSet == 'standardized'",
                selectInput("filterPathogen", "Filter by Pathogen:",
                           choices = c("All" = ""))
              )
            ),
            column(
              width = 3,
              conditionalPanel(
                condition = "input.dataSet == 'standardized'",
                selectInput("filterAntibiotic", "Filter by Antibiotic:",
                           choices = c("All" = ""))
              )
            ),
            column(
              width = 3,
              conditionalPanel(
                condition = "input.dataSet == 'standardized'",
                selectInput("filterRegion", "Filter by Region:",
                           choices = c("All" = ""))
              )
            )
          )
        )
      ),
      fluidRow(
        box(
          title = "Data Table",
          width = 12,
          status = "info",
          solidHeader = TRUE,
          DTOutput("dataTable"),
          downloadButton("downloadData", "Download Displayed Data")
        )
      )
    ),
    
    # Meta-analysis tab
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
              selectInput("metaMethod", "Meta-Analysis Method:",
                         choices = c("Random Effects" = "random", 
                                    "Fixed Effects" = "fixed"))
            ),
            column(
              width = 4,
              checkboxGroupInput("metaGroupBy", "Group By:",
                               choices = c("Pathogen" = "pathogen", 
                                          "Antibiotic" = "antibiotic", 
                                          "Region" = "region"),
                               selected = c("pathogen", "antibiotic"))
            ),
            column(
              width = 4,
              actionButton("runMeta", "Run Meta-Analysis", 
                          icon = icon("play"), class = "btn-success")
            )
          )
        )
      ),
      fluidRow(
        tabBox(
          title = "Meta-Analysis Results",
          width = 12,
          tabPanel(
            title = "Summary",
            verbatimTextOutput("metaSummary")
          ),
          tabPanel(
            title = "Forest Plot",
            plotOutput("metaForestPlot", height = "800px")
          ),
          tabPanel(
            title = "Interactive Forest Plot",
            plotlyOutput("metaInteractiveForest", height = "800px")
          ),
          tabPanel(
            title = "Heterogeneity",
            verbatimTextOutput("heterogeneityResults"),
            plotOutput("heterogeneityPlot")
          ),
          tabPanel(
            title = "Data Table",
            DTOutput("metaResultsTable"),
            downloadButton("downloadMetaResults", "Download Results")
          )
        )
      )
    ),
    
    # Forest plot tab
    tabItem(
      tabName = "forest",
      fluidRow(
        box(
          title = "Forest Plot Settings",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          fluidRow(
            column(
              width = 3,
              selectInput("forestType", "Plot Type:",
                         choices = c("Standard Forest Plot" = "standard", 
                                    "Subgroup Forest Plot" = "subgroup", 
                                    "Cumulative Forest Plot" = "cumulative"))
            ),
            column(
              width = 3,
              textInput("forestTitle", "Plot Title:", 
                       value = "Forest Plot of Antimicrobial Resistance Rates")
            ),
            column(
              width = 3,
              textInput("forestXlab", "X-axis Label:", 
                       value = "Resistance Rate")
            ),
            column(
              width = 3,
              numericInput("forestTextSize", "Text Size:", 
                          value = 12, min = 8, max = 20)
            )
          ),
          fluidRow(
            column(
              width = 3,
              conditionalPanel(
                condition = "input.forestType == 'cumulative'",
                selectInput("forestOrderBy", "Order By:",
                           choices = c("Year" = "year", 
                                      "None" = "none"))
              )
            ),
            column(
              width = 3,
              checkboxInput("forestShowLabels", "Show Study Labels", value = TRUE)
            ),
            column(
              width = 3,
              checkboxInput("forestShowPrediction", "Show Prediction Interval", value = TRUE)
            ),
            column(
              width = 3,
              actionButton("createForest", "Create Forest Plot", 
                          icon = icon("chart-line"), class = "btn-success")
            )
          )
        )
      ),
      fluidRow(
        box(
          title = "Forest Plot",
          width = 12,
          status = "info",
          solidHeader = TRUE,
          plotOutput("forestPlot", height = "800px"),
          downloadButton("downloadForest", "Download Plot")
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
          h3("Meta-analysis Tool for Exploring antimicrobial resistance Outcomes across Realms"),
          p("Version: 0.1.0"),
          p("METEOR is a comprehensive R package for analyzing antimicrobial resistance (AMR) data across different domains: human, animal, and environment."),
          h4("Features:"),
          tags$ul(
            tags$li("Import and standardize AMR data from various sources"),
            tags$li("Perform meta-analyses of resistance rates"),
            tags$li("Create visualizations of AMR patterns"),
            tags$li("Generate reports and summaries"),
            tags$li("Compare data across different domains (One Health approach)")
          ),
          h4("Citation:"),
          p("If you use METEOR in your research, please cite:"),
          tags$blockquote(
            "Doe, J. (2023). METEOR: Meta-analysis Tool for Exploring antimicrobial resistance Outcomes across Realms. R package version 0.1.0."
          ),
          h4("Contact:"),
          p("For support or feedback, please contact: john.doe@example.com")
        )
      )
    )
  )
)

# Create UI
ui <- dashboardPage(
  header,
  sidebar,
  body,
  skin = "blue"
) 