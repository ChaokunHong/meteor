# UI for METEOR Shiny application
library(shiny)
library(shinydashboard)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "METEOR"),
  
  # Sidebar with navigation menu
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Import", tabName = "data", icon = icon("database")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  # Main content area
  dashboardBody(
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
            p("This application provides interactive tools for analyzing antimicrobial resistance (AMR) data."),
            p("Use the sidebar menu to navigate through different features of the application.")
          )
        )
      ),
      
      # Data Import tab
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Import Data",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            fileInput("upload_file", "Upload CSV file",
                      accept = c("text/csv", 
                                ".csv",
                                "text/comma-separated-values")),
            tableOutput("data_preview")
          )
        )
      ),
      
      # Analysis tab
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            title = "Meta-analysis Settings",
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            selectInput("analysis_method", "Method:", 
                        choices = c("Random Effects" = "random",
                                   "Fixed Effects" = "fixed")),
            numericInput("conf_level", "Confidence Level:", 
                         value = 0.95, min = 0.8, max = 0.99, step = 0.01),
            actionButton("run_analysis", "Run Analysis", 
                         class = "btn-primary")
          ),
          box(
            title = "Results",
            width = 8,
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("analysis_results")
          )
        )
      ),
      
      # Visualization tab
      tabItem(
        tabName = "visualization",
        fluidRow(
          box(
            title = "Plot Options",
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            selectInput("plot_type", "Plot Type:", 
                        choices = c("Forest Plot" = "forest",
                                   "Funnel Plot" = "funnel")),
            actionButton("create_plot", "Create Plot", 
                         class = "btn-primary")
          ),
          box(
            title = "Visualization",
            width = 8,
            status = "info",
            solidHeader = TRUE,
            plotOutput("visualization")
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
              "Hong, C. (2023). METEOR: Meta-analysis Tool for Exploring antimicrobial resistance Outcomes across Realms. R package version 0.1.0."
            ),
            h4("Contact:"),
            p("For support or feedback, please contact: chaokun.hong@gmail.com")
          )
        )
      )
    )
  )
) 