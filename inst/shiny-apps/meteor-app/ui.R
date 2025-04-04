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