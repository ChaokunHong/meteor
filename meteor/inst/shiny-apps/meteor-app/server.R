# Server logic for the METEOR Shiny application

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(meta)
library(meteor)  # Load the METEOR package

server <- function(input, output, session) {
  
  # Reactive values to store data and analysis results
  values <- reactiveValues(
    raw_data = NULL,
    validated_data = NULL,
    standardized_data = NULL,
    meta_results = NULL,
    forest_plot = NULL
  )
  
  # Home tab functionality
  observeEvent(input$goto_import, {
    updateTabItems(session, "tabs", "import")
  })
  
  observeEvent(input$load_human, {
    # Load human data demo
    values$raw_data <- data.frame(
      # Create a sample dataset here or load from the package
      # This is a placeholder
      message("Loading human AMR dataset...")
      # In a real implementation, you would load data from:
      # values$raw_data <- meteor::human_amr_data
    )
    updateTabItems(session, "tabs", "view_data")
  })
  
  # Import data tab functionality
  observeEvent(input$importData, {
    req(input$dataFile)
    
    # Use the meteor import function
    tryCatch({
      values$raw_data <- meteor::import_amr_data(
        file = input$dataFile$datapath,
        format = tools::file_ext(input$dataFile$name)
      )
      
      # Show success message
      showNotification("Data imported successfully!", type = "message")
      
    }, error = function(e) {
      # Show error message
      showNotification(paste("Error importing data:", e$message), type = "error")
    })
  })
  
  observeEvent(input$loadSample, {
    # Load sample data based on selection
    domain <- input$sampleData
    
    if (domain == "human") {
      # Load sample human data
      # In a real implementation:
      # values$raw_data <- meteor::sample_human_data
      
      # For now, create a dummy dataset
      values$raw_data <- data.frame(
        `study ID` = paste0("study", 1:5),
        firstauthor_last = c("Smith", "Jones", "Lee", "Wang", "Kumar"),
        year_published = c(2018, 2019, 2020, 2021, 2022),
        location = c("USA", "UK", "China", "India", "Brazil"),
        population = c("Adults", "Children", "Adults", "Children", "Adults"),
        Environment = c("Hospital", "Community", "Hospital", "Community", "Hospital"),
        n_pop = c(100, 150, 200, 250, 300),
        r_CIP_Ecoil = c(0.25, 0.30, 0.35, 0.40, 0.45),
        n_CIP_Ecoil = c(25, 45, 70, 100, 135),
        d_CIP_Ecoil = c(100, 150, 200, 250, 300),
        stringsAsFactors = FALSE
      )
      
      showNotification("Sample human AMR data loaded", type = "message")
    } else {
      showNotification("This sample dataset is not yet available", type = "warning")
    }
  })
  
  # Preview of imported data
  output$dataPreview <- renderDT({
    req(values$raw_data)
    DT::datatable(
      values$raw_data,
      options = list(
        scrollX = TRUE,
        pageLength = 5,
        lengthMenu = c(5, 10, 15, 20)
      )
    )
  })
  
  # Template download
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste0("meteor_template_", input$dataType, ".csv")
    },
    content = function(file) {
      # Create a template based on the selected domain
      domain <- input$dataType
      
      template <- data.frame(
        `study ID` = character(0),
        firstauthor_last = character(0),
        year_published = integer(0),
        location = character(0),
        stringsAsFactors = FALSE
      )
      
      # Add domain-specific columns
      if (domain == "human") {
        template$population <- character(0)
        template$Environment <- character(0)
        template$n_pop <- integer(0)
        template$r_CIP_Ecoil <- numeric(0)
        template$n_CIP_Ecoil <- integer(0)
        template$d_CIP_Ecoil <- integer(0)
      } else if (domain == "animal") {
        template$animal_type <- character(0)
        template$sample_type <- character(0)
        template$r_CIP_Ecoil <- numeric(0)
        template$n_CIP_Ecoil <- integer(0)
        template$d_CIP_Ecoil <- integer(0)
      } else if (domain == "environment") {
        template$environment_category <- character(0)
        template$sample_type <- character(0)
        template$r_CIP_Ecoil <- numeric(0)
        template$n_CIP_Ecoil <- integer(0)
        template$d_CIP_Ecoil <- integer(0)
      }
      
      # Add an example row
      example <- list(
        `study ID` = "study1",
        firstauthor_last = "Smith",
        year_published = 2022,
        location = "USA"
      )
      
      if (domain == "human") {
        example$population <- "Adults"
        example$Environment <- "Hospital"
        example$n_pop <- 100
        example$r_CIP_Ecoil <- 0.25
        example$n_CIP_Ecoil <- 25
        example$d_CIP_Ecoil <- 100
      } else if (domain == "animal") {
        example$animal_type <- "Cattle"
        example$sample_type <- "Fecal"
        example$r_CIP_Ecoil <- 0.30
        example$n_CIP_Ecoil <- 30
        example$d_CIP_Ecoil <- 100
      } else if (domain == "environment") {
        example$environment_category <- "Water"
        example$sample_type <- "River"
        example$r_CIP_Ecoil <- 0.20
        example$n_CIP_Ecoil <- 20
        example$d_CIP_Ecoil <- 100
      }
      
      # Add example row to template
      template <- rbind(template, example)
      
      # Write template to file
      write.csv(template, file, row.names = FALSE)
    }
  )
  
  # Data view tab functionality
  observe({
    # Validate data when raw data is available
    req(values$raw_data)
    
    if (is.null(values$validated_data)) {
      # Determine domain
      domain <- "human"  # Default to human
      
      # Try to validate the data
      tryCatch({
        values$validated_data <- meteor::validate_data(values$raw_data, domain = domain, strict = FALSE)
        values$standardized_data <- meteor::standardize_amr_data(values$validated_data, domain = domain)
      }, error = function(e) {
        # If validation fails, show error but don't stop execution
        showNotification(paste("Validation error:", e$message), type = "warning")
      })
    }
    
    # Update filter dropdowns for standardized data
    if (!is.null(values$standardized_data)) {
      # Get unique values for filters
      pathogens <- c("All" = "", unique(values$standardized_data$pathogen))
      antibiotics <- c("All" = "", unique(values$standardized_data$antibiotic))
      regions <- c("All" = "", unique(values$standardized_data$region))
      
      # Update dropdown options
      updateSelectInput(session, "filterPathogen", choices = pathogens)
      updateSelectInput(session, "filterAntibiotic", choices = antibiotics)
      updateSelectInput(session, "filterRegion", choices = regions)
    }
  })
  
  # Data table display
  output$dataTable <- renderDT({
    # Get selected dataset
    data_set <- input$dataSet
    
    if (data_set == "raw") {
      data <- values$raw_data
    } else if (data_set == "validated") {
      data <- values$validated_data
    } else if (data_set == "standardized") {
      data <- values$standardized_data
      
      # Apply filters
      if (!is.null(input$filterPathogen) && input$filterPathogen != "") {
        data <- data[data$pathogen == input$filterPathogen, ]
      }
      if (!is.null(input$filterAntibiotic) && input$filterAntibiotic != "") {
        data <- data[data$antibiotic == input$filterAntibiotic, ]
      }
      if (!is.null(input$filterRegion) && input$filterRegion != "") {
        data <- data[data$region == input$filterRegion, ]
      }
    }
    
    req(data)
    
    DT::datatable(
      data,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100)
      )
    )
  })
  
  # Download displayed data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("meteor_", input$dataSet, "_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      # Get the currently displayed data
      data_set <- input$dataSet
      
      if (data_set == "raw") {
        data <- values$raw_data
      } else if (data_set == "validated") {
        data <- values$validated_data
      } else if (data_set == "standardized") {
        data <- values$standardized_data
        
        # Apply filters
        if (!is.null(input$filterPathogen) && input$filterPathogen != "") {
          data <- data[data$pathogen == input$filterPathogen, ]
        }
        if (!is.null(input$filterAntibiotic) && input$filterAntibiotic != "") {
          data <- data[data$antibiotic == input$filterAntibiotic, ]
        }
        if (!is.null(input$filterRegion) && input$filterRegion != "") {
          data <- data[data$region == input$filterRegion, ]
        }
      }
      
      # Write data to file
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Meta-analysis tab functionality
  observeEvent(input$runMeta, {
    req(values$standardized_data)
    
    # Get analysis parameters
    method <- input$metaMethod
    by_vars <- input$metaGroupBy
    
    # Check if any grouping variables are selected
    if (length(by_vars) == 0) {
      showNotification("Please select at least one grouping variable", type = "warning")
      return()
    }
    
    # Run meta-analysis
    tryCatch({
      values$meta_results <- meteor::calculate_pooled_rate(
        data = values$standardized_data,
        by = by_vars,
        method = method
      )
      
      # Show success message
      showNotification("Meta-analysis completed successfully!", type = "message")
      
    }, error = function(e) {
      # Show error message
      showNotification(paste("Error in meta-analysis:", e$message), type = "error")
    })
  })
  
  # Meta-analysis summary output
  output$metaSummary <- renderPrint({
    req(values$meta_results)
    
    # Print summary of meta-analysis results
    if (!is.null(values$meta_results$overall)) {
      summary(values$meta_results$overall)
    } else {
      cat("No overall meta-analysis results available.\n")
    }
  })
  
  # Forest plot for meta-analysis
  output$metaForestPlot <- renderPlot({
    req(values$meta_results)
    
    # Create forest plot
    meteor::create_forest_plot(
      meta_result = values$meta_results,
      title = "Forest Plot of Antimicrobial Resistance Rates",
      xlab = "Resistance Rate",
      show_study_labels = TRUE
    )
  })
  
  # Interactive forest plot
  output$metaInteractiveForest <- renderPlotly({
    req(values$meta_results)
    
    # Create interactive forest plot
    meteor::create_interactive_forest(
      meta_result = values$meta_results,
      title = "Interactive Forest Plot of Antimicrobial Resistance Rates",
      xlab = "Resistance Rate",
      sort_by = "effect"
    )
  })
  
  # Heterogeneity results
  output$heterogeneityResults <- renderPrint({
    req(values$meta_results)
    
    # Analyze heterogeneity
    heterogeneity <- meteor::analyze_heterogeneity(values$meta_results, detailed = TRUE)
    print(heterogeneity)
  })
  
  # Visualization tab functionality - Forest Plot
  observeEvent(input$createForest, {
    req(values$meta_results)
    
    # Get plot parameters
    plot_type <- input$forestType
    title <- input$forestTitle
    xlab <- input$forestXlab
    text_size <- input$forestTextSize
    show_labels <- input$forestShowLabels
    
    # Create the appropriate forest plot
    tryCatch({
      if (plot_type == "standard") {
        values$forest_plot <- meteor::create_forest_plot(
          meta_result = values$meta_results,
          title = title,
          xlab = xlab,
          show_study_labels = show_labels,
          text_size = text_size
        )
      } else if (plot_type == "cumulative") {
        values$forest_plot <- meteor::create_cumulative_forest(
          meta_result = values$meta_results,
          order_by = input$forestOrderBy,
          title = title,
          xlab = xlab
        )
      } else if (plot_type == "subgroup") {
        # For subgroup plot, we need subgroup analysis results
        # In a real implementation, you would check if subgroup results exist
        # or run the subgroup analysis first
        showNotification("Subgroup forest plot requires subgroup analysis results", type = "warning")
      }
      
      # Show success message
      showNotification("Forest plot created successfully!", type = "message")
      
    }, error = function(e) {
      # Show error message
      showNotification(paste("Error creating forest plot:", e$message), type = "error")
    })
  })
  
  # Forest plot output
  output$forestPlot <- renderPlot({
    req(values$forest_plot)
    
    # Display the stored forest plot
    # Note: meta forest plots are usually drawn directly rather than returned
    # So this might need to be adjusted based on how meta::forest works
    values$forest_plot
  })
  
  # Forest plot download
  output$downloadForest <- downloadHandler(
    filename = function() {
      paste0("meteor_forest_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      # Save the current forest plot
      png(file, width = 10, height = 8, units = "in", res = 300)
      meteor::create_forest_plot(
        meta_result = values$meta_results,
        title = input$forestTitle,
        xlab = input$forestXlab,
        show_study_labels = input$forestShowLabels,
        text_size = input$forestTextSize
      )
      dev.off()
    }
  )
} 