# Server logic for the meteor Shiny application

library(shiny)
library(DT)
library(plotly)
library(ggplot2)
library(shinydashboard)

# Server logic
server <- function(input, output, session) {
  
  # Load data
  data <- reactive({
    # Try to get data from meteor package options
    data <- getOption("meteor.shiny_data")
    
    # If no data found, load the human AMR dataset
    if (is.null(data)) {
      data(human_amr, package = "meteor", envir = environment())
      data <- human_amr
    }
    
    return(data)
  })
  
  # Initialize dropdowns once data is loaded
  observe({
    # Get unique values from data
    pathogens <- sort(unique(data()$pathogen))
    antibiotics <- sort(unique(data()$antibiotic))
    locations <- sort(unique(data()$location))
    populations <- sort(unique(data()$population))
    study_types <- sort(unique(data()$study_type))
    
    # Update select inputs
    updateSelectInput(session, "explorer_pathogen", choices = pathogens)
    updateSelectInput(session, "explorer_antibiotic", choices = antibiotics)
    updateSelectInput(session, "explorer_location", choices = locations)
    updateSelectInput(session, "explorer_population", choices = populations)
    updateSelectInput(session, "explorer_study_type", choices = study_types)
  })
  
  # Dashboard value boxes
  output$total_studies_box <- renderValueBox({
    n_studies <- length(unique(data()$study_id))
    valueBox(
      n_studies,
      "Studies",
      icon = icon("book"),
      color = "blue"
    )
  })
  
  output$total_datapoints_box <- renderValueBox({
    n_points <- nrow(data())
    valueBox(
      n_points,
      "Data Points",
      icon = icon("database"),
      color = "green"
    )
  })
  
  output$avg_resistance_box <- renderValueBox({
    avg_resistance <- mean(data()$resistance_rate, na.rm = TRUE)
    valueBox(
      paste0(round(avg_resistance * 100, 1), "%"),
      "Average Resistance Rate",
      icon = icon("percentage"),
      color = "red"
    )
  })
  
  # Dashboard plots
  output$dashboard_pathogen_plot <- renderPlotly({
    # Summarize resistance by pathogen
    pathogen_summary <- aggregate(
      data()$resistance_rate,
      by = list(pathogen = data()$pathogen),
      FUN = mean,
      na.rm = TRUE
    )
    colnames(pathogen_summary)[2] <- "resistance_rate"
    
    # Count samples per pathogen
    pathogen_count <- aggregate(
      data()$resistance_rate,
      by = list(pathogen = data()$pathogen),
      FUN = length
    )
    colnames(pathogen_count)[2] <- "count"
    
    # Merge summary and count
    pathogen_data <- merge(pathogen_summary, pathogen_count)
    
    # Sort by resistance rate
    pathogen_data <- pathogen_data[order(pathogen_data$resistance_rate, decreasing = TRUE), ]
    
    # Create bar plot
    p <- ggplot(pathogen_data, aes(x = reorder(pathogen, resistance_rate), y = resistance_rate, text = paste0(
      "Pathogen: ", pathogen, "<br>",
      "Resistance Rate: ", scales::percent(resistance_rate, accuracy = 0.1), "<br>",
      "Sample Size: ", count
    ))) +
      geom_bar(stat = "identity", aes(fill = resistance_rate)) +
      scale_fill_gradient(low = "green", high = "red") +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      labs(x = NULL, y = "Resistance Rate") +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$dashboard_antibiotic_plot <- renderPlotly({
    # Summarize resistance by antibiotic
    antibiotic_summary <- aggregate(
      data()$resistance_rate,
      by = list(antibiotic = data()$antibiotic),
      FUN = mean,
      na.rm = TRUE
    )
    colnames(antibiotic_summary)[2] <- "resistance_rate"
    
    # Count samples per antibiotic
    antibiotic_count <- aggregate(
      data()$resistance_rate,
      by = list(antibiotic = data()$antibiotic),
      FUN = length
    )
    colnames(antibiotic_count)[2] <- "count"
    
    # Merge summary and count
    antibiotic_data <- merge(antibiotic_summary, antibiotic_count)
    
    # Sort by resistance rate
    antibiotic_data <- antibiotic_data[order(antibiotic_data$resistance_rate, decreasing = TRUE), ]
    
    # Take top 10 for readability
    if (nrow(antibiotic_data) > 10) {
      antibiotic_data <- antibiotic_data[1:10, ]
    }
    
    # Create bar plot
    p <- ggplot(antibiotic_data, aes(x = reorder(antibiotic, resistance_rate), y = resistance_rate, text = paste0(
      "Antibiotic: ", antibiotic, "<br>",
      "Resistance Rate: ", scales::percent(resistance_rate, accuracy = 0.1), "<br>",
      "Sample Size: ", count
    ))) +
      geom_bar(stat = "identity", aes(fill = resistance_rate)) +
      scale_fill_gradient(low = "green", high = "red") +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      labs(x = NULL, y = "Resistance Rate") +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$dashboard_geo_plot <- renderPlotly({
    # Summarize resistance by location
    geo_summary <- aggregate(
      data()$resistance_rate,
      by = list(location = data()$location),
      FUN = mean,
      na.rm = TRUE
    )
    colnames(geo_summary)[2] <- "resistance_rate"
    
    # Count samples per location
    geo_count <- aggregate(
      data()$resistance_rate,
      by = list(location = data()$location),
      FUN = length
    )
    colnames(geo_count)[2] <- "count"
    
    # Merge summary and count
    geo_data <- merge(geo_summary, geo_count)
    
    # Sort by location name
    geo_data <- geo_data[order(geo_data$location), ]
    
    # Create plot
    p <- ggplot(geo_data, aes(x = location, y = resistance_rate, text = paste0(
      "Location: ", location, "<br>",
      "Resistance Rate: ", scales::percent(resistance_rate, accuracy = 0.1), "<br>",
      "Sample Size: ", count
    ))) +
      geom_bar(stat = "identity", aes(fill = resistance_rate)) +
      scale_fill_gradient(low = "green", high = "red") +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      labs(x = "Location", y = "Resistance Rate") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Explorer filtered data
  filtered_data <- eventReactive(input$explorer_filter, {
    df <- data()
    
    # Apply filters
    if (!is.null(input$explorer_pathogen) && length(input$explorer_pathogen) > 0) {
      df <- df[df$pathogen %in% input$explorer_pathogen, ]
    }
    
    if (!is.null(input$explorer_antibiotic) && length(input$explorer_antibiotic) > 0) {
      df <- df[df$antibiotic %in% input$explorer_antibiotic, ]
    }
    
    if (!is.null(input$explorer_location) && length(input$explorer_location) > 0) {
      df <- df[df$location %in% input$explorer_location, ]
    }
    
    if (!is.null(input$explorer_population) && length(input$explorer_population) > 0) {
      df <- df[df$population %in% input$explorer_population, ]
    }
    
    if (!is.null(input$explorer_study_type) && length(input$explorer_study_type) > 0) {
      df <- df[df$study_type %in% input$explorer_study_type, ]
    }
    
    if (!is.null(input$explorer_year_range)) {
      df <- df[df$year_published >= input$explorer_year_range[1] & 
               df$year_published <= input$explorer_year_range[2], ]
    }
    
    if (!is.null(input$explorer_resistance_range)) {
      df <- df[df$resistance_rate >= input$explorer_resistance_range[1] & 
               df$resistance_rate <= input$explorer_resistance_range[2], ]
    }
    
    return(df)
  }, ignoreNULL = FALSE)
  
  # Explorer data table
  output$explorer_table <- renderDT({
    df <- filtered_data()
    
    # Format resistance rate as percentage
    df$resistance_rate <- scales::percent(df$resistance_rate, accuracy = 0.1)
    
    datatable(
      df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        searching = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Explorer summary plot
  output$explorer_summary_plot <- renderPlotly({
    df <- filtered_data()
    
    # Summarize resistance by pathogen and antibiotic
    summary_data <- aggregate(
      df$resistance_rate,
      by = list(pathogen = df$pathogen, antibiotic = df$antibiotic),
      FUN = mean,
      na.rm = TRUE
    )
    colnames(summary_data)[3] <- "resistance_rate"
    
    # Count samples
    count_data <- aggregate(
      df$resistance_rate,
      by = list(pathogen = df$pathogen, antibiotic = df$antibiotic),
      FUN = length
    )
    colnames(count_data)[3] <- "count"
    
    # Merge
    plot_data <- merge(summary_data, count_data)
    
    # Create heatmap
    p <- ggplot(plot_data, aes(x = antibiotic, y = pathogen, fill = resistance_rate, text = paste0(
      "Pathogen: ", pathogen, "<br>",
      "Antibiotic: ", antibiotic, "<br>",
      "Resistance Rate: ", scales::percent(resistance_rate, accuracy = 0.1), "<br>",
      "Sample Size: ", count
    ))) +
      geom_tile() +
      scale_fill_gradient(low = "green", high = "red", limits = c(0, 1), labels = scales::percent) +
      labs(fill = "Resistance Rate") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Meta-analysis results
  meta_results <- eventReactive(input$meta_run, {
    req(input$meta_group_by)
    
    # Run meta-analysis
    withProgress(message = "Running meta-analysis...", {
      results <- calculate_pooled_rate(
        data = filtered_data(),
        by = input$meta_group_by,
        method = input$meta_method
      )
    })
    
    return(results)
  })
  
  # Meta-analysis forest plot
  output$meta_forest_plot <- renderPlotly({
    req(meta_results())
    
    create_interactive_forest(meta_results())
  })
  
  # Meta-analysis summary table
  output$meta_summary_table <- renderDT({
    req(meta_results())
    
    summary_data <- meta_results()$summary
    
    # Format percentages
    summary_data$pooled_rate <- scales::percent(summary_data$pooled_rate, accuracy = 0.1)
    summary_data$ci_lower <- scales::percent(summary_data$ci_lower, accuracy = 0.1)
    summary_data$ci_upper <- scales::percent(summary_data$ci_upper, accuracy = 0.1)
    
    # Create CI column
    summary_data$ci <- paste0(summary_data$ci_lower, " - ", summary_data$ci_upper)
    
    # Select and reorder columns
    display_data <- summary_data[, c("group", "k", "pooled_rate", "ci", "I2")]
    colnames(display_data) <- c("Group", "Studies", "Pooled Rate", "95% CI", "I²")
    
    datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Meta-analysis heterogeneity output
  output$meta_heterogeneity <- renderPrint({
    req(meta_results())
    
    # Get heterogeneity statistics
    cat("Method:", meta_results()$method, "\n\n")
    
    if (!is.null(meta_results()$overall)) {
      cat("Overall Heterogeneity:\n")
      cat("I² =", round(meta_results()$overall$heterogeneity["I2"], 2), "%\n")
      cat("H² =", round(meta_results()$overall$heterogeneity["H2"], 2), "\n")
      cat("τ² =", round(meta_results()$overall$heterogeneity["tau2"], 4), "\n\n")
    }
    
    if (!is.null(meta_results()$by_group)) {
      cat("Group Heterogeneity Summary:\n")
      
      # Extract I2 values and create summary
      i2_values <- sapply(meta_results()$by_group, function(x) {
        if (is.list(x$heterogeneity)) {
          return(x$heterogeneity["I2"])
        } else {
          return(NA)
        }
      })
      
      cat("Mean I² =", round(mean(i2_values, na.rm = TRUE), 2), "%\n")
      cat("Median I² =", round(median(i2_values, na.rm = TRUE), 2), "%\n")
      cat("Min I² =", round(min(i2_values, na.rm = TRUE), 2), "%\n")
      cat("Max I² =", round(max(i2_values, na.rm = TRUE), 2), "%\n")
    }
  })
  
  # Visualization options UI
  output$viz_options_1 <- renderUI({
    req(input$viz_type)
    
    switch(input$viz_type,
           "forest" = selectInput("viz_forest_groups", "Groups to Include", 
                                choices = NULL, multiple = TRUE),
           "heatmap" = selectInput("viz_heatmap_x", "X-axis Variable", 
                                 choices = c("pathogen", "antibiotic", "location")),
           "geo" = selectInput("viz_geo_group", "Group By", 
                             choices = c("pathogen", "antibiotic"), multiple = TRUE),
           "trend" = selectInput("viz_trend_group", "Group By", 
                               choices = c("pathogen", "antibiotic", "location"), multiple = TRUE),
           "comparison" = selectInput("viz_compare_var", "Compare Variable", 
                                   choices = c("location", "population", "study_type"))
    )
  })
  
  output$viz_options_2 <- renderUI({
    req(input$viz_type)
    
    switch(input$viz_type,
           "forest" = selectInput("viz_forest_sort", "Sort By", 
                                choices = c("Effect Size" = "effect", "Name" = "name", "Precision" = "precision")),
           "heatmap" = selectInput("viz_heatmap_y", "Y-axis Variable", 
                                 choices = c("antibiotic", "pathogen", "location")),
           "geo" = selectInput("viz_geo_color", "Color Palette", 
                             choices = c("viridis", "magma", "plasma", "inferno")),
           "trend" = selectInput("viz_trend_time", "Time Variable", 
                               choices = c("year_published")),
           "comparison" = selectInput("viz_compare_group", "Group Variable", 
                                   choices = c("pathogen", "antibiotic"))
    )
  })
  
  # Update visualization options based on data
  observe({
    req(meta_results())
    
    # Update forest plot groups
    if (input$viz_type == "forest" && !is.null(meta_results()$summary)) {
      groups <- meta_results()$summary$group
      updateSelectInput(session, "viz_forest_groups", choices = groups)
    }
  })
  
  # Create visualization
  viz_plot_data <- eventReactive(input$viz_create, {
    req(input$viz_type)
    
    withProgress(message = "Creating visualization...", {
      # Create different types of visualizations
      switch(input$viz_type,
             "forest" = {
               req(meta_results())
               create_interactive_forest(
                 meta_results(),
                 groups = input$viz_forest_groups,
                 sort_by = input$viz_forest_sort
               )
             },
             "heatmap" = {
               req(input$viz_heatmap_x, input$viz_heatmap_y)
               create_resistance_heatmap(
                 data = filtered_data(),
                 x_var = input$viz_heatmap_x,
                 y_var = input$viz_heatmap_y,
                 interactive = TRUE
               )
             },
             "geo" = {
               create_geo_map(
                 data = filtered_data(),
                 by = input$viz_geo_group,
                 color_palette = input$viz_geo_color
               )
             },
             "trend" = {
               create_trend_plot(
                 data = filtered_data(),
                 by = input$viz_trend_group,
                 time_var = input$viz_trend_time,
                 interactive = TRUE
               )
             },
             "comparison" = {
               create_comparison_plot(
                 data = filtered_data(),
                 compare_var = input$viz_compare_var,
                 group_var = input$viz_compare_group,
                 interactive = TRUE
               )
             }
      )
    })
  })
  
  # Render visualization
  output$viz_plot <- renderPlotly({
    viz_plot_data()
  })
  
  # Compare tab - column mapping UI
  output$compare_mapping <- renderUI({
    req(input$compare_file)
    
    # Read header to get column names
    ext <- tools::file_ext(input$compare_file$name)
    
    if (ext == "csv") {
      header <- names(read.csv(input$compare_file$datapath, nrows = 1))
    } else if (ext %in% c("xls", "xlsx")) {
      header <- names(readxl::read_excel(input$compare_file$datapath, n_max = 1))
    } else {
      return(NULL)
    }
    
    # Create mapping UI
    fluidRow(
      column(
        width = 3,
        selectInput("map_pathogen", "Pathogen Column", choices = c("", header))
      ),
      column(
        width = 3,
        selectInput("map_antibiotic", "Antibiotic Column", choices = c("", header))
      ),
      column(
        width = 3,
        selectInput("map_resistance", "Resistance Rate Column", choices = c("", header))
      ),
      column(
        width = 3,
        selectInput("map_sample_size", "Sample Size Column", choices = c("", header))
      )
    )
  })
  
  # Compare tab - imported local data
  local_data <- reactive({
    req(input$compare_file, input$map_pathogen, input$map_antibiotic, input$map_resistance)
    
    # Read data
    ext <- tools::file_ext(input$compare_file$name)
    
    if (ext == "csv") {
      raw_data <- read.csv(input$compare_file$datapath, header = input$compare_header)
    } else if (ext %in% c("xls", "xlsx")) {
      raw_data <- readxl::read_excel(input$compare_file$datapath)
    } else {
      return(NULL)
    }
    
    # Create mapping
    mapping <- list()
    mapping[[input$map_pathogen]] <- "pathogen"
    mapping[[input$map_antibiotic]] <- "antibiotic"
    mapping[[input$map_resistance]] <- "resistance_rate"
    
    if (input$map_sample_size != "") {
      mapping[[input$map_sample_size]] <- "sample_size_ab"
    }
    
    # Use import_local_data to standardize
    std_data <- import_local_data(
      file_path = input$compare_file$datapath,
      format = ext,
      mapping = mapping
    )
    
    return(std_data)
  })
  
  # Compare tab - run comparison
  comparison_results <- eventReactive(input$compare_run, {
    req(local_data(), meta_results(), input$compare_group_by)
    
    # Run comparison
    withProgress(message = "Running comparison...", {
      compare_with_meta(
        local_data = local_data(),
        meta_results = meta_results(),
        by = input$compare_group_by
      )
    })
  })
  
  # Compare tab - plot
  output$compare_plot <- renderPlotly({
    req(comparison_results())
    
    # Create comparison plot
    create_comparison_plot(
      data = comparison_results(),
      compare_var = "direction",
      group_var = input$compare_group_by[1],
      interactive = TRUE
    )
  })
  
  # Compare tab - table
  output$compare_table <- renderDT({
    req(comparison_results())
    
    # Format summary table
    summary <- comparison_results()$summary
    
    # Format percentages
    summary$local_rate <- scales::percent(summary$local_rate, accuracy = 0.1)
    summary$meta_rate <- scales::percent(summary$meta_rate, accuracy = 0.1)
    summary$absolute_diff <- scales::percent(summary$absolute_diff, accuracy = 0.1)
    
    # Round percentage difference
    summary$percentage_diff <- round(summary$percentage_diff, 1)
    
    # Format p-values
    summary$p_value <- format.pval(summary$p_value, digits = 3)
    
    # Reorder and rename columns
    display_cols <- c(input$compare_group_by, "local_rate", "meta_rate", 
                     "absolute_diff", "percentage_diff", "direction", "p_value", "significant")
    display_data <- summary[, display_cols]
    
    colnames(display_data) <- c(
      input$compare_group_by,
      "Local Rate", "Meta Rate", "Absolute Diff", "% Diff", 
      "Direction", "p-value", "Significant"
    )
    
    datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Compare tab - stats
  output$compare_stats <- renderPrint({
    req(comparison_results())
    
    # Summary statistics
    summary <- comparison_results()$summary
    
    cat("Comparison Summary\n")
    cat("------------------\n\n")
    
    cat("Number of comparisons:", nrow(summary), "\n\n")
    
    # Count significantly different results
    sig_diff <- sum(summary$significant, na.rm = TRUE)
    cat("Significantly different:", sig_diff, "of", nrow(summary), 
        "(", round(sig_diff/nrow(summary)*100, 1), "%)\n")
    
    # Direction of differences
    higher <- sum(summary$direction == "higher", na.rm = TRUE)
    lower <- sum(summary$direction == "lower", na.rm = TRUE)
    same <- sum(summary$direction == "same", na.rm = TRUE)
    
    cat("Higher than meta-analysis:", higher, "\n")
    cat("Lower than meta-analysis:", lower, "\n")
    cat("Same as meta-analysis:", same, "\n\n")
    
    # Average difference
    cat("Average absolute difference:", 
        scales::percent(mean(abs(summary$absolute_diff), na.rm = TRUE), accuracy = 0.1), "\n")
    cat("Average percentage difference:", 
        round(mean(abs(summary$percentage_diff), na.rm = TRUE), 1), "%\n")
  })
  
  # About tab - version
  output$about_version <- renderText({
    meteor_version()
  })
} 