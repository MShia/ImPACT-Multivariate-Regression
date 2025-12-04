################################################################################
# Shiny App: Cognitive Scores Tolerance Analysis
# Based on Multivariate Regression with Personalized Cutoffs
# + Univariate Regression Percentiles
# Modified to accept custom data file uploads
################################################################################

library(shiny)
library(tolerance)
library(DT)

################################################################################
# Helper Functions (moved to top for better organization)
################################################################################

# Iverson Classification Function
classify_iverson <- function(percentiles) {
  # percentiles should be a vector of 4 percentile values
  
  # Count how many scores fall below each threshold
  count_below_25 <- sum(percentiles <= 25)
  count_below_16 <- sum(percentiles <= 16)
  count_below_10 <- sum(percentiles <= 10)
  count_below_5 <- sum(percentiles <= 5)
  count_below_2 <- sum(percentiles <= 2)
  
  # Apply Iverson classification rules (check from most severe to least severe)
  if (count_below_5 >= 3 || count_below_2 >= 2) {
    return("Extremely Low")
  } else if (count_below_10 >= 3 || count_below_5 >= 2 || count_below_2 >= 1) {
    return("Unusually Low")
  } else if (count_below_16 >= 3 || count_below_10 >= 2 || count_below_5 >= 1) {
    return("Well Below Average")
  } else if (count_below_25 >= 3 || count_below_16 >= 2 || count_below_10 >= 1) {
    return("Below Average")
  } else {
    return("Broadly Normal")
  }
}

# Define depth function
mahal_depth <- function(pts, x) {
  if (is.vector(pts)) pts <- matrix(pts, nrow = 1)
  center <- colMeans(x)
  cov_mat <- cov(x)
  diag(cov_mat) <- diag(cov_mat) + 1e-6
  depths <- apply(pts, 1, function(pt) {
    tryCatch(-mahalanobis(pt, center, cov_mat), 
             error = function(e) -sum((pt - center)^2))
  })
  return(depths)
}

# Function to process and validate data
process_data <- function(file_path) {
  tryCatch({
    # Load data
    data <- read.csv(file_path, stringsAsFactors = FALSE)
    
    # Check required columns
    required_cols <- c('age', 'sex', 'education', 'visual_memory', 
                       'visual_motor_speed', 'verbal_memory', 'reaction_speed')
    
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      return(list(
        success = FALSE,
        message = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
      ))
    }
    
    # Create derived columns
    data$sex_numeric <- ifelse(tolower(data$sex) == 'm', 0, 1)
    data$reaction_speed_flipped <- -data$reaction_speed
    
    # Remove NA values
    data <- na.omit(data)
    
    # Filter data: Age - Education must be >= 5
    valid_indices <- (data$age - data$education) >= 5
    data <- data[valid_indices, ]
    
    if (nrow(data) < 10) {
      return(list(
        success = FALSE,
        message = "Insufficient valid data rows (need at least 10 subjects)"
      ))
    }
    
    return(list(
      success = TRUE,
      data = data,
      message = sprintf("Successfully loaded %d subjects", nrow(data))
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Error reading file:", e$message)
    ))
  })
}

################################################################################
# UI
################################################################################

ui <- fluidPage(
  titlePanel("Jockey-Specific ImPACT Normative Tool Using Irish Horseracing Data (2021 - 2024)"),
  
  tags$head(tags$style(HTML("
    .atypical-cell { background-color: #ffcccc !important; font-weight: bold; }
    .typical-cell { background-color: #ccffcc !important; }
    .result-box { 
      padding: 20px; 
      margin: 20px 0; 
      border-radius: 10px; 
      border: 2px solid #ddd;
    }
    .atypical-result { background-color: #ffe6e6; border-color: #ff6666; }
    .typical-result { background-color: #e6ffe6; border-color: #66ff66; }
    .error-message { 
      color: #cc0000; 
      font-weight: bold; 
      padding: 10px; 
      background-color: #ffe6e6; 
      border: 2px solid #cc0000; 
      border-radius: 5px; 
      margin-top: 10px;
    }
    .success-message {
      color: #009900;
      font-weight: bold;
      padding: 10px;
      background-color: #e6ffe6;
      border: 2px solid #009900;
      border-radius: 5px;
      margin-top: 10px;
    }
    .info-icon {
      cursor: help;
      color: #0066cc;
      margin-left: 5px;
      font-size: 14px;
    }
    .tooltip-content {
      position: relative;
      display: inline-block;
    }
    .tooltip-content .tooltip-text {
      visibility: hidden;
      width: 400px;
      background-color: #333;
      color: #fff;
      text-align: left;
      border-radius: 6px;
      padding: 15px;
      position: absolute;
      z-index: 1000;
      top: 125%;
      left: 50%;
      margin-left: -200px;
      opacity: 0;
      transition: opacity 0.3s;
      font-size: 12px;
      line-height: 1.6;
      box-shadow: 0px 4px 8px rgba(0,0,0,0.3);
    }
    .tooltip-content .tooltip-text::after {
      content: '';
      position: absolute;
      bottom: 100%;
      left: 50%;
      margin-left: -5px;
      border-width: 5px;
      border-style: solid;
      border-color: transparent transparent #333 transparent;
    }
    .tooltip-content:hover .tooltip-text {
      visibility: visible;
      opacity: 1;
    }
    .tooltip-text strong {
      color: #66ccff;
      display: block;
      margin-top: 8px;
      margin-bottom: 3px;
    }
    .tooltip-text strong:first-child {
      margin-top: 0;
    }
    .upload-section {
      background-color: #f0f8ff;
      padding: 20px;
      border-radius: 10px;
      border: 2px solid #0066cc;
      margin-bottom: 20px;
    }
  "))),
  
  # File Upload Section
  fluidRow(
    column(12,
      div(class = "upload-section",
        h4(icon("upload"), " Data Upload"),
        p("Upload a CSV file containing cognitive assessment data with the following columns:",
          br(), strong("Required: "), "age, sex, education, visual_memory, visual_motor_speed, verbal_memory, reaction_speed"),
        fileInput("data_file", 
                  "Choose CSV File",
                  accept = c("text/csv", "text/comma-separated-values", ".csv"),
                  placeholder = "No file selected"),
        uiOutput("data_status"),
        conditionalPanel(
          condition = "output.data_loaded",
          actionButton("use_sample", "Use Sample Data Instead", 
                      class = "btn-info",
                      style = "margin-top: 10px;")
        )
      )
    )
  ),
  
  # Main content - only show when data is loaded
  conditionalPanel(
    condition = "output.data_loaded",
    fluidRow(
      # Left panel: Input
      column(4,
        wellPanel(
          h4("Tolerance Region Parameters"),
          
          sliderInput("alpha", 
                     "Confidence Level (1 - α):",
                     min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          
          sliderInput("P", 
                     "Population Coverage (P):",
                     min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          
          actionButton("update_tolerance", "Update Tolerance Region", 
                      class = "btn-warning btn-block",
                      style = "margin-top: 10px; margin-bottom: 20px;"),
          
          hr(),
          
          h4("New Subject Assessment"),
          
          uiOutput("dynamic_inputs")
        )
      ),
      
      # Right panel: Results
      column(8,
        # New subject results
        uiOutput("new_subject_results"),
        
        hr(),
        
        # Full dataset table
        h4("Complete Dataset Analysis"),
        p("Table shows all subjects with observed cognitive scores and classification results from both methods. ",
          strong("Tolerance Bounds: ", style = "color: #0066cc;"), 
          strong("Red = OUTSIDE", style = "color: #cc0000;"), " (at least one score below personalized cutoff), ",
          strong("Green = INSIDE", style = "color: #009900;"), " (all scores at/above cutoff). ",
          strong("Iverson Classification", style = "color: #6600cc;"), " ranges from ", 
          strong("Broadly Normal", style = "color: #009900;"), " to ",
          strong("Extremely Low", style = "color: #cc0000;"), " based on percentile thresholds."),
        
        downloadButton("download_table", "Download Complete Table (CSV)", 
                       class = "btn-success",
                       style = "margin-bottom: 15px;"),
        
        DTOutput("full_table")
      )
    )
  )
)

################################################################################
# Server
################################################################################

server <- function(input, output, session) {
  
  # Reactive values to store all analysis results
  analysis_data <- reactiveValues(
    data = NULL,
    data_loaded = FALSE,
    mvreg = NULL,
    predictions = NULL,
    residuals_matrix = NULL,
    univariate_models = NULL,
    residual_lower_cutoffs = NULL,
    personalized_cutoffs = NULL,
    atypical_flags = NULL,
    iverson_classification = NULL,
    data_ranges = NULL,
    data_medians = NULL,
    alpha = 0.05,
    P = 0.95
  )
  
  # Handle file upload
  observeEvent(input$data_file, {
    req(input$data_file)
    
    showNotification("Processing uploaded file...", type = "message", duration = 2)
    
    result <- process_data(input$data_file$datapath)
    
    if (result$success) {
      perform_analysis(result$data)
      showNotification(result$message, type = "message", duration = 3)
    } else {
      showNotification(result$message, type = "error", duration = 5)
    }
  })
  
  # Handle use sample data button
  observeEvent(input$use_sample, {
    # Check if sample data exists
    if (file.exists('data/impact_phi_reading.csv')) {
      result <- process_data('data/impact_phi_reading.csv')
      if (result$success) {
        perform_analysis(result$data)
        showNotification("Loaded sample data successfully", type = "message", duration = 3)
      }
    } else {
      showNotification("Sample data file not found", type = "error", duration = 3)
    }
  })
  
  # Function to perform complete analysis
  perform_analysis <- function(data) {
    
    # Store data ranges and medians
    analysis_data$data_ranges <- list(
      visual_memory = c(min(data$visual_memory), max(data$visual_memory)),
      visual_motor_speed = c(min(data$visual_motor_speed), max(data$visual_motor_speed)),
      verbal_memory = c(min(data$verbal_memory), max(data$verbal_memory)),
      reaction_speed = c(min(data$reaction_speed), max(data$reaction_speed)),
      age = c(min(data$age), max(data$age)),
      education = c(min(data$education), max(data$education))
    )
    
    analysis_data$data_medians <- list(
      visual_memory = round(median(data$visual_memory)),
      visual_motor_speed = round(median(data$visual_motor_speed)),
      verbal_memory = round(median(data$verbal_memory)),
      reaction_speed = round(median(data$reaction_speed)),
      age = round(median(data$age)),
      education = round(median(data$education))
    )
    
    # Fit multivariate regression
    mvreg <- lm(cbind(visual_memory, visual_motor_speed, 
                      verbal_memory, reaction_speed_flipped) ~ 
                  age + sex_numeric + education, 
                data = data)
    
    # Get predictions and residuals
    predictions <- fitted(mvreg)
    residuals_matrix <- residuals(mvreg)
    colnames(predictions) <- c("visual_memory", "visual_motor_speed", 
                               "verbal_memory", "reaction_speed_flipped")
    colnames(residuals_matrix) <- colnames(predictions)
    
    # Compute tolerance region on residuals
    n_outcomes <- ncol(residuals_matrix)
    semi_order_list <- list(lower = 1:n_outcomes, center = NULL, upper = NULL)
    
    set.seed(123)
    tolerance_region_residuals <- npmvtol.region(
      x = residuals_matrix,
      alpha = 0.05,
      P = 0.95,
      depth.fn = mahal_depth,
      type = "semispace",
      semi.order = semi_order_list
    )
    
    # Extract residual cutoffs
    residual_lower_cutoffs <- tolerance_region_residuals[, 1]
    names(residual_lower_cutoffs) <- colnames(residuals_matrix)
    
    # Calculate personalized cutoffs
    personalized_cutoffs <- predictions + 
      matrix(residual_lower_cutoffs, nrow = nrow(predictions), 
             ncol = ncol(predictions), byrow = TRUE)
    
    # Get observed outcomes
    observed_outcomes <- cbind(
      data$visual_memory,
      data$visual_motor_speed,
      data$verbal_memory,
      data$reaction_speed_flipped
    )
    colnames(observed_outcomes) <- colnames(predictions)
    
    # Identify atypical samples
    atypical_flags <- observed_outcomes < personalized_cutoffs
    
    # Fit univariate models
    model_visual_memory <- lm(visual_memory ~ age + sex_numeric + education, data = data)
    model_visual_motor_speed <- lm(visual_motor_speed ~ age + sex_numeric + education, data = data)
    model_verbal_memory <- lm(verbal_memory ~ age + sex_numeric + education, data = data)
    model_reaction_speed_flipped <- lm(reaction_speed_flipped ~ age + sex_numeric + education, data = data)
    
    # Get residuals and compute percentiles
    resid_visual_memory <- residuals(model_visual_memory)
    resid_visual_motor_speed <- residuals(model_visual_motor_speed)
    resid_verbal_memory <- residuals(model_verbal_memory)
    resid_reaction_speed_flipped <- residuals(model_reaction_speed_flipped)
    
    sd_visual_memory <- sd(resid_visual_memory)
    sd_visual_motor_speed <- sd(resid_visual_motor_speed)
    sd_verbal_memory <- sd(resid_verbal_memory)
    sd_reaction_speed_flipped <- sd(resid_reaction_speed_flipped)
    
    std_resid_visual_memory <- resid_visual_memory / sd_visual_memory
    std_resid_visual_motor_speed <- resid_visual_motor_speed / sd_visual_motor_speed
    std_resid_verbal_memory <- resid_verbal_memory / sd_verbal_memory
    std_resid_reaction_speed_flipped <- resid_reaction_speed_flipped / sd_reaction_speed_flipped
    
    percentile_visual_memory <- pnorm(std_resid_visual_memory) * 100
    percentile_visual_motor_speed <- pnorm(std_resid_visual_motor_speed) * 100
    percentile_verbal_memory <- pnorm(std_resid_verbal_memory) * 100
    percentile_reaction_speed <- pnorm(std_resid_reaction_speed_flipped) * 100
    
    # Apply Iverson classification
    iverson_classification <- apply(
      cbind(percentile_visual_memory, 
            percentile_visual_motor_speed, 
            percentile_verbal_memory, 
            percentile_reaction_speed),
      1,
      classify_iverson
    )
    
    # Store all results
    analysis_data$data <- data
    analysis_data$mvreg <- mvreg
    analysis_data$predictions <- predictions
    analysis_data$residuals_matrix <- residuals_matrix
    analysis_data$residual_lower_cutoffs <- residual_lower_cutoffs
    analysis_data$personalized_cutoffs <- personalized_cutoffs
    analysis_data$atypical_flags <- atypical_flags
    analysis_data$iverson_classification <- iverson_classification
    
    # Store univariate models
    analysis_data$univariate_models <- list(
      visual_memory = model_visual_memory,
      visual_motor_speed = model_visual_motor_speed,
      verbal_memory = model_verbal_memory,
      reaction_speed_flipped = model_reaction_speed_flipped,
      sd = list(
        visual_memory = sd_visual_memory,
        visual_motor_speed = sd_visual_motor_speed,
        verbal_memory = sd_verbal_memory,
        reaction_speed_flipped = sd_reaction_speed_flipped
      )
    )
    
    analysis_data$data_loaded <- TRUE
  }
  
  # Output for data loading status
  output$data_loaded <- reactive({
    return(analysis_data$data_loaded)
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Render data status message
  output$data_status <- renderUI({
    if (is.null(input$data_file) && !analysis_data$data_loaded) {
      div(class = "error-message",
        icon("info-circle"),
        " Please upload a CSV file to begin analysis"
      )
    } else if (analysis_data$data_loaded) {
      div(class = "success-message",
        icon("check-circle"),
        sprintf(" Data loaded successfully: %d subjects", nrow(analysis_data$data))
      )
    }
  })
  
  # Dynamic input fields based on loaded data
  output$dynamic_inputs <- renderUI({
    req(analysis_data$data_loaded)
    
    tagList(
      numericInput("age", "Age (years):", 
                  value = analysis_data$data_medians$age,
                  min = analysis_data$data_ranges$age[1], 
                  max = analysis_data$data_ranges$age[2]),
      
      selectInput("sex", "Sex:", 
                 choices = c("Male" = "m", "Female" = "f"),
                 selected = "m"),
      
      numericInput("education", "Education (years):", 
                  value = analysis_data$data_medians$education,
                  min = analysis_data$data_ranges$education[1], 
                  max = analysis_data$data_ranges$education[2]),
      
      uiOutput("validation_message"),
      
      hr(),
      h5("Observed Scores:"),
      
      numericInput("visual_memory", "Visual Memory:", 
                  value = analysis_data$data_medians$visual_memory,
                  min = analysis_data$data_ranges$visual_memory[1], 
                  max = analysis_data$data_ranges$visual_memory[2]),
      
      numericInput("visual_motor_speed", "Visual Motor Speed:", 
                  value = analysis_data$data_medians$visual_motor_speed,
                  min = analysis_data$data_ranges$visual_motor_speed[1], 
                  max = analysis_data$data_ranges$visual_motor_speed[2]),
      
      numericInput("verbal_memory", "Verbal Memory:", 
                  value = analysis_data$data_medians$verbal_memory,
                  min = analysis_data$data_ranges$verbal_memory[1], 
                  max = analysis_data$data_ranges$verbal_memory[2]),
      
      numericInput("reaction_speed", "Reaction Speed (ms):", 
                  value = analysis_data$data_medians$reaction_speed,
                  min = analysis_data$data_ranges$reaction_speed[1], 
                  max = analysis_data$data_ranges$reaction_speed[2]),
      
      actionButton("assess", "Assess", 
                  class = "btn-primary btn-lg btn-block",
                  style = "margin-top: 20px;")
    )
  })
  
  # Update tolerance region when button is clicked
  observeEvent(input$update_tolerance, {
    req(analysis_data$data_loaded)
    
    showNotification("Recalculating tolerance region... This may take a moment.",
                     type = "message", duration = 2)
    
    new_alpha <- 1 - input$alpha
    new_P <- input$P
    
    n_outcomes <- ncol(analysis_data$residuals_matrix)
    semi_order_list <- list(lower = 1:n_outcomes, center = NULL, upper = NULL)
    
    set.seed(123)
    new_tolerance_region <- npmvtol.region(
      x = analysis_data$residuals_matrix,
      alpha = new_alpha,
      P = new_P,
      depth.fn = mahal_depth,
      type = "semispace",
      semi.order = semi_order_list
    )
    
    new_residual_lower_cutoffs <- new_tolerance_region[, 1]
    names(new_residual_lower_cutoffs) <- colnames(analysis_data$residuals_matrix)
    
    new_personalized_cutoffs <- analysis_data$predictions + 
      matrix(new_residual_lower_cutoffs, nrow = nrow(analysis_data$predictions), 
             ncol = ncol(analysis_data$predictions), byrow = TRUE)
    
    observed_outcomes <- cbind(
      analysis_data$data$visual_memory,
      analysis_data$data$visual_motor_speed,
      analysis_data$data$verbal_memory,
      analysis_data$data$reaction_speed_flipped
    )
    colnames(observed_outcomes) <- colnames(analysis_data$predictions)
    
    new_atypical_flags <- observed_outcomes < new_personalized_cutoffs
    
    analysis_data$residual_lower_cutoffs <- new_residual_lower_cutoffs
    analysis_data$personalized_cutoffs <- new_personalized_cutoffs
    analysis_data$atypical_flags <- new_atypical_flags
    analysis_data$alpha <- new_alpha
    analysis_data$P <- new_P
    
    showNotification(
      sprintf("Tolerance region updated: Confidence = %.1f%%, Coverage = %.1f%%",
              input$alpha * 100, input$P * 100),
      type = "message", duration = 3
    )
  })
  
  # Validate age and education combination
  is_valid_demographics <- reactive({
    req(input$age, input$education, analysis_data$data_loaded)
    input$age - input$education >= 5
  })
  
  # Render validation message
  output$validation_message <- renderUI({
    req(input$age, input$education, analysis_data$data_loaded)
    
    if (!is_valid_demographics()) {
      div(class = "error-message",
        icon("exclamation-triangle"),
        " Invalid combination!",
        br(),
        sprintf("Age (%d) - Education (%d) = %d years", 
                input$age, input$education, input$age - input$education),
        br(),
        "Education cannot exceed Age - 5 years.",
        br(),
        sprintf("Maximum education for age %d is %d years.", 
                input$age, input$age - 5)
      )
    } else {
      div(style = "color: #009900; font-weight: bold; padding: 5px;",
        icon("check-circle"),
        " Valid demographics"
      )
    }
  })
  
  # Assess new subject
  new_assessment <- eventReactive(input$assess, {
    req(analysis_data$data_loaded)
    
    if (!is_valid_demographics()) {
      showNotification(
        "Cannot assess: Invalid age and education combination. Please adjust the values.",
        type = "error",
        duration = 5
      )
      return(NULL)
    }
    
    new_data <- data.frame(
      age = input$age,
      sex = input$sex,
      sex_numeric = ifelse(input$sex == "m", 0, 1),
      education = input$education,
      visual_memory = input$visual_memory,
      visual_motor_speed = input$visual_motor_speed,
      verbal_memory = input$verbal_memory,
      reaction_speed = input$reaction_speed,
      reaction_speed_flipped = -input$reaction_speed
    )
    
    # Multivariate predictions
    new_pred_mv <- predict(analysis_data$mvreg, newdata = new_data)
    new_cutoffs <- new_pred_mv + analysis_data$residual_lower_cutoffs
    
    new_observed <- c(
      input$visual_memory,
      input$visual_motor_speed,
      input$verbal_memory,
      -input$reaction_speed
    )
    
    is_atypical <- new_observed < new_cutoffs
    any_atypical <- any(is_atypical)
    
    region_status <- ifelse(any_atypical, "OUTSIDE", "INSIDE")
    region_text <- ifelse(any_atypical, "OUTSIDE Region", "INSIDE Region")
    
    # Univariate predictions and percentiles
    models <- analysis_data$univariate_models
    
    new_pred_visual_memory <- predict(models$visual_memory, newdata = new_data)
    new_pred_visual_motor_speed <- predict(models$visual_motor_speed, newdata = new_data)
    new_pred_verbal_memory <- predict(models$verbal_memory, newdata = new_data)
    new_pred_reaction_speed_flipped <- predict(models$reaction_speed_flipped, newdata = new_data)
    
    new_resid_visual_memory <- input$visual_memory - new_pred_visual_memory
    new_resid_visual_motor_speed <- input$visual_motor_speed - new_pred_visual_motor_speed
    new_resid_verbal_memory <- input$verbal_memory - new_pred_verbal_memory
    new_resid_reaction_speed_flipped <- -input$reaction_speed - new_pred_reaction_speed_flipped
    
    new_std_resid_visual_memory <- new_resid_visual_memory / models$sd$visual_memory
    new_std_resid_visual_motor_speed <- new_resid_visual_motor_speed / models$sd$visual_motor_speed
    new_std_resid_verbal_memory <- new_resid_verbal_memory / models$sd$verbal_memory
    new_std_resid_reaction_speed_flipped <- new_resid_reaction_speed_flipped / models$sd$reaction_speed_flipped
    
    new_percentile_visual_memory <- pnorm(new_std_resid_visual_memory) * 100
    new_percentile_visual_motor_speed <- pnorm(new_std_resid_visual_motor_speed) * 100
    new_percentile_verbal_memory <- pnorm(new_std_resid_verbal_memory) * 100
    new_percentile_reaction_speed <- pnorm(new_std_resid_reaction_speed_flipped) * 100
    
    new_percentiles <- c(new_percentile_visual_memory,
                         new_percentile_visual_motor_speed,
                         new_percentile_verbal_memory,
                         new_percentile_reaction_speed)
    new_iverson_class <- classify_iverson(new_percentiles)
    
    results <- data.frame(
      Outcome = c("Visual Memory", "Visual Motor Speed", 
                  "Verbal Memory", "Reaction Speed"),
      Cutoff = c(round(new_cutoffs[1], 1), 
                 round(new_cutoffs[2], 1),
                 round(new_cutoffs[3], 1), 
                 round(-new_cutoffs[4], 3)),
      Percentile = c(round(new_percentile_visual_memory, 1),
                     round(new_percentile_visual_motor_speed, 1),
                     round(new_percentile_verbal_memory, 1),
                     round(new_percentile_reaction_speed, 1))
    )
    
    list(
      overall_status = region_status,
      status_text = region_text,
      any_atypical = any_atypical,
      results = results,
      demographics = new_data,
      iverson_classification = new_iverson_class
    )
  })
  
  # Render new subject results (same as original - truncated for brevity)
  output$new_subject_results <- renderUI({
    result <- new_assessment()
    
    if (is.null(result)) {
      return(div(class = "result-box atypical-result",
        h4(style = "color: #cc0000;", 
           icon("exclamation-circle"),
           " Cannot Display Results"),
        p("Please correct the age and education values above.")
      ))
    }
    
    # Demographics section
    demographics_section <- div(
      style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
      h4(icon("user"), " Subject Demographics"),
      p(style = "font-size: 16px; margin: 5px 0;",
        strong("Age: "), result$demographics$age, " years | ",
        strong("Sex: "), ifelse(result$demographics$sex == "m", "Male", "Female"), " | ",
        strong("Education: "), result$demographics$education, " years")
    )
    
    # Tolerance panel
    tolerance_color <- ifelse(result$any_atypical, "#cc0000", "#009900")
    tolerance_bg <- ifelse(result$any_atypical, "#ffe6e6", "#e6ffe6")
    
    tolerance_panel <- div(
      style = paste0("background-color: ", tolerance_bg, "; padding: 20px; border-radius: 10px; border: 3px solid ", tolerance_color, ";"),
      h4(style = paste0("color: ", tolerance_color, "; margin-top: 0;"),
         icon("chart-line"), " Tolerance-Based Method"),
      h3(style = paste0("color: ", tolerance_color, ";"),
         result$status_text),
      p(style = "color: #666; font-size: 14px;",
        "Multivariate regression with personalized cutoffs"),
      hr(style = "border-color: #ccc;"),
      renderTable({
        data.frame(
          Outcome = result$results$Outcome,
          Cutoff = result$results$Cutoff
        )
      }, striped = FALSE, hover = FALSE, bordered = TRUE, width = "100%")
    )
    
    # Iverson panel (with highlighting and tooltip)
    iverson_colors <- list(
      "Broadly Normal" = list(color = "#009900", bg = "#e6ffe6"),
      "Below Average" = list(color = "#cc8800", bg = "#fff8e6"),
      "Well Below Average" = list(color = "#ff6600", bg = "#fff0e6"),
      "Unusually Low" = list(color = "#cc0000", bg = "#ffe6e6"),
      "Extremely Low" = list(color = "#990000", bg = "#ffe0e0")
    )
    
    iverson_style <- iverson_colors[[result$iverson_classification]]
    
    # Determine which percentiles to highlight based on classification
    # Only highlight for "Unusually Low" and "Extremely Low"
    highlight_rows <- rep(FALSE, 4)
    if (result$iverson_classification %in% c("Unusually Low", "Extremely Low")) {
      percentiles <- result$results$Percentile
      
      if (result$iverson_classification == "Extremely Low") {
        # Highlight scores ≤5th percentile or ≤2nd percentile
        highlight_rows <- percentiles <= 5
      } else if (result$iverson_classification == "Unusually Low") {
        # Highlight scores ≤10th percentile or ≤5th percentile or ≤2nd percentile
        highlight_rows <- percentiles <= 10
      }
    }
    
    # Create table with highlighting
    percentile_table <- result$results
    percentile_table$Percentile_Display <- paste0(percentile_table$Percentile, "%")
    
    iverson_panel <- div(
      style = paste0("background-color: ", iverson_style$bg, "; padding: 20px; border-radius: 10px; border: 3px solid ", iverson_style$color, ";"),
      h4(style = paste0("color: ", iverson_style$color, "; margin-top: 0;"),
         icon("percent"), " Percentile-Based Method (Iverson)",
         span(class = "tooltip-content",
           span(class = "info-icon", icon("info-circle")),
           span(class = "tooltip-text",
             HTML("
               <strong>Iverson Classification Rules:</strong>
               <strong>Broadly Normal:</strong>
               ≤2 scores ≤25th percentile AND<br/>
               ≤1 score ≤16th percentile AND<br/>
               0 scores ≤10th percentile
               
               <strong>Below Average:</strong>
               ≥3 scores ≤25th percentile OR<br/>
               2 scores ≤16th percentile OR<br/>
               1 score ≤10th percentile
               
               <strong>Well Below Average:</strong>
               ≥3 scores ≤16th percentile OR<br/>
               2 scores ≤10th percentile OR<br/>
               1 score ≤5th percentile
               
               <strong>Unusually Low:</strong>
               ≥3 scores ≤10th percentile OR<br/>
               2 scores ≤5th percentile OR<br/>
               1 score ≤2nd percentile
               
               <strong>Extremely Low:</strong>
               ≥3 scores ≤5th percentile OR<br/>
               ≥2 scores ≤2nd percentile
             ")
           )
         )
      ),
      h3(style = paste0("color: ", iverson_style$color, ";"),
         result$iverson_classification),
      p(style = "color: #666; font-size: 14px;",
        "Univariate regression with standardized percentiles"),
      hr(style = "border-color: #ccc;"),
      tags$table(style = "width: 100%; border-collapse: collapse; border: 1px solid #ddd;",
        tags$thead(
          tags$tr(
            tags$th(style = "padding: 8px; text-align: left; border: 1px solid #ddd;", "Outcome"),
            tags$th(style = "padding: 8px; text-align: left; border: 1px solid #ddd;", "Percentile")
          )
        ),
        tags$tbody(
          lapply(1:nrow(percentile_table), function(i) {
            row_style <- if(highlight_rows[i]) {
              "padding: 8px; border: 1px solid #ddd; background-color: #ffcccc; font-weight: bold;"
            } else {
              "padding: 8px; border: 1px solid #ddd;"
            }
            tags$tr(
              tags$td(style = row_style, percentile_table$Outcome[i]),
              tags$td(style = row_style, percentile_table$Percentile_Display[i])
            )
          })
        )
      )
    )
    
    div(
      demographics_section,
      fluidRow(
        column(6, tolerance_panel),
        column(6, iverson_panel)
      )
    )
  })
  
  # Render full dataset table
  output$full_table <- renderDT({
    req(analysis_data$data_loaded)
    
    display_data <- data.frame(
      ID = 1:nrow(analysis_data$data),
      Age = analysis_data$data$age,
      Sex = analysis_data$data$sex,
      Education = analysis_data$data$education,
      
      Visual_Memory = round(analysis_data$data$visual_memory, 3),
      Visual_Motor_Speed = round(analysis_data$data$visual_motor_speed, 3),
      Verbal_Memory = round(analysis_data$data$verbal_memory, 3),
      Reaction_Speed = round(analysis_data$data$reaction_speed, 3),
      
      Tolerance_Bounds = ifelse(rowSums(analysis_data$atypical_flags) > 0, "OUTSIDE", "INSIDE"),
      Iverson_Classification = analysis_data$iverson_classification
    )
    
    datatable(display_data,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'frtip',
                columnDefs = list(
                  list(className = 'dt-center', targets = '_all')
                )
              ),
              rownames = FALSE) %>%
      formatStyle(
        'Tolerance_Bounds',
        fontWeight = 'bold',
        backgroundColor = styleEqual(c('OUTSIDE', 'INSIDE'), 
                                     c('#ff9999', '#99ff99'))
      ) %>%
      formatStyle(
        'Iverson_Classification',
        fontWeight = 'bold',
        backgroundColor = styleEqual(
          c('Broadly Normal', 'Below Average', 'Well Below Average', 
            'Unusually Low', 'Extremely Low'),
          c('#99ff99', '#ffff99', '#ffcc99', '#ff9999', '#ff6666')
        )
      )
  })
  
  # Download handler
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("cognitive_analysis_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(analysis_data$data_loaded)
      
      download_data <- data.frame(
        ID = 1:nrow(analysis_data$data),
        Age = analysis_data$data$age,
        Sex = analysis_data$data$sex,
        Education = analysis_data$data$education,
        
        Visual_Memory = analysis_data$data$visual_memory,
        Visual_Motor_Speed = analysis_data$data$visual_motor_speed,
        Verbal_Memory = analysis_data$data$verbal_memory,
        Reaction_Speed = analysis_data$data$reaction_speed,
        
        Tolerance_Bounds = ifelse(rowSums(analysis_data$atypical_flags) > 0, "ATYPICAL", "Typical"),
        Iverson_Classification = analysis_data$iverson_classification
      )
      
      write.csv(download_data, file, row.names = FALSE)
    }
  )
}

################################################################################
# Run App
################################################################################

shinyApp(ui = ui, server = server)