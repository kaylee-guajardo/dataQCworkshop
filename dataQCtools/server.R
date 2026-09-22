#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Source the function calls
source("functions/functions.R")

# Load packages
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)
library(readxl)
library(ggplot2)

# Define server logic
function(input, output, session) {

  # Track current step and store data objects
  vals <- reactiveValues(
    step = "start",
    is_processing = FALSE, # For folder processing step
    ldrtimes = NULL,
    prepared_data = NULL,    # For single file plot
    processed_data = NULL,   # For single file plot
    original_filename = NULL,
    current_plot = NULL,     # For single file plot
    batch_plots = list(),    # Gallery: Stores all ggplot objects from batch
    batch_filenames = c()    # Gallery: Stores names of files in batch
  )

  output$step_container <- renderUI({

    # LOADING OVERLAY
    # If is_processing is TRUE, show a full-screen gray overlay with a spinner/text
    if(vals$is_processing) {
      div(
        style = "position: fixed; z-index: 999; left: 0; top: 0; width: 100%; height: 100%;
               background-color: rgba(255,255,255,0.7); display: flex;
               justify-content: center; align-items: center; flex-direction: column;",
        div(class = "spinner-border text-primary", role = "status"), # Bootstrap spinner
        h3("Processing your files...", style = "margin-top: 20px;"),
        p("Please do not close the browser. This may take a few minutes.")
      )
    }

    # Step 1: Initial Choice
    if(vals$step == "start") {
      tagList(
        p("Welcome! Please begin by selecting your data source."),
        layout_column_wrap(width = 0.5, actionButton("btn_begin", "Get Started", class = "btn-primary"))
      )
    }
    # Step 2: Upload LDR File
    else if (vals$step == "ldr_upload") {
      tagList(
        p("Step 1: Please upload the LDR dataset (ldrtimes) file:"),
        fileInput("ldr_file", "Upload LDR Excel File", accept = c(".xlsx", ".xls")),
        hr(),
        layout_column_wrap(width = 0.5,
                           actionButton("btn_to_source_choice", "Next: Select Data Source", class = "btn-success"),
                           actionButton("btn_back_start", "← Go Back", class = "btn-secondary")
        )
      )
    }
    # Step 3: File or folder?
    else if(vals$step == "source_choice") {
      tagList(
        p("Step 2: Do you have one data file or a folder of data files you would like to crop?"),
        layout_column_wrap(width = 0.5,
                           actionButton("btn_file", "Single File", class = "btn-primary"),
                           actionButton("btn_folder", "Folder", class = "btn-primary")
        )
      )
    }
    # Step 4 Option A: Single File upload
    else if (vals$step == "file_upload") {
      tagList(
        p("Step 3: Please upload your raw data csv file:"),
        fileInput("user_file", "Upload", accept = ".csv"),
        hr(),
        layout_column_wrap(width = 0.5,
                           actionButton("btn_process_file", "Process & Crop", class = "btn-success"),
                           actionButton("btn_back_choice", "← Go Back", class = "btn-secondary")
        )
      )
    }
    # Step 4 Option B: Folder Pathway Inputs
    else if (vals$step == "folder_input"){
      tagList(
        p("Step 3: Provide the absolute folder paths for processing:"),
        textInput("folder_path_raw", "Path to Raw Data Folder:", placeholder = "e.g. C:/Data/Summer2022/Raw"),
        textInput("folder_path_csv", "Path to save Cropped CSVs:", placeholder = "e.g. C:/Data/Summer2022/Cropped_CSVs"),
        textInput("folder_path_plots", "Path to save Plot Images:", placeholder = "e.g. C:/Data/Summer2022/Cropped_Plots"),
        hr(),
        layout_column_wrap(width = 0.5,
                           actionButton("btn_submit_folder", "Start Batch Processing", class = "btn-success"),
                           actionButton("btn_back_choice", "← Go Back", class = "btn-secondary")
        )
      )
    }
    # Step 5 Option A: Single File Completion
    else if (vals$step == "complete") {
      tagList(
        p("Complete. Would you like to download the cropped data file, or proceed directly to visualizing the cropped data?"),
        layout_column_wrap(width = 0.5,
                           downloadButton("download_data", "Download Datafile", class = "btn-primary"),
                           actionButton("btn_visualize", "Cropped Visualization", class = "btn-info")
        ),
        hr(),
        actionButton("btn_restart", "Start Over", class = "btn-secondary")
      )
    }
    # Step 5 Option B: Batch Processing Completion / Gallery
    else if (vals$step == "batch_gallery") {
      tagList(
        p("Batch Processing Complete! All files have been saved to your folders."),
        p("Select a file from the list to preview its plot below:"),

        # Use layout_sidebar for a clean "Control Panel" vs "Viewing Area" look
        layout_sidebar(
          sidebar = sidebar(
            title = "Select",
            selectInput("selected_batch_file", "Choose a file:", choices = vals$batch_filenames)
          ),
          # Main content area (the plot and buttons)
          div(
            plotOutput("batch_plot", height = "600px"),
            hr(),
            layout_column_wrap(
              width = 0.5,
              downloadButton("download_batch_plot", "Download Selected Plot", class = "btn-primary"),
              actionButton("btn_restart", "Start Over", class = "btn-secondary")
            )
          )
        )
      )
    }
    # Step 6: Single File Visualization
    else if (vals$step == "visualize") {
      tagList(
        p("Data Visualization: Raw vs Cropped"),
        plotOutput("crop_plot", height = "500px"),
        hr(),
        layout_column_wrap(width = 0.5,
                           downloadButton("download_plot", "Download Plot Image", class = "btn-primary"),
                           actionButton("btn_back_complete", "← Back to Options", class = "btn-secondary")
        )
      )
    }
  })

  # --- Navigation ---
  observeEvent(input$btn_begin, { vals$step <- "ldr_upload" })
  observeEvent(input$btn_back_start, { vals$step <- "start" })
  observeEvent(input$btn_to_source_choice, {
    req(input$ldr_file)
    vals$ldrtimes <- readxl::read_excel(input$ldr_file$datapath)
    vals$step <- "source_choice"
  })
  observeEvent(input$btn_file, { vals$step <- "file_upload" })
  observeEvent(input$btn_folder, { vals$step <- "folder_input" })
  observeEvent(input$btn_back_choice, { vals$step <- "source_choice" })
  observeEvent(input$btn_restart, {
    vals$step <- "start"; vals$processed_data <- NULL; vals$prepared_data <- NULL;
    vals$ldrtimes <- NULL; vals$batch_plots <- list(); vals$batch_filenames <- c()
  })

  # --- Single File Processing Logic ---
  observeEvent(input$btn_process_file, {
    req(input$user_file, vals$ldrtimes)
    temp_path <- input$user_file$datapath
    actual_name <- input$user_file$name
    tryCatch({
      file.copy(temp_path, actual_name, overwrite = TRUE)
      vals$prepared_data <- prep_raw_data(csv_file = actual_name, ldrtimes = vals$ldrtimes)
      vals$processed_data <- crop_raw_data(this.data = vals$prepared_data, csv_file = actual_name, ldrtimes = vals$ldrtimes)
      vals$original_filename <- actual_name
      vals$step <- "complete"
    }, error = function(e) { showNotification(paste("Error:", e$message), type = "error") })
  })

  # --- Batch Folder Processing Logic ---
  observeEvent(input$btn_submit_folder, {
    req(vals$ldrtimes)

    # 1. Path cleaning (same as before)
    raw_dir <- input$folder_path_raw; if(!grepl("/$", raw_dir)) raw_dir <- paste0(raw_dir, "/")
    csv_dir <- input$folder_path_csv; if(!grepl("/$", csv_dir)) csv_dir <- paste0(csv_dir, "/")
    plot_dir <- input$folder_path_plots; if(!grepl("/$", plot_dir)) plot_dir <- paste0(plot_dir, "/")

    csv_files <- list.files(path = raw_dir, pattern = "\\.csv$", full.names = FALSE)

    if(length(csv_files) == 0) {
      showNotification("No CSV files found in the raw data folder.", type = "error")
      return()
    }

    # --- START PROCESSING STATE ---
    vals$is_processing <- TRUE

    # Use withProgress to show the blue bar at the top
    withProgress(message = 'Processing Batch...', value = 0, {

      tryCatch({
        temp_plots_list <- list()
        temp_names <- c()

        # Loop through files
        for(i in seq_along(csv_files)) {
          csv_file <- csv_files[i]

          # Update the progress bar
          # (i / total_files) calculates the percentage completion
          setProgress(value = i / length(csv_files),
                      detail = paste("Processing file", i, "of", length(csv_files), ":", csv_file))

          # --- YOUR ACTUAL PROCESSING LOGIC ---
          full_path <- paste0(raw_dir, csv_file)
          this_data <- prep_raw_data(csv_file = full_path, ldrtimes = vals$ldrtimes)
          cropped_data <- crop_raw_data(this.data = this_data, csv_file = full_path, ldrtimes = vals$ldrtimes)
          p <- plot_cropped_data(cropped.data = cropped_data, this.data = this_data, ldrtimes = vals$ldrtimes)

          temp_plots_list[[csv_file]] <- p
          temp_names <- c(temp_names, csv_file)

          readr::write_csv(cropped_data, file=paste0(csv_dir, stringr::str_split_i(csv_file, "[.]", 1), "_cropped.csv"))

          filename_parts = stringr::str_split_1(csv_file, '[_.]')
          ggplot2::ggsave(paste0(plot_dir, filename_parts[1], "_", filename_parts[2], "_rawvscroppeddata.png"),
                          p, width = 11, height = 8.5)
        }

        vals$batch_plots <- temp_plots_list
        vals$batch_filenames <- temp_names
        vals$step <- "batch_gallery"

      }, error = function(e) {
        showNotification(paste("Batch Error:", e$message), type = "error")
      })

      # --- END PROCESSING STATE ---
      vals$is_processing <- FALSE
    })
  })

  # --- Rendering Plots ---
  # Single file plot
  observeEvent(input$btn_visualize, {
    vals$current_plot <- plot_cropped_data(vals$processed_data, vals$prepared_data, vals$ldrtimes)
    vals$step <- "visualize"
  })
  output$crop_plot <- renderPlot({ req(vals$current_plot); vals$current_plot })

  # Batch gallery plot
  output$batch_plot <- renderPlot({
    req(input$selected_batch_file)
    vals$batch_plots[[input$selected_batch_file]]
  })

  # --- Download Handlers ---
  output$download_data <- downloadHandler(
    filename = function() { paste0("cropped_", vals$original_filename) },
    content = function(file) { write.csv(vals$processed_data, file, row.names = FALSE) }
  )
  output$download_plot <- downloadHandler(
    filename = function() { paste0("plot_", vals$original_filename, ".png") },
    content = function(file) { ggplot2::ggsave(file, plot = vals$current_plot, width = 11, height = 8.5) }
  )
  output$download_batch_plot <- downloadHandler(
    filename = function() { paste0("plot_", input$selected_batch_file, ".png") },
    content = function(file) { ggplot2::ggsave(file, plot = vals$batch_plots[[input$selected_batch_file]], width = 11, height = 8.5) }
  )

  observeEvent(input$btn_back_complete, { vals$step <- "complete" })
}
