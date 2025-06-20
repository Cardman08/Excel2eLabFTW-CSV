####
# Version: 0.13.3
# Author:  Christian Jaeger (christian.jaeger@uk-halle.de)
# Date:    2025-06-20
#
# Description: Excel to CSV converter for eLabFTW (>= v5.2.x)
#
# Changelog 0.13.3:
# - Adjusted to new import logic in eLabFTW (from version 5.2.x):
#   → All selected content columns are now combined into a single 'body' column formatted as an HTML table.
#   → Format: <table><tr><td>ColumnName</td><td>Value</td></tr>...</table>
# - HTML escaping added for all cell values to prevent formatting issues or HTML injection.
# - New feature: "Select all / deselect all" toggle button for the content column selection (`checkboxGroupInput`).
# - All inline code comments translated to English.
#
# Previous versions:
#
# 0.13.2
# - BugFix: Prevented crash when no metadata was selected for export.
#
# 0.13.1
# - Added support for selectable metadata field types: 
#     * Text: free text
#     * Date: automatic formatting to YYYY-MM-DD
#     * Dropdown: unique cell values used as selectable options
#     * URL: web link format
# - Automatic filtering of empty/invalid values (NA, NULL, empty string).
#
# 0.12.x
# - Structural changes:
#     * All Excel columns now exported as separate fields (no longer grouped in one content block).
#     * Optional export of metadata (text or URL) and tags via checkboxes.
#     * Reordered UI components for better usability.
#     * Default tag renamed from "Standard-tags" to "Imported".
#     * README and contact section with logo placeholder added.
#
####

library(shiny)
library(readxl)
library(jsonlite)
library(readr)

# UI definition
ui <- fluidPage(
  tags$head(tags$title("Excel to CSV Converter - DIZ & Biomedical Data Science")),
  titlePanel(
    div(
      tags$img(src = "Logo_DIZ_DE.jpg", height = "80px", style = "margin-right: 10px;"),
      div(
        h1("Excel to CSV Converter for eLabFTW - 0.13.3", style = "margin-bottom: 0px;"),
        h4("A service by the Data Integration Center (DIZ) and the (Bio-)Medical Data Science Unit", 
           style = "margin-top: 5px; color: gray; font-weight: normal;")
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload an Excel file", accept = c(".xlsx")),
      uiOutput("columns_selector"),
      hr(),
      checkboxInput("enable_tags", "Export tags", value = TRUE),
      uiOutput("tags_selector"),
      hr(),
      checkboxInput("enable_text_metadata", "Add text metadata", value = FALSE),
      uiOutput("group_name_text_ui"),
      uiOutput("json_columns_selector"),
      hr(),
      actionButton("generate_csv", "Generate CSV"),
      downloadButton("download_csv", "Download CSV"),
      hr(),
      tags$a(href = "readme.html", "README", target = "_blank"),
      br(), br(),
      h4("Contact"),
      tags$p("Questions? Contact: "),
      tags$a(href = "mailto:christian.jaeger@uk-halle.de", "christian.jaeger@uk-halle.de")
    ),
    mainPanel(
      tableOutput("preview_data")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # HTML escaping function to prevent injection
  htmlEscape <- function(s) {
    if (is.na(s) || is.null(s)) return("")
    s <- gsub("&", "&amp;", s)
    s <- gsub("<", "&lt;", s)
    s <- gsub(">", "&gt;", s)
    s <- gsub("\"", "&quot;", s)
    s <- gsub("'", "&#039;", s)
    return(s)
  }
  
  # Normalize and clean values (handling NULL, NA, etc.)
  clean_value <- function(x) {
    val <- trimws(as.character(x))
    if (is.null(x) || is.na(x) || val == "" || toupper(val) == "NA" || toupper(val) == "NULL") {
      return("")
    } else {
      return(val)
    }
  }
  
  # Load uploaded Excel file
  excel_data <- reactive({
    req(input$file)
    read_excel(input$file$datapath)
  })
  
  # Toggle select/deselect all content columns
  observeEvent(input$toggle_all_cols, {
    req(excel_data())
    current <- input$content_cols
    all_cols <- colnames(excel_data())
    
    updateCheckboxGroupInput(session, "content_cols",
                             selected = if (length(current) < length(all_cols)) all_cols else character(0))
  })
  
  # Column selection UI (including select all toggle)
  output$columns_selector <- renderUI({
    req(excel_data())
    column_names <- colnames(excel_data())
    
    tagList(
      selectInput("title_col", "Select column for 'title':", choices = column_names, selected = column_names[1]),
      actionLink("toggle_all_cols", "Select / Deselect All", style = "margin-bottom: 5px; display: block;"),
      checkboxGroupInput("content_cols", "Select columns for body content output:", choices = column_names, selected = column_names[2])
    )
  })
  
  # Tags selection UI (optional)
  output$tags_selector <- renderUI({
    req(excel_data())
    column_names <- colnames(excel_data())
    
    if (input$enable_tags) {
      selectInput("tags_col", "Select column for 'tags' (empty = \"Imported\") (optional):", choices = c("", column_names), selected = "")
    }
  })
  
  # JSON metadata selector UI
  output$json_columns_selector <- renderUI({
    req(excel_data())
    column_names <- colnames(excel_data())
    
    if (input$enable_text_metadata) {
      tagList(
        checkboxGroupInput("json_cols", "Select columns for additional metadata:", 
                           choices = column_names, selected = NULL),
        uiOutput("json_col_type_selectors")
      )
    }
  })
  
  # Metadata type selectors per column
  output$json_col_type_selectors <- renderUI({
    req(input$json_cols)
    lapply(input$json_cols, function(col) {
      selectInput(
        inputId = paste0("json_type_", col),
        label = paste("Type for column:", col),
        choices = c("Text", "Date (no time)", "Drop-Down", "URL"),
        selected = "Text"
      )
    })
  })
  
  # Group name for JSON metadata
  output$group_name_text_ui <- renderUI({
    if (input$enable_text_metadata) {
      textInput("group_name_text", "Name of JSON group for text metadata:", value = "Text Metadata (JSON)")
    }
  })
  
  # Preview of uploaded data
  output$preview_data <- renderTable({
    req(excel_data())
    head(excel_data(), 10)
  })
  
  # CSV generation
  generate_csv <- reactive({
    req(excel_data(), input$title_col)
    
    data <- excel_data()
    
    # Add 'title' column
    data$title <- data[[input$title_col]]
    
    # Add 'tags' column if enabled
    if (input$enable_tags) {
      if (!is.null(input$tags_col) && input$tags_col != "") {
        data$tags <- data[[input$tags_col]]
      } else {
        data$tags <- "imported"
      }
    }
    
    # Create metadata as JSON if enabled
    if (input$enable_text_metadata) {
      data$metadata <- apply(data, 1, function(row) {
        extra_fields_groups <- list()
        extra_fields <- list()
        group_id <- 1
        
        if (!is.null(input$json_cols) && length(input$json_cols) > 0) {
          for (field in input$json_cols) {
            field_type <- input[[paste0("json_type_", field)]]
            value <- as.character(row[[field]])
            field_data <- list(group_id = group_id)
            
            if (field_type == "Text") {
              field_data$type <- "text"
              field_data$value <- value
            } else if (field_type == "Date (no time)") {
              field_data$type <- "date"
              value <- clean_value(row[[field]])
              parsed_date <- suppressWarnings(as.Date(value, tryFormats = c("%Y-%m-%d", "%d.%m.%Y", "%m/%d/%Y")))
              field_data$value <- if (!is.na(parsed_date)) format(parsed_date, "%Y-%m-%d") else ""
            } else if (field_type == "Drop-Down") {
              field_data$type <- "select"
              value <- clean_value(row[[field]])
              field_data$value <- value
              field_values <- excel_data()[[field]]
              field_values <- sapply(field_values, clean_value, USE.NAMES = FALSE)
              opts <- unique(field_values)
              if (length(opts) == 1) {
                opts <- c(opts, opts[1])
              }
              field_data$options <- opts
            } else if (field_type == "URL") {
              field_data$type <- "url"
              field_data$value <- value
            }
            
            extra_fields[[field]] <- field_data
          }
          
          extra_fields_groups <- list(list(id = group_id, name = input$group_name_text))
        }
        
        json <- list(
          elabftw = list(
            extra_fields_groups = extra_fields_groups
          ),
          extra_fields = extra_fields
        )
        
        toJSON(json, auto_unbox = TRUE)
      })
    }
    
    # Create 'body' column as HTML table from selected content columns
    if (!is.null(input$content_cols) && length(input$content_cols) > 0) {
      data$body <- apply(data, 1, function(row) {
        rows <- lapply(input$content_cols, function(col) {
          value <- clean_value(row[[col]])
          paste0("<tr><td><strong>", htmlEscape(col), "</strong></td><td>", htmlEscape(value), "</td></tr>")
        })
        paste0("<table>", paste0(rows, collapse = ""), "</table>")
      })
    }
    
    # Define output columns
    selected_columns <- c("title", "body")
    if (input$enable_tags) {
      selected_columns <- c(selected_columns, "tags")
    }
    if (input$enable_text_metadata) {
      selected_columns <- c(selected_columns, "metadata")
    }
    data <- data[selected_columns]
    
    data
  })
  
  # CSV file download
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("elabftw_output_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- generate_csv()
      write_delim(data, file = file, delim = ",", col_names = TRUE, quote = "needed")
    }
  )
}

# Launch the Shiny app
shinyApp(ui = ui, server = server)
