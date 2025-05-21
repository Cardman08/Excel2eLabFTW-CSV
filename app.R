####
# Version: 0.13.2
# Author:  Christian Jaeger (christian.jaeger@uk-halle.de)
# 20250519
####

################
#
## Beschreibung: Excel zu CSV Konverter
# 0.13.2
# BugFix - Fehler, wenn keine Metadaten ausgegeben werden sollen, behoben.
#
#
# 0.12.x
#
## Main Changes:
# Separate Felder für alle Spalten des Excels (nicht mehr im Content Block, da sonst keine Zeilenumbrüche umsetzbar sind)
# Checkboxen zur Auswahl, ob Metadaten (text oder URLs) und Tags exportiert werden sollen.
# Änderung der Reihenfolge von Schaltflächen und Eingabefeldern (etwas aufgeräumt ;)
# Standard-tags wurde in Imported umbenannt. (wird nur gesetzt, wenn keine Auswahl für Tags getroffen wird)
# Readme und Kontakt (Logo vorbereitet)
#
# 0.13.x
#
# Für jede Metadaten-Spalte wählbarer Typ:
#   Text: Freitext
#   Datum: Automatische Formatierung in YYYY-MM-DD
#   Dropdown: Automatische Erkennung aller unterschiedlichen Zellinhalte als Auswahloptionen
#   URL: Format für Weblinks
# Automatisches Filtern leerer oder ungültiger Eingaben (NA, NULL, leere Felder)
#
################



library(shiny)
library(readxl)
library(jsonlite)
library(readr)

# UI der Shiny-App
ui <- fluidPage(
  tags$head(tags$title("Excel zu CSV Konverter - DIZ & Biomedical Data Science")),
  titlePanel(
    div(
      tags$img(src = "Logo_DIZ_DE.jpg", height = "80px", style = "margin-right: 10px;"),
      div(
        h1("Excel zu CSV Konverter für eLabFTW - 0.13.2", style = "margin-bottom: 0px;"),
        h4("Ein Service des Datenintegrationszentrums (DIZ) und der AG (Bio-) Medical Data Science", 
           style = "margin-top: 5px; color: gray; font-weight: normal;")
      )
    )
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Lade eine Excel-Datei hoch", accept = c(".xlsx")),
      uiOutput("columns_selector"),
      hr(),
      checkboxInput("enable_tags", "Tags ausgeben", value = TRUE),
      uiOutput("tags_selector"),
      hr(),
      checkboxInput("enable_text_metadata", "Text-Metadaten hinzufügen", value = FALSE),
      uiOutput("group_name_text_ui"),
      uiOutput("json_columns_selector"),
      hr(),
      actionButton("generate_csv", "CSV erstellen"),
      downloadButton("download_csv", "CSV herunterladen"),
      hr(),
      tags$a(href = "readme.html", "README", target = "_blank"),
      br(), br(),
      h4("Kontakt"),
      tags$p("Fragen? Schreiben Sie an: "),
      tags$a(href = "mailto:christian.jaeger@uk-halle.de", "christian.jaeger@uk-halle.de"),
    ),
    mainPanel(
      tableOutput("preview_data")
    )
  )
)

# Server der Shiny-App
server <- function(input, output, session) {
  
  # New 13 - fange Nullwerte ab (Auch groß und kleinschreibung der nullwerte beachten)
  clean_value <- function(x) {
    val <- trimws(as.character(x))
    if (is.null(x) || is.na(x) || val == "" || toupper(val) == "NA" || toupper(val) == "NULL") {
      return("")
    } else {
      return(val)
    }
  }
  
  
  # Reaktive Datei: Lese die hochgeladene Excel-Datei
  excel_data <- reactive({
    req(input$file)
    read_excel(input$file$datapath)
  })
  
  # Zeige die verfügbaren Spalten zur Auswahl
  output$columns_selector <- renderUI({
    req(excel_data())
    column_names <- colnames(excel_data())
    
    tagList(
      selectInput("title_col", "Wähle die Spalte für 'title':", choices = column_names, selected = column_names[1]),
      checkboxGroupInput("content_cols", "Wähle die Spalten für die Ausgabe:", choices = column_names, selected = column_names[2])
    )
  })
  
  # Tags-Spalte aktivieren/deaktivieren
  output$tags_selector <- renderUI({
    req(excel_data())
    column_names <- colnames(excel_data())
    
    if (input$enable_tags) {
      selectInput("tags_col", "Wähle die Spalte für 'tags' (leer = \"Imported\") (optional):", choices = c("", column_names), selected = "")
    }
  })
  
  # JSON-Text-Metadaten aktivieren/deaktivieren NEW_13
  output$json_columns_selector <- renderUI({
    req(excel_data())
    column_names <- colnames(excel_data())
    
    if (input$enable_text_metadata) {
      tagList(
        checkboxGroupInput("json_cols", "Wähle die Spalten für zusätzliche Metadaten:", 
                           choices = column_names, selected = NULL),
        uiOutput("json_col_type_selectors")
      )
    }
  })
  
  output$json_col_type_selectors <- renderUI({
    req(input$json_cols)
    lapply(input$json_cols, function(col) {
      selectInput(
        inputId = paste0("json_type_", col),
        label = paste("Typ für Spalte:", col),
        choices = c("Text", "Datum (ohne Uhrzeit)", "Drop-Down", "URL"),
        selected = "Text"
      )
    })
  })
  
  output$group_name_text_ui <- renderUI({
    if (input$enable_text_metadata) {
      textInput("group_name_text", "Name der JSON-Gruppe für Textmetadaten:", value = "Textmetadaten (JSON)")
    }
  })

  
  # Vorschau
  output$preview_data <- renderTable({
    req(excel_data())
    head(excel_data(), 10)  # Zeige die ersten 10 Zeilen
  })
  
  # Generiere die CSV-Datei
  generate_csv <- reactive({
    req(excel_data(), input$title_col)
    
    data <- excel_data()
    
    # 'title'-Spalte
    data$title <- data[[input$title_col]]
    
    # 'tags'-Spalte, falls aktiviert und ein Wert ausgewählt wurde oder Standardwert setzen - (Imported)
    if (input$enable_tags) {
      if (!is.null(input$tags_col) && input$tags_col != "") {
        data$tags <- data[[input$tags_col]]
      } else {
        data$tags <- "imported"
      }
    }
    
    # Metadaten generieren, falls aktiviert NEW_13
    if (input$enable_text_metadata) {
      data$metadata <- apply(data, 1, function(row) {
        extra_fields_groups <- list()
        extra_fields <- list()
        group_id <- 1
        
        if (input$enable_text_metadata && !is.null(input$json_cols) && length(input$json_cols) > 0) {
          for (field in input$json_cols) {
            field_type <- input[[paste0("json_type_", field)]]
            value <- as.character(row[[field]])
            field_data <- list(group_id = group_id)
            
            # Typ-bezogene Verarbeitung
            if (field_type == "Text") {
              field_data$type <- "text"
              field_data$value <- value
            } else if (field_type == "Datum (ohne Uhrzeit)") {
              field_data$type <- "date"
              value <- clean_value(row[[field]])
              #field_data$value <- format(as.Date(value), "%Y-%m-%d")
              parsed_date <- suppressWarnings(as.Date(value, tryFormats = c("%Y-%m-%d", "%d.%m.%Y", "%m/%d/%Y")))
              field_data$value <- if (!is.na(parsed_date)) format(parsed_date, "%Y-%m-%d") else ""
            } else if (field_type == "Drop-Down") {
              field_data$type <- "select"
              value <- clean_value(row[[field]])
              field_data$value <- value
              
              # Alle Werte der Spalte bereinigen
              field_values <- excel_data()[[field]]
              field_values <- sapply(field_values, clean_value, USE.NAMES = FALSE)
              
              # Duplikate entfernen
              opts <- unique(field_values)
              
              # Workaround: eLabFTW erwartet bei 'options' ein Array – auch wenn nur 1 Wert!
              if (length(opts) == 1) {
                opts <- c(opts, opts[1])  # künstlich duplizieren
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
    
    # Wähle nur die tatsächlich benötigten Spalten
    selected_columns <- c("title")
    if (input$enable_tags) {
      selected_columns <- c(selected_columns, "tags")
    }
    if (!is.null(input$content_cols) && length(input$content_cols) > 0) {
      selected_columns <- c(selected_columns, input$content_cols)
    }
    if (input$enable_text_metadata) {
      selected_columns <- c(selected_columns, "metadata")
    }
    data <- data[selected_columns]
    
    data
  })
  
  # CSV-Datei für den Download bereitstellen
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

# Shiny-App starten
shinyApp(ui = ui, server = server)
