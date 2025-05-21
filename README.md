# Excel-to-CSV Converter for eLabFTW

This Shiny application provides a user-friendly interface for converting existing Excel files (e.g., lab inventories, reagent lists, device registries) into CSV files compatible with the import functionality of [eLabFTW](https://www.elabftw.net/), an open-source electronic lab notebook (ELN). It also supports the generation of structured JSON metadata blocks for enhanced data annotation in eLabFTW.

## Screenshot

![screenshot_excel_converter](https://github.com/user-attachments/assets/0780fdeb-0a2a-4065-9ddd-4a0a16c51b43)

## Features

- Upload `.xlsx` files and preview contents
- Select spreadsheet columns to map to eLabFTW fields:
  - Title, tags, and content columns
- Create structured JSON metadata blocks:
  - Field types: Text, Date, Dropdown, URL
  - Grouping of fields under user-defined labels
- Auto-formatting for dates (YYYY-MM-DD)
- Cleaning of empty, `NA`, or `NULL` values
- CSV export with metadata embedded in JSON structure (eLabFTW-compatible)

## Intended Use

This application is intended to support research groups transitioning from Excel-based inventory systems to eLabFTW. It reduces manual formatting and provides a standardized output that adheres to the FAIR principles (Findable, Accessible, Interoperable, Reusable).

## Installation and Usage

1. Ensure [R](https://cran.r-project.org/) and [RStudio](https://posit.co/) are installed.
2. Install required packages:

```r
install.packages(c("shiny", "readxl", "jsonlite", "readr"))

3. Launch the application:
shiny::runApp("path/to/csv-sql-merger")

## Dependencies
R ≥ 4.0
Packages:
shiny
readr
writexl
stringr
DT
shinyjs

## File Structure
app.R – Main application code
screenshot_sql_merger.png – Placeholder for interface screenshot
readme.html – Optional help documentation
