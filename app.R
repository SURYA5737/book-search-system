# ==============================================================================
# Interactive Book Catalog Search Engine
# ------------------------------------------------------------------------------
# A Shiny app that reads "books_catalog.csv" and lets the user search by
# Book_Title or Genre in real time (case-insensitive, as they type).
#
# Required packages: shiny, shinythemes, DT
#   install.packages(c("shiny", "shinythemes", "DT"))
#
# Run: open this file in RStudio and click "Run App"
#      (or run shiny::runApp("book-search-system") from the console).
# ==============================================================================

library(shiny)
library(shinythemes)   # provides the clean 'cosmo' Bootstrap theme
library(DT)            # interactive tables

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------
# Path to the data file. Because the CSV sits next to app.R, a relative path
# works when the app is launched from its own folder (RStudio does this).
CSV_FILE <- "books_catalog.csv"

# Columns the app expects to find in the CSV
REQUIRED_COLS <- c("Book_Title", "Genre", "Price_INR", "Cost_Status")

# ------------------------------------------------------------------------------
# USER INTERFACE
# ------------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  # Small CSS tweak to make the search box large and highly visible
  tags$head(
    tags$style(HTML("
      #search_text {
        font-size: 20px;
        height: 50px;
        border: 2px solid #2780E3;
      }
      .search-label label {
        font-size: 18px;
        font-weight: bold;
      }
    "))
  ),
  
  titlePanel("Book Catalog Search Engine"),
  
  fluidRow(
    column(
      width = 8, offset = 2,
      wellPanel(
        div(class = "search-label",
            textInput(
              inputId     = "search_text",
              label       = "Search Book Title or Genre",
              placeholder = "Start typing, e.g. 'fiction' or 'python'...",
              width       = "100%"
            )
        ),
        helpText("Results update instantly as you type. Search is not case-sensitive.")
      )
    )
  ),
  
  fluidRow(
    column(
      width = 10, offset = 1,
      textOutput("result_count"),
      br(),
      DTOutput("books_table")
    )
  )
)

# ------------------------------------------------------------------------------
# SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # ---- Load data -------------------------------------------------------------
  # reactive() re-reads the file whenever the app needs it, so changes to the
  # CSV are picked up without restarting the app (reload the page to refresh).
  # It returns EITHER a data frame OR a character string describing the error.
  catalog <- reactive({
    
    # ERROR HANDLING 1: file missing or in the wrong folder
    if (!file.exists(CSV_FILE)) {
      return(paste0(
        "ERROR: '", CSV_FILE, "' was not found. ",
        "Please place it in the same folder as app.R. ",
        "Current working directory: ", getwd()
      ))
    }
    
    # ERROR HANDLING 2: file exists but cannot be parsed
    data <- tryCatch(
      read.csv(CSV_FILE, stringsAsFactors = FALSE, strip.white = TRUE),
      error = function(e) paste("ERROR: Could not read the CSV file. Details:", e$message)
    )
    if (is.character(data)) return(data)
    
    # ERROR HANDLING 3: required columns missing
    missing_cols <- setdiff(REQUIRED_COLS, names(data))
    if (length(missing_cols) > 0) {
      return(paste(
        "ERROR: The CSV is missing required column(s):",
        paste(missing_cols, collapse = ", ")
      ))
    }
    
    data[, REQUIRED_COLS]   # keep columns in a predictable order
  })
  
  # ---- Filter data -----------------------------------------------------------
  # Re-runs automatically on every keystroke in the search box.
  filtered <- reactive({
    data <- catalog()
    
    # If loading failed, pass the error message straight through
    if (is.character(data)) return(data)
    
    query <- trimws(input$search_text)
    
    # Empty search box -> show the full catalog
    if (is.null(query) || query == "") return(data)
    
    # grepl with ignore.case = TRUE, checked on Title OR Genre at the same time.
    # fixed = TRUE treats the query as plain text so characters like "(" or "+"
    # can't break the search with regex errors.
    match_rows <- grepl(query, data$Book_Title, ignore.case = TRUE, fixed = FALSE) |
      grepl(query, data$Genre,      ignore.case = TRUE, fixed = FALSE)
    
    data[match_rows, , drop = FALSE]
  })
  
  # ---- Outputs ---------------------------------------------------------------
  output$result_count <- renderText({
    data <- filtered()
    if (is.character(data)) return("")
    paste(nrow(data), "book(s) found")
  })
  
  output$books_table <- renderDT({
    data <- filtered()
    
    # Show the helpful error message inside the table instead of crashing
    if (is.character(data)) {
      return(datatable(
        data.frame(Message = data),
        rownames = FALSE,
        options  = list(dom = "t")
      ))
    }
    
    datatable(
      data,
      rownames = FALSE,
      options  = list(pageLength = 10, dom = "tip"),
      colnames = c("Book Title", "Genre", "Price (INR)", "Cost Status")
    )
  })
}

# ------------------------------------------------------------------------------
# RUN THE APP
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)