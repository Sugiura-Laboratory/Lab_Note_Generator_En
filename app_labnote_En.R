# ============================================================
# Version    : 1.0.0
# Author     : Shuichi Sugiura
# Date       : 2025-08-09
# Code Name  : Wadden
# ------------------------------------------------------------
# Description:
#   This Shiny app automatically generates lab notes for psychology experiments.
#   When a participant ID is entered, it assigns Heartbeat Counting Task (HCT)
#   trial durations (e.g., 30, 45, 55 seconds) in a non-repeating order based
#   on a pre-prepared randomization list (randomization_list.csv).
#
#   Experiment details (participant ID, lab number, experimenter name,
#   start/end times) are recorded and output as:
#     - PDF or editable Rmd lab note
#     - UTF-8 BOM encoded CSV (Excel compatible on Windows/macOS)
#
#   All generated files are stored locally and are not uploaded online.
# ------------------------------------------------------------
# Notes:
#   - Participant ID is formatted as 3 digits (e.g., 001)
#   - Timestamps use "YYYY-MM-DD HH:MM"
#   - randomization_list.csv is provided as a sample
# ============================================================

# app_labnote_En.R
library(shiny)
library(rmarkdown)
library(dplyr)
library(stringr)
library(readr)  # robust CSV reading (handles BOM, etc.)

# Current time (YYYY-MM-DD HH:MM)
now_str <- function() format(Sys.time(), "%Y-%m-%d %H:%M")

ui <- fluidPage(
  titlePanel("Lab note (EN)"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Input"),
      textInput("lab_number", "Lab room:", placeholder = "e.g., A101"),
      textInput("experimenter_name", "Experimenter (shown in table):", placeholder = "e.g., Sugiura"),
      hr(),
      textInput("subject_id", "Participant ID:", placeholder = "e.g., 1"),
      hr(),
      h4("Procedure"),
      p(strong("1. Heartbeat Counting Task")),
      p(strong("2. Experiment (Computer)")),
      p(strong("3. Questionnaire (Computer)")),
      hr(),
      h4("Heartbeat Counting Task (HCT)"),
      p("HCT trial order automatically changes by Participant ID."),
      tags$div(
        style = "font-size: 1.2em; color: blue; margin-bottom: 10px; border: 1px solid #ccc; padding: 10px; border-radius: 5px;",
        htmlOutput("assigned_task_order")
      ),
      hr(),
      fluidRow(
        column(8, textInput("start_time", "Start (YYYY-MM-DD HH:MM):", value = now_str())),
        column(4, style = "margin-top: 25px;", actionButton("timestamp_start", "Now"))
      ),
      fluidRow(
        column(8, textInput("end_time", "End (YYYY-MM-DD HH:MM):", value = now_str())),
        column(4, style = "margin-top: 25px;", actionButton("timestamp_end", "Now"))
      ),
      hr(),
      checkboxInput("generate_pdf", "Generate PDF", value = TRUE),
      helpText("If unchecked, an editable .Rmd will be saved (title/author edited in Rmd)."),
      hr(),
      actionButton("generate_files", "Generate Report", class = "btn-primary btn-lg"),
      hr(),
      helpText(strong("App Info")),
      helpText("Version: 1.0.0"),
      helpText("Author: Shuichi Sugiura"),
      helpText("Code Name: Wadden"),
      helpText("Date: 2025-08-07")
    ),
    mainPanel(
      width = 8,
      h3("Preview (CSV content)"),
      tableOutput("result_table")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$timestamp_start, { updateTextInput(session, "start_time", value = now_str()) })
  observeEvent(input$timestamp_end,   { updateTextInput(session, "end_time",   value = now_str()) })
  
  # --- Robust randomization list reader (handles BOM, header variants) ---
  validate_rand_list <- reactive({
    path <- "randomization_list.csv"
    if (!file.exists(path)) stop("randomization_list.csv not found. Put it in the repo root.")
    
    # Prefer readr (auto BOM handling), fallback to base read.csv
    df <- tryCatch(
      {
        suppressWarnings(readr::read_csv(path, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8")))
      },
      error = function(e) {
        read.csv(path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE)
      }
    )
    
    # Normalize column names: strip BOM, trim, case-insensitive matching
    nm <- names(df)
    nm <- sub("^\ufeff", "", nm) # remove BOM at start if exists
    nm <- trimws(nm)
    names(df) <- nm
    
    # Identify ID column (accept id/subject_id/participant/participant_id)
    id_candidates <- tolower(names(df))
    id_idx <- which(id_candidates %in% c("id","subject_id","participant","participant_id"))[1]
    if (is.na(id_idx)) stop("No ID column found in randomization_list.csv (e.g., ID, subject_id).")
    names(df)[id_idx] <- "ID"
    
    # Standardize ID to 3-digit zero-padded
    df$ID <- sprintf("%03d", as.integer(stringr::str_extract(df$ID, "\\d+")))
    
    # Map trial columns (allow trial1/2/3 or t1/2/3)
    tl <- tolower(names(df))
    t1 <- names(df)[which(tl %in% c("trial1","t1"))[1]]
    t2 <- names(df)[which(tl %in% c("trial2","t2"))[1]]
    t3 <- names(df)[which(tl %in% c("trial3","t3"))[1]]
    if (any(is.na(c(t1,t2,t3)))) stop("Trial1–Trial3 columns not found in randomization_list.csv.")
    df[[t1]] <- as.integer(df[[t1]])
    df[[t2]] <- as.integer(df[[t2]])
    df[[t3]] <- as.integer(df[[t3]])
    
    dplyr::rename(df, Trial1 = !!t1, Trial2 = !!t2, Trial3 = !!t3)
  })
  
  # HCT order text (e.g., 45 s → 55 s → 30 s)
  assigned_order_formatted <- reactive({
    req(input$subject_id)
    id_num_str <- stringr::str_extract(input$subject_id, "\\d+")
    if (is.na(id_num_str)) return(NULL)
    id3 <- sprintf("%03d", as.integer(id_num_str))
    
    rl <- validate_rand_list()
    matched <- dplyr::filter(rl, ID == id3)
    if (nrow(matched) == 1) {
      paste0(matched$Trial1, " s → ", matched$Trial2, " s → ", matched$Trial3, " s")
    } else NULL
  })
  
  output$assigned_task_order <- renderUI({
    ord <- assigned_order_formatted()
    if (!is.null(ord)) HTML(paste("<b>HCT order:</b>", ord))
    else HTML("<span style='color: red;'>ID not found in randomization list</span>")
  })
  
  # Escape for LaTeX
  sanitize_latex <- function(x) {
    if (is.null(x)) return(NULL)
    x <- gsub("\\\\", "\\\\textbackslash{}", x)
    x <- gsub("([{}])", "\\\\\\1", x, perl = TRUE)
    x <- gsub("_", "\\\\_", x, fixed = TRUE)
    x <- gsub("%", "\\\\%", x, fixed = TRUE)
    x <- gsub("\\$", "\\\\$", x)
    x <- gsub("&", "\\\\&", x, fixed = TRUE)
    x <- gsub("#", "\\\\#", x, fixed = TRUE)
    x <- gsub("\\^", "\\\\textasciicircum{}", x)
    x <- gsub("~", "\\\\textasciitilde{}", x)
    x
  }
  
  observeEvent(input$generate_files, {
    tryCatch({
      withProgress(message = 'Generating...', value = 0, {
        ord <- assigned_order_formatted()
        if (is.null(ord)) stop("Valid Participant ID not found in randomization list.")
        
        # 3-digit ID
        id_num <- as.integer(stringr::str_extract(input$subject_id, "\\d+"))
        id3 <- sprintf("%03d", id_num)
        
        # Naming: labnote_ID-<3digits>_<YYYYMMDD-HHMM>
        ts_tag <- format(Sys.time(), "%Y%m%d-%H%M")
        base_filename <- sprintf("labnote_ID-%s_%s", id3, ts_tag)
        
        # CSV (UTF-8 + BOM)
        incProgress(0.33, detail = "Writing CSV...")
        csv_file_path <- paste0(base_filename, ".csv")
        output_data <- data.frame(
          ID = id3,
          ExperimentOrder = "Heartbeat Counting Task → Experiment → Questionnaire",
          HCT_Order = ord,
          StartDateTime = input$start_time,
          EndDateTime   = input$end_time,
          LabNumber = input$lab_number,
          Experimenter = input$experimenter_name,
          stringsAsFactors = FALSE
        )
        con <- file(csv_file_path, open = "wb")
        writeBin(charToRaw('\ufeff'), con)  # BOM
        close(con)
        write.table(
          output_data, csv_file_path, sep = ",",
          row.names = FALSE, col.names = TRUE,
          fileEncoding = "UTF-8", append = TRUE, qmethod = "double"
        )
        
        # Params for Rmd (title/author edited in Rmd; experimenter in table from Shiny)
        incProgress(0.66, detail = "Preparing Rmd params...")
        params <- list(
          subject_id        = id3,
          experiment_order  = "Heartbeat Counting Task → Experiment → Questionnaire",
          start_time        = input$start_time,
          end_time          = input$end_time,
          lab_number        = sanitize_latex(input$lab_number),
          hct_order         = sanitize_latex(ord),
          output_timestamp  = now_str(),
          experimenter_name = sanitize_latex(input$experimenter_name)
        )
        
        # Render
        incProgress(1, detail = "Rendering...")
        generated_file <- ""
        
        if (isTRUE(input$generate_pdf)) {
          pdf_file_path <- paste0(base_filename, ".pdf")
          rmarkdown::render(
            input = "labnote_template_En.Rmd",
            output_file = pdf_file_path,
            params = params,
            envir = new.env(parent = globalenv())
          )
          generated_file <- pdf_file_path
        } else {
          rmd_file_path <- paste0(base_filename, ".Rmd")
          template <- readLines("labnote_template_En.Rmd", warn = FALSE, encoding = "UTF-8")
          yaml_end_index <- which(template == "---")[2]
          rmd_body <- template[(yaml_end_index + 1):length(template)]
          date_str <- format(Sys.Date(), "%Y-%m-%d")
          
          new_yaml <- sprintf(
            '---
title: "Research Title (edit here)"
author: "Author Name (edit here)"
date: "%s"
output:
  pdf_document:
    latex_engine: lualatex
    toc: false
    number_sections: false
header-includes: |
  \\usepackage{luatexja}
  \\usepackage{luatexja-fontspec}
  \\setmainfont{IPAexMincho}
  \\usepackage{geometry}
  \\geometry{a4paper, left=2cm, right=2cm, top=2.5cm, bottom=2.5cm}
  \\usepackage{fancyhdr}
  \\setlength{\\headheight}{14pt}
  \\fancyhf{}
  \\lhead{Experiment Record}
  \\rhead{Report Generated: \\texttt{`r params$output_timestamp`}}
  \\cfoot{\\thepage}
  \\renewcommand{\\headrulewidth}{0.4pt}
  \\renewcommand{\\footrulewidth}{0.4pt}
  \\usepackage{booktabs}
  \\usepackage{tabularx}
  \\pagestyle{fancy}
  \\fancypagestyle{plain}{
    \\fancyhf{}
    \\lhead{Experiment Record}
    \\rhead{Report Generated: \\texttt{`r params$output_timestamp`}}
    \\cfoot{\\thepage}
    \\renewcommand{\\headrulewidth}{0.4pt}
    \\renewcommand{\\footrulewidth}{0.4pt}
  }
params:
  subject_id: "%s"
  experiment_order: "%s"
  start_time: "%s"
  end_time: "%s"
  lab_number: "%s"
  hct_order: "%s"
  output_timestamp: "%s"
  experimenter_name: "%s"
---',
            date_str,
            params$subject_id, params$experiment_order, params$start_time,
            params$end_time, params$lab_number, params$hct_order,
            params$output_timestamp, params$experimenter_name
          )
          
          writeLines(c(new_yaml, rmd_body), rmd_file_path, useBytes = TRUE)
          generated_file <- rmd_file_path
        }
        
        output$result_table <- renderTable(output_data)
        showModal(modalDialog(
          title = "Done",
          paste(generated_file, "and", csv_file_path, "have been saved."),
          easyClose = TRUE, footer = NULL
        ))
      })
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        tags$b("Failed to generate PDF/Rmd. Message:"),
        hr(),
        tags$pre(conditionMessage(e))
      ))
    })
  })
}

shinyApp(ui = ui, server = server)
