###############################################################################
# app.R — Readability Analyzer (MULTI-PDF compare + colourful plots)
# What you get:
# - Upload 1–5 PDFs and compare them
# - Cleaned extraction (de-hyphenation, whitespace)
# - Metrics: Flesch, FKG, ARI, Linsear, Fry (+ optional Spache)
# - Plots:
#   1) Dashboard (banded + labels) for selected doc
#   2) Compare plot (all docs side-by-side)
#   3) Fry plot (selected doc)
# - Tables + downloadable CSV (all docs)
#
# Run:
#   install.packages(c("shiny","pdftools","tokenizers","ggplot2","dplyr","DT"))
#   shiny::runApp("path/to/folder/with/app.R")
###############################################################################

library(shiny)
library(pdftools)
library(tokenizers)
library(ggplot2)
library(dplyr)
library(DT)

###############################################################################
# Colours
###############################################################################
PAL_FLESCH <- c(
  "Very difficult"   = "#D73027",
  "Difficult"        = "#FC8D59",
  "Fairly difficult" = "#FEE08B",
  "Standard"         = "#D9EF8B",
  "Fairly easy"      = "#91CF60",
  "Easy"             = "#1A9850",
  "Very easy"        = "#006837"
)

PAL_GRADE <- c(
  "Primary"       = "#2C7BB6",
  "Middle school" = "#00A6CA",
  "High school"   = "#00CCBC",
  "College"       = "#FEC44F",
  "Graduate+"     = "#E34A33"
)

COL_POINT_MAIN <- "#6A00FF"

###############################################################################
# 1) Text cleaning + token helpers
###############################################################################
clean_pdf_text <- function(text) {
  x <- text
  x <- gsub("-\\s*\\n\\s*", "", x)     # de-hyphenate across line breaks
  x <- gsub("\\n+", " ", x)           # newlines -> space
  x <- gsub("\\s+", " ", x)           # collapse spaces
  trimws(x)
}

.clean_text <- function(text) gsub("\\s+", " ", trimws(text))
.get_sentences <- function(text) tokenizers::tokenize_sentences(text)[[1]]
.get_words <- function(text)
  tokenizers::tokenize_words(text, strip_punct = TRUE, strip_numeric = FALSE)[[1]]

.syllables_in_word <- function(w) {
  w <- tolower(gsub("[^a-z]", "", w))
  if (nchar(w) == 0) return(0)
  vowel_groups <- gregexpr("[aeiouy]+", w, perl = TRUE)[[1]]
  n <- if (vowel_groups[1] == -1) 0 else length(vowel_groups)
  if (nchar(w) > 2 && grepl("e$", w) && !grepl("[aeiouy]le$", w) && n > 1) n <- n - 1
  max(1, n)
}

.total_syllables <- function(words) sum(vapply(words, .syllables_in_word, numeric(1)))
.total_characters <- function(words) sum(nchar(gsub("[^A-Za-z0-9]", "", words)))

###############################################################################
# 2) Metrics
###############################################################################
flesch_fkg_ari <- function(text) {
  txt <- .clean_text(text)
  if (nchar(txt) == 0) stop("Text is empty.")
  
  sents <- .get_sentences(txt)
  words <- .get_words(txt)
  total_sentences <- length(sents)
  total_words <- length(words)
  if (total_sentences == 0 || total_words == 0) stop("Could not detect sentences or words.")
  
  total_syllables <- .total_syllables(words)
  total_characters <- .total_characters(words)
  
  wps <- total_words / total_sentences
  spw <- total_syllables / total_words
  cpw <- total_characters / total_words
  
  fk_grade <- 0.39 * wps + 11.8 * spw - 15.59
  flesch_ease <- 206.835 - 1.015 * wps - 84.6 * spw
  ari <- 4.71 * cpw + 0.5 * wps - 21.43
  
  tibble::tibble(
    total_sentences = total_sentences,
    total_words = total_words,
    total_syllables = total_syllables,
    total_characters = total_characters,
    words_per_sentence = wps,
    syllables_per_word = spw,
    characters_per_word = cpw,
    flesch_reading_ease = flesch_ease,
    flesch_kincaid_grade = fk_grade,
    automated_readability_index = ari
  )
}

linsear_write <- function(text, sample_words = 100) {
  txt <- .clean_text(text)
  words <- .get_words(txt)
  if (length(words) == 0) stop("No words found.")
  sample_words <- min(sample_words, length(words))
  w <- words[1:sample_words]
  
  sample_txt <- paste(w, collapse = " ")
  sents <- .get_sentences(sample_txt)
  n_sent <- max(1, length(sents))
  
  syl <- vapply(w, .syllables_in_word, numeric(1))
  easy <- sum(syl <= 2)
  hard <- sum(syl >= 3)
  
  points <- easy * 1 + hard * 3
  r <- points / n_sent
  lw <- if (r > 20) (r / 2) else (r / 2 - 1)
  
  tibble::tibble(
    linsear_write = lw,
    linsear_sample_words = sample_words,
    linsear_sample_sentences = n_sent,
    linsear_easy_words = easy,
    linsear_hard_words = hard
  )
}

fry_stats <- function(text, n_samples = 3, sample_size = 100) {
  txt <- .clean_text(text)
  words <- .get_words(txt)
  if (length(words) < sample_size) stop("Not enough words for a 100-word Fry sample.")
  
  n_possible <- floor(length(words) / sample_size)
  n_take <- min(n_samples, n_possible)
  if (n_take < 1) stop("Not enough words for a 100-word Fry sample.")
  
  per_sample <- bind_rows(lapply(seq_len(n_take), function(i) {
    idx_start <- (i - 1) * sample_size + 1
    idx_end <- i * sample_size
    w <- words[idx_start:idx_end]
    s <- .get_sentences(paste(w, collapse = " "))
    tibble::tibble(
      sample = i,
      sentences_per_100 = length(s),
      syllables_per_100 = .total_syllables(w)
    )
  }))
  
  list(
    samples_used = n_take,
    avg_sentences_per_100 = mean(per_sample$sentences_per_100),
    avg_syllables_per_100 = mean(per_sample$syllables_per_100),
    per_sample = per_sample
  )
}

spache <- function(text, familiar_words, revised = TRUE) {
  if (missing(familiar_words) || is.null(familiar_words)) stop("Provide familiar_words for Spache.")
  txt <- .clean_text(text)
  sents <- .get_sentences(txt)
  words <- .get_words(txt)
  
  total_sentences <- length(sents)
  total_words <- length(words)
  if (total_sentences == 0 || total_words == 0) stop("Could not detect sentences or words.")
  
  asl <- total_words / total_sentences
  w_lower <- tolower(words)
  w_lower <- w_lower[nchar(gsub("[^a-z]", "", w_lower)) > 0]
  uniq_words <- unique(w_lower)
  fam <- unique(tolower(familiar_words))
  
  unfamiliar_unique <- setdiff(uniq_words, fam)
  pct_unfamiliar_unique <- if (length(uniq_words) == 0) 0 else (length(unfamiliar_unique) / length(uniq_words)) * 100
  
  if (revised) {
    grade <- (0.121 * asl) + (0.082 * pct_unfamiliar_unique) + 0.659
    version <- "Spache (revised)"
  } else {
    grade <- (0.141 * asl) + (0.086 * pct_unfamiliar_unique) + 0.839
    version <- "Spache (original)"
  }
  
  tibble::tibble(
    version = version,
    spache_grade_level = grade,
    spache_avg_sentence_length = asl,
    spache_pct_unique_unfamiliar = pct_unfamiliar_unique
  )
}

###############################################################################
# 3) Bands + plotting
###############################################################################
flesch_band <- function(fre) {
  cut(
    fre,
    breaks = c(-Inf, 30, 50, 60, 70, 80, 90, Inf),
    labels = c("Very difficult", "Difficult", "Fairly difficult", "Standard",
               "Fairly easy", "Easy", "Very easy"),
    right = FALSE
  )
}

grade_band <- function(g) {
  cut(
    g,
    breaks = c(-Inf, 6, 9, 13, 17, Inf),
    labels = c("Primary", "Middle school", "High school", "College", "Graduate+"),
    right = FALSE
  )
}

# Dashboard for ONE doc
plot_dashboard_one <- function(metrics_row) {
  df_f <- tibble::tibble(
    value = metrics_row$flesch_reading_ease,
    band = as.character(flesch_band(metrics_row$flesch_reading_ease)),
    label = paste0(round(metrics_row$flesch_reading_ease, 1), " — ", band)
  )
  
  df_g <- tibble::tibble(
    index = c("Flesch–Kincaid Grade", "ARI", "Linsear Write",
              if ("spache_original" %in% names(metrics_row)) "Spache (original)" else NULL,
              if ("spache_revised"  %in% names(metrics_row)) "Spache (revised)"  else NULL),
    value = c(metrics_row$flesch_kincaid_grade,
              metrics_row$automated_readability_index,
              metrics_row$linsear_write,
              if ("spache_original" %in% names(metrics_row)) metrics_row$spache_original else NULL,
              if ("spache_revised"  %in% names(metrics_row)) metrics_row$spache_revised  else NULL)
  ) |>
    mutate(
      band = as.character(grade_band(value)),
      label = paste0(round(value, 1), " — ", band),
      index = factor(index, levels = rev(index)) # stable order
    )
  
  flesch_bands <- tibble::tibble(
    xmin = c(0, 30, 50, 60, 70, 80, 90),
    xmax = c(30, 50, 60, 70, 80, 90, 100),
    band = factor(names(PAL_FLESCH), levels = names(PAL_FLESCH))
  )
  
  p1 <- ggplot() +
    geom_rect(
      data = flesch_bands,
      aes(xmin = xmin, xmax = xmax, ymin = 0.6, ymax = 1.4, fill = band),
      alpha = 0.85
    ) +
    geom_point(aes(x = df_f$value, y = 1), size = 6, shape = 21,
               fill = COL_POINT_MAIN, colour = "white", stroke = 1.1) +
    geom_label(aes(x = df_f$value, y = 1.62, label = df_f$label),
               fill = "white", alpha = 0.95, label.size = 0.25) +
    scale_fill_manual(values = PAL_FLESCH) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    labs(title = "Flesch Reading Ease (higher = easier)",
         subtitle = "Background colours show difficulty categories",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  grade_bands <- tibble::tibble(
    xmin = c(0, 6, 9, 13, 17),
    xmax = c(6, 9, 13, 17, 25),
    band = factor(names(PAL_GRADE), levels = names(PAL_GRADE))
  )
  
  p2 <- ggplot(df_g) +
    geom_rect(
      data = grade_bands,
      aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = nrow(df_g) + 1, fill = band),
      alpha = 0.55, inherit.aes = FALSE
    ) +
    geom_point(aes(x = value, y = index), size = 4.5, shape = 21,
               fill = COL_POINT_MAIN, colour = "white", stroke = 1.1) +
    geom_label(aes(x = value, y = index, label = label),
               hjust = -0.05, fill = "white", alpha = 0.95, label.size = 0.25) +
    scale_fill_manual(values = PAL_GRADE) +
    scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
    labs(title = "Grade-level indices (higher = harder)",
         subtitle = "Background colours show grade bands",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  list(p_flesch = p1, p_grade = p2)
}

# Compare plot for ALL docs
plot_compare <- function(metrics_tbl) {
  df <- metrics_tbl |>
    select(doc_id, doc_name, flesch_reading_ease,
           flesch_kincaid_grade, automated_readability_index, linsear_write) |>
    tidyr::pivot_longer(
      cols = c(flesch_reading_ease, flesch_kincaid_grade, automated_readability_index, linsear_write),
      names_to = "metric",
      values_to = "value"
    ) |>
    mutate(
      metric = recode(metric,
                      flesch_reading_ease = "Flesch Reading Ease (↑ easier)",
                      flesch_kincaid_grade = "Flesch–Kincaid Grade (↑ harder)",
                      automated_readability_index = "ARI (↑ harder)",
                      linsear_write = "Linsear Write (↑ harder)"),
      doc_name = factor(doc_name, levels = unique(doc_name))
    )
  
  ggplot(df, aes(x = doc_name, y = value, fill = doc_name)) +
    geom_col(width = 0.75, alpha = 0.9) +
    facet_wrap(~metric, scales = "free_y", ncol = 2) +
    geom_text(aes(label = round(value, 1)), vjust = -0.4, size = 3.6) +
    labs(
      title = "Compare readability indices across documents",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 25, hjust = 1)
    )
}

plot_fry_selected <- function(fry, label = "Selected", pad_x = 12, pad_y = 6) {
  x <- fry$avg_syllables_per_100
  y <- fry$avg_sentences_per_100
  xlim <- c(max(0, x - pad_x), x + pad_x)
  ylim <- c(max(0, y - pad_y), y + pad_y)
  
  ggplot() +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal(base_size = 13) +
    labs(
      title = "Fry components (selected document)",
      subtitle = "Right = more complex words; Up = shorter sentences",
      x = "Syllables per 100 words",
      y = "Sentences per 100 words"
    ) +
    annotate("rect", xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2],
             fill = "#F3F0FF", alpha = 0.6) +
    geom_point(
      data = fry$per_sample,
      aes(x = syllables_per_100, y = sentences_per_100),
      size = 4, shape = 21, stroke = 1.2,
      fill = "#00C2FF", colour = "#111111"
    ) +
    geom_text(
      data = fry$per_sample,
      aes(x = syllables_per_100, y = sentences_per_100, label = sample),
      vjust = -1, size = 4, colour = "#003B73"
    ) +
    geom_point(
      data = data.frame(x=x, y=y),
      aes(x = x, y = y),
      size = 6, shape = 21, stroke = 1.3,
      fill = COL_POINT_MAIN, colour = "white"
    ) +
    geom_label(
      data = data.frame(x=x, y=y, label=label),
      aes(x = x, y = y, label = label),
      vjust = -1.2, label.size = 0.25, alpha = 0.95, fill = "white"
    ) +
    geom_segment(aes(x = x, xend = x, y = ylim[1], yend = y),
                 linewidth = 0.8, linetype = "dashed", colour = "#444444") +
    geom_segment(aes(x = xlim[1], xend = x, y = y, yend = y),
                 linewidth = 0.8, linetype = "dashed", colour = "#444444")
}

###############################################################################
# 4) Shiny UI
###############################################################################
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body {background:#fafafa;}
    .box {background:#fff; border:1px solid #e6e6e6; border-radius:12px; padding:14px; margin-bottom:14px;}
    .title {font-weight:700; font-size:18px; margin-bottom:6px;}
    .small {color:#666; font-size:12px;}
    .tab-content { padding-top: 10px; }
    .nav-tabs > li > a { border-radius: 10px 10px 0 0; }
  "))),
  titlePanel("Readability Analyzer — Compare up to 5 PDFs"),
  
  sidebarLayout(
    sidebarPanel(
      div(class="box",
          div(class="title", "1) Input"),
          fileInput("pdfs", "Upload PDFs (1–5)", accept = c(".pdf"),
                    multiple = TRUE),
          helpText(class="small", "Tip: upload up to 5 PDFs to compare them."),
          checkboxInput("remove_refs", "Remove references section (simple heuristic)", value = TRUE),
          textInput("refs_regex", "Regex for reference start (edit if needed)",
                    value = "(?i)\\breferences\\b.*$"),
          hr(),
          checkboxInput("use_spache", "Compute Spache (requires familiar word list)", value = FALSE),
          fileInput("fam", "Upload familiar words (.txt, one word per line)", accept = c(".txt")),
          hr(),
          uiOutput("doc_selector_ui"),
          hr(),
          downloadButton("download_csv", "Download metrics CSV (all docs)")
      ),
      div(class="box",
          div(class="title", "2) Quick notes"),
          div(class="small",
              "• Flesch: higher = easier.\n",
              "• FKG/ARI/Linsear/Spache: higher = harder (grade level).\n",
              "• Fry uses syllables/100 + sentences/100 from 100-word chunks.")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Compare",
                 div(class="box", plotOutput("p_compare", height = 520)),
                 div(class="box", DTOutput("compare_table"))
        ),
        tabPanel("Selected doc dashboard",
                 div(class="box", plotOutput("p_flesch", height = 260)),
                 div(class="box", plotOutput("p_grade",  height = 360))
        ),
        tabPanel("Fry (selected doc)",
                 div(class="box", plotOutput("p_fry", height = 450)),
                 div(class="box", DTOutput("fry_samples"))
        ),
        tabPanel("Extracted text (selected preview)",
                 div(class="box", verbatimTextOutput("text_preview"))
        )
      )
    )
  )
)

###############################################################################
# 5) Shiny server
###############################################################################
server <- function(input, output, session) {
  
  familiar_words_reactive <- reactive({
    if (!isTRUE(input$use_spache)) return(NULL)
    req(input$fam)
    x <- readLines(input$fam$datapath, warn = FALSE, encoding = "UTF-8")
    x <- tolower(trimws(x))
    x <- x[nchar(x) > 0]
    unique(x)
  })
  
  # Extract+clean text for ALL uploaded PDFs
  texts_reactive <- reactive({
    req(input$pdfs)
    validate(need(nrow(input$pdfs) <= 5, "Please upload at most 5 PDFs."))
    
    out <- lapply(seq_len(nrow(input$pdfs)), function(i) {
      path <- input$pdfs$datapath[i]
      nm <- input$pdfs$name[i]
      pages <- pdftools::pdf_text(path)
      raw <- paste(pages, collapse = "\n")
      txt <- clean_pdf_text(raw)
      
      if (isTRUE(input$remove_refs) && nzchar(input$refs_regex)) {
        txt <- sub(input$refs_regex, "", txt, perl = TRUE)
        txt <- .clean_text(txt)
      }
      
      list(doc_id = paste0("doc", i), doc_name = nm, text = txt)
    })
    
    out
  })
  
  # doc selector (after upload)
  output$doc_selector_ui <- renderUI({
    req(input$pdfs)
    choices <- input$pdfs$name
    selectInput("selected_doc", "Select document for detailed view",
                choices = choices, selected = choices[[1]])
  })
  
  # Compute ALL metrics into one table + keep fry objects per doc
  all_results_reactive <- reactive({
    docs <- texts_reactive()
    fam <- familiar_words_reactive()
    
    metrics_list <- list()
    fry_map <- list()
    text_map <- list()
    
    for (d in docs) {
      b <- flesch_fkg_ari(d$text)
      lw <- linsear_write(d$text)
      fry <- fry_stats(d$text)
      
      # optional Spache (both versions)
      sp_o <- sp_r <- NULL
      if (!is.null(fam)) {
        sp_o <- spache(d$text, fam, revised = FALSE)
        sp_r <- spache(d$text, fam, revised = TRUE)
      }
      
      row <- b |>
        bind_cols(lw) |>
        mutate(
          doc_id = d$doc_id,
          doc_name = d$doc_name,
          fry_avg_syllables_per_100 = fry$avg_syllables_per_100,
          fry_avg_sentences_per_100 = fry$avg_sentences_per_100,
          fry_samples_used = fry$samples_used
        )
      
      if (!is.null(sp_o)) {
        row$spache_original <- sp_o$spache_grade_level
        row$spache_revised  <- sp_r$spache_grade_level
      }
      
      metrics_list[[d$doc_id]] <- row
      fry_map[[d$doc_name]] <- fry
      text_map[[d$doc_name]] <- d$text
    }
    
    metrics_tbl <- bind_rows(metrics_list) |>
      select(doc_id, doc_name, everything())
    
    list(metrics = metrics_tbl, fry_map = fry_map, text_map = text_map)
  })
  
  # Selected doc row helper
  selected_row_reactive <- reactive({
    res <- all_results_reactive()
    req(input$selected_doc)
    row <- res$metrics |> filter(doc_name == input$selected_doc)
    validate(need(nrow(row) == 1, "Selected document not found."))
    row[1, ]
  })
  
  # --- Compare plot + table ---
  output$p_compare <- renderPlot({
    res <- all_results_reactive()
    plot_compare(res$metrics)
  })
  
  output$compare_table <- renderDT({
    res <- all_results_reactive()
    tbl <- res$metrics |>
      select(doc_name,
             flesch_reading_ease, flesch_kincaid_grade,
             automated_readability_index, linsear_write,
             fry_avg_syllables_per_100, fry_avg_sentences_per_100,
             if ("spache_original" %in% names(res$metrics)) spache_original else NULL,
             if ("spache_revised"  %in% names(res$metrics)) spache_revised  else NULL)
    
    datatable(tbl, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # --- Selected doc dashboard ---
  output$p_flesch <- renderPlot({
    row <- selected_row_reactive()
    dash <- plot_dashboard_one(row)
    dash$p_flesch
  })
  
  output$p_grade <- renderPlot({
    row <- selected_row_reactive()
    dash <- plot_dashboard_one(row)
    dash$p_grade
  })
  
  # --- Selected doc Fry ---
  output$p_fry <- renderPlot({
    res <- all_results_reactive()
    req(input$selected_doc)
    fry <- res$fry_map[[input$selected_doc]]
    validate(need(!is.null(fry), "No Fry data found for this document."))
    plot_fry_selected(fry, label = input$selected_doc)
  })
  
  output$fry_samples <- renderDT({
    res <- all_results_reactive()
    req(input$selected_doc)
    fry <- res$fry_map[[input$selected_doc]]
    validate(need(!is.null(fry), "No Fry data found for this document."))
    datatable(fry$per_sample, options = list(pageLength = 5), rownames = FALSE)
  })
  
  # --- Selected doc text preview ---
  output$text_preview <- renderText({
    res <- all_results_reactive()
    req(input$selected_doc)
    txt <- res$text_map[[input$selected_doc]]
    validate(need(!is.null(txt), "No text found for this document."))
    substr(txt, 1, 2500)
  })
  
  # --- Download CSV (all docs) ---
  output$download_csv <- downloadHandler(
    filename = function() paste0("readability_metrics_compare_", Sys.Date(), ".csv"),
    content = function(file) {
      res <- all_results_reactive()
      write.csv(res$metrics, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)


