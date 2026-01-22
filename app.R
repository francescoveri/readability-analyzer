###############################################################################
# app.R — Readability + Credibility Signals Analyzer (MULTI-PDF compare)
# Upload 1–5 PDFs and compare:
# - Readability metrics (classic + extended)
# - Relationships: correlation heatmap + PCA
# - Windowed readability across the document
# - Credibility-style signals (heuristics): evidence density, hedging/boosters, etc.
###############################################################################

library(shiny)
library(pdftools)
library(tokenizers)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(tibble)
library(stringr)
library(purrr)
library(scales)

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
.get_words <- function(text) tokenizers::tokenize_words(text, strip_punct = TRUE, strip_numeric = FALSE)[[1]]

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
.total_letters <- function(words) sum(nchar(gsub("[^A-Za-z]", "", words)))

.count_complex_words <- function(words) {
  syl <- vapply(words, .syllables_in_word, numeric(1))
  sum(syl >= 3)
}

.count_long_words <- function(words, min_chars = 7) {
  w <- gsub("[^A-Za-z]", "", words)
  sum(nchar(w) >= min_chars)
}

###############################################################################
# 2) Readability metrics
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
  
  tibble(
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
  
  tibble(
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
    tibble(
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
  
  tibble(
    version = version,
    spache_grade_level = grade,
    spache_avg_sentence_length = asl,
    spache_pct_unique_unfamiliar = pct_unfamiliar_unique
  )
}

extra_indices <- function(text) {
  txt <- .clean_text(text)
  if (nchar(txt) == 0) stop("Text is empty.")
  
  sents <- .get_sentences(txt)
  words <- .get_words(txt)
  total_sentences <- length(sents)
  total_words <- length(words)
  if (total_sentences == 0 || total_words == 0) stop("Could not detect sentences or words.")
  
  wps <- total_words / total_sentences
  
  complex_words <- .count_complex_words(words)        # >= 3 syllables
  pct_complex <- (complex_words / total_words) * 100
  
  gunning_fog <- 0.4 * (wps + pct_complex)
  
  smog <- NA_real_
  if (total_sentences >= 3 && complex_words > 0) {
    smog <- 1.0430 * sqrt(complex_words * (30 / total_sentences)) + 3.1291
  }
  
  letters <- .total_letters(words)
  L <- (letters / total_words) * 100
  S <- (total_sentences / total_words) * 100
  coleman_liau <- 0.0588 * L - 0.296 * S - 15.8
  
  long_words <- .count_long_words(words, min_chars = 7) # >6 letters
  lix <- (total_words / total_sentences) + (long_words * 100 / total_words)
  rix <- long_words / total_sentences
  
  tibble(
    complex_words = complex_words,
    pct_complex_words = pct_complex,
    letters = letters,
    long_words_7plus = long_words,
    gunning_fog = gunning_fog,
    smog = smog,
    coleman_liau = coleman_liau,
    lix = lix,
    rix = rix
  )
}

###############################################################################
# 3) Credibility-style signals (heuristics)
###############################################################################
cred_signals <- function(text) {
  txt <- .clean_text(text)
  if (nchar(txt) == 0) stop("Text is empty.")
  words <- .get_words(txt)
  total_words <- length(words)
  if (total_words == 0) stop("No words found.")
  
  # Evidence markers (rough)
  n_numbers <- str_count(txt, "\\b\\d+(?:[\\.,]\\d+)?\\b")
  n_urls <- str_count(txt, "https?://|www\\.")
  n_cite_year <- str_count(txt, "\\((?:[^\\)]*?)(19\\d{2}|20\\d{2})(?:[^\\)]*?)\\)") + str_count(txt, "\\b(19\\d{2}|20\\d{2})\\b")
  n_et_al <- str_count(txt, "\\bet\\s+al\\.?\\b")
  
  # Hedge vs booster lexicons (tiny, editable)
  hedges <- c("may","might","could","possibly","perhaps","likely","suggests","suggest","indicates","indicate","appears","appear","seems","seem")
  boosters <- c("clearly","obviously","undoubtedly","certainly","proves","prove","always","never","everyone","no one","must","cannot")
  
  wlow <- tolower(words)
  hedge_n <- sum(wlow %in% hedges)
  booster_n <- sum(wlow %in% boosters)
  
  # Style alarms
  all_caps_words <- sum(str_detect(words, "^[A-Z]{3,}$"))
  exclam <- str_count(txt, "!")
  question <- str_count(txt, "\\?")
  
  # Densities per 1,000 words
  per_k <- function(x) (x / max(1, total_words)) * 1000
  
  tibble(
    numbers_n = n_numbers,
    urls_n = n_urls,
    cite_markers_n = n_cite_year,
    et_al_n = n_et_al,
    hedge_n = hedge_n,
    booster_n = booster_n,
    all_caps_words_n = all_caps_words,
    exclam_n = exclam,
    question_n = question,
    evidence_per_1k_words = per_k(n_numbers + n_urls + n_cite_year + n_et_al),
    hedges_per_1k_words = per_k(hedge_n),
    boosters_per_1k_words = per_k(booster_n),
    hype_per_1k_words = per_k(all_caps_words + exclam)
  )
}

###############################################################################
# 4) Windowed readability (across text)
###############################################################################
windowed_readability <- function(text, window_words = 400) {
  txt <- .clean_text(text)
  words <- .get_words(txt)
  if (length(words) < window_words) return(NULL)
  
  nwin <- floor(length(words) / window_words)
  if (nwin < 2) return(NULL)
  
  out <- bind_rows(lapply(seq_len(nwin), function(i) {
    w <- words[((i - 1) * window_words + 1):(i * window_words)]
    chunk <- paste(w, collapse = " ")
    base <- flesch_fkg_ari(chunk)
    tibble(
      window = i,
      start_word = (i - 1) * window_words + 1,
      end_word = i * window_words,
      fre = base$flesch_reading_ease,
      fkg = base$flesch_kincaid_grade
    )
  }))
  out
}

###############################################################################
# 5) Bands + plotting
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

plot_dashboard_one <- function(metrics_row) {
  df_f <- tibble(
    value = metrics_row$flesch_reading_ease,
    band = as.character(flesch_band(metrics_row$flesch_reading_ease)),
    label = paste0(round(metrics_row$flesch_reading_ease, 1), " — ", band)
  )
  
  idx_names <- c(
    "Flesch–Kincaid Grade" = "flesch_kincaid_grade",
    "ARI"                 = "automated_readability_index",
    "Linsear Write"       = "linsear_write",
    "Gunning Fog"         = "gunning_fog",
    "SMOG"                = "smog",
    "Coleman–Liau"        = "coleman_liau"
  )
  
  df_g <- tibble(
    index = names(idx_names),
    value = as.numeric(metrics_row[1, unname(idx_names)])
  )
  
  if ("spache_original" %in% names(metrics_row)) df_g <- bind_rows(df_g, tibble(index="Spache (original)", value=metrics_row$spache_original))
  if ("spache_revised"  %in% names(metrics_row)) df_g <- bind_rows(df_g, tibble(index="Spache (revised)",  value=metrics_row$spache_revised))
  
  df_g <- df_g |>
    mutate(
      band = as.character(grade_band(value)),
      label = ifelse(is.na(value), "NA", paste0(round(value, 1), " — ", band)),
      index = factor(index, levels = rev(index))
    )
  
  flesch_bands <- tibble(
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
  
  grade_bands <- tibble(
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
               fill = COL_POINT_MAIN, colour = "white", stroke = 1.1, na.rm = TRUE) +
    geom_label(aes(x = value, y = index, label = label),
               hjust = -0.05, fill = "white", alpha = 0.95, label.size = 0.25, na.rm = TRUE) +
    scale_fill_manual(values = PAL_GRADE) +
    scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
    labs(title = "Grade-level indices (higher = harder)",
         subtitle = "NA indicates metric not computed (e.g., very short text)",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  list(p_flesch = p1, p_grade = p2)
}

plot_compare <- function(metrics_tbl) {
  df <- metrics_tbl |>
    select(
      doc_id, doc_name,
      flesch_reading_ease,
      flesch_kincaid_grade,
      automated_readability_index,
      linsear_write,
      gunning_fog,
      smog,
      coleman_liau,
      lix,
      rix
    ) |>
    tidyr::pivot_longer(
      cols = -c(doc_id, doc_name),
      names_to = "metric",
      values_to = "value"
    ) |>
    mutate(
      metric = recode(metric,
                      flesch_reading_ease = "Flesch Reading Ease (↑ easier)",
                      flesch_kincaid_grade = "Flesch–Kincaid Grade (↑ harder)",
                      automated_readability_index = "ARI (↑ harder)",
                      linsear_write = "Linsear Write (↑ harder)",
                      gunning_fog = "Gunning Fog (↑ harder)",
                      smog = "SMOG (↑ harder)",
                      coleman_liau = "Coleman–Liau (↑ harder)",
                      lix = "LIX (↑ harder)",
                      rix = "RIX (↑ harder)"
      ),
      doc_name = factor(doc_name, levels = unique(doc_name))
    )
  
  ggplot(df, aes(x = doc_name, y = value, fill = doc_name)) +
    geom_col(width = 0.75, alpha = 0.9, na.rm = TRUE) +
    facet_wrap(~metric, scales = "free_y", ncol = 2) +
    geom_text(aes(label = ifelse(is.na(value), "NA", round(value, 1))),
              vjust = -0.4, size = 3.6, na.rm = TRUE) +
    labs(title = "Compare readability indices across documents", x = NULL, y = NULL) +
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
    geom_point(data = fry$per_sample,
               aes(x = syllables_per_100, y = sentences_per_100),
               size = 4, shape = 21, stroke = 1.2, fill = "#00C2FF", colour = "#111111"
    ) +
    geom_text(data = fry$per_sample,
              aes(x = syllables_per_100, y = sentences_per_100, label = sample),
              vjust = -1, size = 4, colour = "#003B73"
    ) +
    geom_point(data = data.frame(x=x, y=y),
               aes(x = x, y = y),
               size = 6, shape = 21, stroke = 1.3, fill = COL_POINT_MAIN, colour = "white"
    ) +
    geom_label(data = data.frame(x=x, y=y, label=label),
               aes(x = x, y = y, label = label),
               vjust = -1.2, label.size = 0.25, alpha = 0.95, fill = "white"
    )
}

plot_corr_heatmap <- function(metrics_tbl) {
  cols <- c("flesch_reading_ease","flesch_kincaid_grade","automated_readability_index",
            "linsear_write","gunning_fog","smog","coleman_liau","lix","rix")
  m <- metrics_tbl |> select(any_of(cols))
  # correlation needs >=2 rows and non-NA columns
  if (nrow(m) < 2) return(NULL)
  
  cmat <- suppressWarnings(cor(m, use = "pairwise.complete.obs"))
  df <- as.data.frame(as.table(cmat)) |>
    rename(x = Var1, y = Var2, r = Freq) |>
    mutate(
      x = factor(x, levels = cols),
      y = factor(y, levels = rev(cols))
    )
  
  ggplot(df, aes(x = x, y = y, fill = r)) +
    geom_tile() +
    geom_text(aes(label = ifelse(is.na(r), "", round(r, 2))), size = 3.6) +
    scale_fill_gradient2(limits = c(-1,1), oob = squish) +
    labs(title = "Correlation between readability metrics", x = NULL, y = NULL, fill = "r") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1),
          plot.title = element_text(face = "bold"))
}

plot_pca_docs <- function(metrics_tbl) {
  cols <- c("flesch_reading_ease","flesch_kincaid_grade","automated_readability_index",
            "linsear_write","gunning_fog","smog","coleman_liau","lix","rix")
  X <- metrics_tbl |> select(any_of(cols))
  if (nrow(X) < 2) return(NULL)
  
  # Keep columns with at least some variance
  keep <- sapply(X, function(v) sd(v, na.rm = TRUE) > 0)
  X <- X[, keep, drop = FALSE]
  if (ncol(X) < 2) return(NULL)
  
  # Simple impute with column means for PCA
  Xi <- as.data.frame(lapply(X, function(v) { v[is.na(v)] <- mean(v, na.rm = TRUE); v }))
  p <- prcomp(scale(Xi), center = TRUE, scale. = FALSE)
  
  df <- tibble(
    doc_name = metrics_tbl$doc_name,
    PC1 = p$x[,1],
    PC2 = p$x[,2]
  )
  
  ggplot(df, aes(x = PC1, y = PC2)) +
    geom_point(size = 4) +
    geom_text(aes(label = doc_name), vjust = -0.9, size = 4) +
    labs(title = "Documents in readability space (PCA)", x = "PC1", y = "PC2") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face="bold"))
}

plot_windowed <- function(df_win, doc_label) {
  if (is.null(df_win) || nrow(df_win) < 2) return(NULL)
  
  df_long <- df_win |>
    select(window, fre, fkg) |>
    pivot_longer(cols = c(fre, fkg), names_to = "metric", values_to = "value") |>
    mutate(metric = recode(metric, fre = "FRE (↑ easier)", fkg = "FKG (↑ harder)"))
  
  ggplot(df_long, aes(x = window, y = value)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.2) +
    facet_wrap(~metric, scales = "free_y", ncol = 1) +
    labs(title = paste0("Readability across the document: ", doc_label),
         subtitle = "Computed on consecutive word windows",
         x = "Window (in order)", y = NULL) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face="bold"))
}

plot_cred_compare <- function(cred_tbl) {
  df <- cred_tbl |>
    select(doc_name, evidence_per_1k_words, hedges_per_1k_words, boosters_per_1k_words, hype_per_1k_words) |>
    pivot_longer(cols = -doc_name, names_to = "metric", values_to = "value") |>
    mutate(
      metric = recode(metric,
                      evidence_per_1k_words = "Evidence markers / 1k words",
                      hedges_per_1k_words = "Hedges / 1k words",
                      boosters_per_1k_words = "Boosters / 1k words",
                      hype_per_1k_words = "Hype markers / 1k words"
      ),
      doc_name = factor(doc_name, levels = unique(doc_name))
    )
  
  ggplot(df, aes(x = doc_name, y = value, fill = doc_name)) +
    geom_col(width = 0.75, alpha = 0.9) +
    facet_wrap(~metric, scales = "free_y", ncol = 2) +
    geom_text(aes(label = round(value, 1)), vjust = -0.4, size = 3.6) +
    labs(title = "Credibility-style signals (heuristics)", x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 25, hjust = 1)
    )
}

###############################################################################
# UI
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
  titlePanel("Readability + Credibility Signals — Compare up to 5 PDFs"),
  
  sidebarLayout(
    sidebarPanel(
      div(class="box",
          div(class="title", "1) Input"),
          fileInput("pdfs", "Upload PDFs (1–5)", accept = c(".pdf"), multiple = TRUE),
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
          sliderInput("win_words", "Window size for across-document plot (words)", min = 200, max = 900, value = 400, step = 50),
          hr(),
          downloadButton("download_csv", "Download metrics CSV (all docs)")
      ),
      div(class="box",
          div(class="title", "2) Quick notes"),
          div(class="small",
              "• Flesch: higher = easier.\n",
              "• Grade-like indices: higher = harder.\n",
              "• Credibility signals are heuristics (not fact-checking).")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Compare",
                 div(class="box", plotOutput("p_compare", height = 560)),
                 div(class="box", DTOutput("compare_table"))
        ),
        tabPanel("Selected doc dashboard",
                 div(class="box", plotOutput("p_flesch", height = 260)),
                 div(class="box", plotOutput("p_grade",  height = 420))
        ),
        tabPanel("Fry (selected doc)",
                 div(class="box", plotOutput("p_fry", height = 450)),
                 div(class="box", DTOutput("fry_samples"))
        ),
        tabPanel("Across document",
                 div(class="box", plotOutput("p_windowed", height = 520)),
                 div(class="box", DTOutput("window_table"))
        ),
        tabPanel("Relationships",
                 div(class="box", plotOutput("p_corr", height = 520)),
                 div(class="box", plotOutput("p_pca", height = 420))
        ),
        tabPanel("Credibility signals",
                 div(class="box", plotOutput("p_cred", height = 560)),
                 div(class="box", DTOutput("cred_table"))
        ),
        tabPanel("Extracted text (selected preview)",
                 div(class="box", verbatimTextOutput("text_preview"))
        )
      )
    )
  )
)

###############################################################################
# Server
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
  
  texts_reactive <- reactive({
    req(input$pdfs)
    validate(need(nrow(input$pdfs) <= 5, "Please upload at most 5 PDFs."))
    
    lapply(seq_len(nrow(input$pdfs)), function(i) {
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
  })
  
  output$doc_selector_ui <- renderUI({
    req(input$pdfs)
    choices <- input$pdfs$name
    selectInput("selected_doc", "Select document for detailed view",
                choices = choices, selected = choices[[1]])
  })
  
  all_results_reactive <- reactive({
    docs <- texts_reactive()
    fam <- familiar_words_reactive()
    
    metrics_list <- list()
    fry_map <- list()
    text_map <- list()
    win_map <- list()
    cred_list <- list()
    
    for (d in docs) {
      b <- flesch_fkg_ari(d$text)
      lw <- linsear_write(d$text)
      ex <- extra_indices(d$text)
      fry <- fry_stats(d$text)
      
      sp_o <- sp_r <- NULL
      if (!is.null(fam)) {
        sp_o <- spache(d$text, fam, revised = FALSE)
        sp_r <- spache(d$text, fam, revised = TRUE)
      }
      
      cred <- cred_signals(d$text)
      
      row <- b |>
        bind_cols(lw) |>
        bind_cols(ex) |>
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
      cred_list[[d$doc_id]] <- tibble(doc_name = d$doc_name) |> bind_cols(cred)
      
      # windowed stored separately (depends on input$win_words; computed later per selected)
    }
    
    metrics_tbl <- bind_rows(metrics_list) |> select(doc_id, doc_name, everything())
    cred_tbl <- bind_rows(cred_list)
    
    list(metrics = metrics_tbl, fry_map = fry_map, text_map = text_map, cred = cred_tbl)
  })
  
  selected_row_reactive <- reactive({
    res <- all_results_reactive()
    req(input$selected_doc)
    row <- res$metrics |> filter(doc_name == input$selected_doc)
    validate(need(nrow(row) == 1, "Selected document not found."))
    row[1, ]
  })
  
  # Compare
  output$p_compare <- renderPlot({
    res <- all_results_reactive()
    plot_compare(res$metrics)
  })
  
  output$compare_table <- renderDT({
    res <- all_results_reactive()
    tbl <- res$metrics |>
      select(
        doc_name,
        flesch_reading_ease,
        flesch_kincaid_grade,
        automated_readability_index,
        linsear_write,
        gunning_fog,
        smog,
        coleman_liau,
        lix,
        rix,
        pct_complex_words,
        long_words_7plus,
        fry_avg_syllables_per_100,
        fry_avg_sentences_per_100,
        fry_samples_used,
        if ("spache_original" %in% names(res$metrics)) spache_original else NULL,
        if ("spache_revised"  %in% names(res$metrics)) spache_revised  else NULL
      )
    datatable(tbl, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Selected dashboard
  output$p_flesch <- renderPlot({
    row <- selected_row_reactive()
    plot_dashboard_one(row)$p_flesch
  })
  
  output$p_grade <- renderPlot({
    row <- selected_row_reactive()
    plot_dashboard_one(row)$p_grade
  })
  
  # Fry
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
  
  # Windowed
  output$p_windowed <- renderPlot({
    res <- all_results_reactive()
    req(input$selected_doc)
    txt <- res$text_map[[input$selected_doc]]
    w <- windowed_readability(txt, window_words = input$win_words)
    validate(need(!is.null(w), "Not enough text for windowed readability (try smaller window or longer document)."))
    plot_windowed(w, input$selected_doc)
  })
  
  output$window_table <- renderDT({
    res <- all_results_reactive()
    req(input$selected_doc)
    txt <- res$text_map[[input$selected_doc]]
    w <- windowed_readability(txt, window_words = input$win_words)
    validate(need(!is.null(w), "Not enough text for windowed readability."))
    datatable(w, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Relationships
  output$p_corr <- renderPlot({
    res <- all_results_reactive()
    p <- plot_corr_heatmap(res$metrics)
    validate(need(!is.null(p), "Upload at least 2 PDFs to compute correlations."))
    p
  })
  
  output$p_pca <- renderPlot({
    res <- all_results_reactive()
    p <- plot_pca_docs(res$metrics)
    validate(need(!is.null(p), "Upload at least 2 PDFs (and enough metrics variance) for PCA."))
    p
  })
  
  # Credibility signals
  output$p_cred <- renderPlot({
    res <- all_results_reactive()
    plot_cred_compare(res$cred)
  })
  
  output$cred_table <- renderDT({
    res <- all_results_reactive()
    datatable(res$cred, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Text preview
  output$text_preview <- renderText({
    res <- all_results_reactive()
    req(input$selected_doc)
    txt <- res$text_map[[input$selected_doc]]
    validate(need(!is.null(txt), "No text found."))
    substr(txt, 1, 3000)
  })
  
  # Download CSV
  output$download_csv <- downloadHandler(
    filename = function() paste0("readability_plus_signals_", Sys.Date(), ".csv"),
    content = function(file) {
      res <- all_results_reactive()
      out <- res$metrics |> left_join(res$cred, by = "doc_name")
      write.csv(out, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)


