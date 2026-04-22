# Shiny app for Expert Elicitation Aggregation (PERT + Linear Opinion Pool)
suppressPackageStartupMessages({
  library(shiny)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(scales)
})

############################################################################# 
#APP THEME (on-screen)
#############################################################################
theme_app <- theme_bw() +
  theme(axis.title   = element_text(size=24, family="sans", colour="black"),
        axis.text.x  = element_text(size=20, family="sans", colour="black"),
        axis.text.y  = element_text(size=20, family="sans", colour="black"),
        strip.text   = element_text(size=20, family="sans", colour="black"),
        plot.title   = element_text(size=24, family="sans", colour="black"),
        panel.border = element_rect(colour="black"),
        legend.position = "none")

# Apply it app-wide
theme_set(theme_app)

#############################################################################
#EXPORT THEME (smaller text)
#############################################################################
theme_export <- theme_bw() +
  theme(axis.title   = element_text(size=11,   family="sans", colour="black"),
        axis.text.x  = element_text(size=10.5, angle=45,vjust=0.7, family="sans", colour="black"),
        axis.text.y  = element_text(size=10.5, family="sans", colour="black"),
        strip.text   = element_text(size=11,   family="sans", colour="black"),
        plot.title   = element_text(size=11,   family="sans", colour="black"),
        panel.border = element_rect(colour="black"),
        legend.position = "none")

############################################################################# 
#Core engine: summarize a single question (hard-bounds PERT)
############################################################################
summarize_question_pert <- function(df,
                                    id_col  = "Participant",
                                    lpp_col = "Lowest_Plausible_Pr",
                                    bgp_col = "Best_Guess_Pr",
                                    hpp_col = "Highest_Plausible_Pr",
                                    dob_col = "Degree_of_Belief",          
                                    lambda  = 4,
                                    Nsim    = 40000,
                                    grid    = seq(0, 1, length.out = 1000),
                                    seed    = NULL,
                                    question_label = NULL) {
  # Use a random seed if none provided
  if (is.null(seed)) seed <- sample.int(1e9, 1)
  set.seed(seed)
  
  # Standardize columns (string-safe)
  df2 <- df %>%
    transmute(
      id = .data[[id_col]],
      a  = .data[[lpp_col]],
      m  = .data[[bgp_col]],
      b  = .data[[hpp_col]]
    ) %>%
    mutate(
      a = pmin(a, b),
      b = pmax(a, b),
      m = pmin(pmax(m, a + 1e-8), b - 1e-8),
      alpha = 1 + lambda * (m - a) / (b - a),
      beta  = 1 + lambda * (b - m) / (b - a)
    )
  
  # Degree of Belief weights:
  # If a DoB column is provided, parse it (50–100 scale).
  # When DoB is not provided or is missing, full confidence (100) is assumed,
  # interpreting absence as acceptance of stated choices rather than uncertainty.
  if (!is.null(dob_col) && dob_col %in% names(df)) {
    raw_dob <- suppressWarnings(as.numeric(df[[dob_col]]))
    raw_dob <- pmax(pmin(raw_dob, 100), 0)   # clamp to [0, 100]
    raw_dob[is.na(raw_dob)] <- 100           # missing = full confidence
    df2$dob <- raw_dob
  } else {
    df2$dob <- 100                           # uniform if no column supplied
  }
  
  # Normalize to sum-to-1 sampling probabilities
  w <- df2$dob / sum(df2$dob)
  
  # Equal-weight mixture
  idx_eq <- sample.int(nrow(df2), Nsim, replace = TRUE)
  samples_eq <- df2$a[idx_eq] + (df2$b[idx_eq] - df2$a[idx_eq]) *
    rbeta(Nsim, df2$alpha[idx_eq], df2$beta[idx_eq])
  
  # DoB-weighted mixture
  idx_w <- sample.int(nrow(df2), Nsim, replace = TRUE, prob = w)
  samples_w <- df2$a[idx_w] + (df2$b[idx_w] - df2$a[idx_w]) *
    rbeta(Nsim, df2$alpha[idx_w], df2$beta[idx_w])
  
  # Keep 'samples' as equal-weight for backward compatability with existing plots
  samples <- samples_eq
  
  # Vectorized individual densities (carries dob for weighted pool)
  dens_individual <- df2 %>%
    select(id, a, b, alpha, beta, dob) %>%
    crossing(p = grid) %>%
    mutate(
      density = if_else(
        p >= a & p <= b,
        dbeta((p - a) / (b - a), alpha, beta) / (b - a),
        0
      )
    )
  
  # Equal-weight Linear Opinion Pool
  dens_mixture <- dens_individual %>%
    group_by(p) %>%
    summarise(density = mean(density), .groups = "drop")
  
  # DoB-weighted Linear Opinion Pool
  dens_mixture_w <- dens_individual %>%
    group_by(p) %>%
    summarise(density = weighted.mean(density, w = dob), .groups = "drop")
  
  # Moment-matched Beta: equal-weight
  m_hat <- mean(samples_eq)
  v_hat <- var(samples_eq)
  if (!is.finite(v_hat) || v_hat <= 1e-12) v_hat <- 1e-6
  ab_term <- max(m_hat * (1 - m_hat) / v_hat - 1, 2)
  alpha_star <- m_hat * ab_term
  beta_star  <- (1 - m_hat) * ab_term
  grid_beta  <- seq(0, 1, length.out = 1000)
  df_beta_fit <- data.frame(p = grid_beta, density = dbeta(grid_beta, alpha_star, beta_star))
  
  # Moment-matched Beta: DoB-weighted
  m_hat_w <- mean(samples_w)
  v_hat_w <- var(samples_w)
  if (!is.finite(v_hat_w) || v_hat_w <= 1e-12) v_hat_w <- 1e-6
  ab_term_w    <- max(m_hat_w * (1 - m_hat_w) / v_hat_w - 1, 2)
  alpha_star_w <- m_hat_w * ab_term_w
  beta_star_w  <- (1 - m_hat_w) * ab_term_w
  df_beta_fit_w <- data.frame(p = grid_beta, density = dbeta(grid_beta, alpha_star_w, beta_star_w))
  
  # Summary table
  summary_tbl <- tibble(
    Question              = if (is.null(question_label)) NA_character_ else question_label,
    # Equal-weight columns
    EqW_Mean              = m_hat,
    EqW_Median            = median(samples_eq),
    EqW_5th               = as.numeric(quantile(samples_eq, 0.05)),
    EqW_95th              = as.numeric(quantile(samples_eq, 0.95)),
    # DoB-weighted columns
    DoB_Mean              = m_hat_w,
    DoB_Median            = median(samples_w),
    DoB_5th               = as.numeric(quantile(samples_w, 0.05)),
    DoB_95th              = as.numeric(quantile(samples_w, 0.95)),
    # Shared
    Hard_Union_LPP        = min(df2$a),
    Hard_Union_HPP        = max(df2$b),
    N_Participants        = nrow(df2),
    Lambda                = lambda,
    Nsim                  = Nsim
  )
  
  # Cumulative density functions (CDF)
  cdf_individual <- df2 %>%
    select(id, a, b, alpha, beta, dob) %>%
    crossing(p = grid) %>%
    mutate(
      cdf = case_when(
        p <= a ~ 0,
        p >= b ~ 1,
        TRUE   ~ pbeta((p - a) / (b - a), alpha, beta)
      )
    )
  
  # Equal-weight CDF mixture
  cdf_mixture <- cdf_individual %>%
    group_by(p) %>%
    summarise(cdf = mean(cdf), .groups = "drop")
  
  # DoB-weighted CDF mixture
  cdf_mixture_w <- cdf_individual %>%
    group_by(p) %>%
    summarise(cdf = weighted.mean(cdf, w = dob), .groups = "drop")
  
  ec             <- ecdf(samples_eq)
  ec_w           <- ecdf(samples_w)
  cdf_emp        <- data.frame(p = grid, cdf = ec(grid))
  cdf_emp_w      <- data.frame(p = grid, cdf = ec_w(grid))
  cdf_beta_fit   <- data.frame(p = grid_beta, cdf = pbeta(grid_beta, alpha_star,   beta_star))
  cdf_beta_fit_w <- data.frame(p = grid_beta, cdf = pbeta(grid_beta, alpha_star_w, beta_star_w))
  
  # Facet-ready helper
  add_q <- function(df_fac) {
    df_fac %>%
      mutate(Question = if (is.null(question_label)) NA_character_ else question_label) %>%
      relocate(Question, .before = 1)
  }
  
  list(
    summary              = summary_tbl,
    densities_individual = dens_individual,
    density_mixture      = dens_mixture,
    density_mixture_w    = dens_mixture_w,
    samples              = samples_eq,
    samples_w            = samples_w,
    cdfs                 = list(individual   = cdf_individual,
                                mixture      = cdf_mixture,
                                mixture_w    = cdf_mixture_w,
                                empirical    = cdf_emp,
                                empirical_w  = cdf_emp_w,
                                beta_fit     = cdf_beta_fit,
                                beta_fit_w   = cdf_beta_fit_w),
    facet                = list(
      mixture        = add_q(dens_mixture),
      mixture_w      = add_q(dens_mixture_w),
      individual     = add_q(dens_individual),
      beta           = add_q(df_beta_fit),
      beta_w         = add_q(df_beta_fit_w),
      cdf_mixture    = add_q(cdf_mixture),
      cdf_mixture_w  = add_q(cdf_mixture_w),
      cdf_emp        = add_q(cdf_emp),
      cdf_emp_w      = add_q(cdf_emp_w),
      cdf_beta       = add_q(cdf_beta_fit),
      cdf_beta_w     = add_q(cdf_beta_fit_w),
      cdf_individual = add_q(cdf_individual)
    ),
    beta_fit_params      = list(alpha = alpha_star,   beta = beta_star),
    beta_fit_params_w    = list(alpha = alpha_star_w, beta = beta_star_w)
  )
}

############################################################################# 
# Demo dataset used if no upload
############################################################################
set.seed(123)  # reproducible demo; remove if you want fresh draws each run

bimodal_question <- "2"   
bimodal_shift    <- 0.06  # mode separation

demo_df <- tidyr::crossing(
  Question    = c("1","2","3","4","5","6","7","8"),
  Participant = paste0("P", 1:6)
) %>%
  group_by(Question) %>%
  mutate(
    # Base center differs by leading digit
    ctr_base = dplyr::case_when(
      grepl("^1", Question) ~ 0.25,
      grepl("^2", Question) ~ 0.55,
      grepl("^3", Question) ~ 0.35,
      grepl("^4", Question) ~ 0.15,
      grepl("^5", Question) ~ 0.45,
      grepl("^6", Question) ~ 0.60,
      grepl("^8", Question) ~ 0.20,
      TRUE                  ~ 0.40
    ),
    
    # Participant-level jitter around the center (controls mild disagreement in location)
    ctr_jitter = rnorm(n(), mean = 0, sd = 0.04),
    
    # Optional bimodality: for the chosen question, split participants into two clusters
    cluster_sign = if_else(
      Question == bimodal_question,
      # half negatives, half positives, randomly permuted
      sample(rep(c(-1, 1), length.out = n())),
      0  # for all other questions, no cluster shift
    ),
    
    ctr_raw = ctr_base + ctr_jitter + cluster_sign * bimodal_shift,
    
    # Keep centers inside (0.02, 0.98) to leave room for LPP/HPP
    ctr = pmin(pmax(ctr_raw, 0.02), 0.98),
    
    # Widths vary per participant (heterogeneous uncertainty)
    w_lo = runif(n(), 0.06, 0.12),   # below center
    w_hi = runif(n(), 0.06, 0.12),   # above center
    
    # Small chance a participant is noticeably wider or tighter
    width_bump = case_when(
      runif(n()) < 0.10 ~ 1.5,  # ~10% wider
      runif(n()) < 0.10 ~ 0.7,  # ~10% tighter
      TRUE                     ~ 1.0
    ),
    
    LPP_raw = ctr - width_bump * w_lo,
    HPP_raw = ctr + width_bump * w_hi,
    
    # Clip to [0,1] and enforce a minimal span
    LPP  = pmax(LPP_raw, 0),
    HPP  = pmin(HPP_raw, 1),
    span = pmax(HPP - LPP, 0.02),
    
    # Best guess near ctr, allowed to drift a bit within [LPP, HPP]
    BGP_raw = ctr + rnorm(n(), 0, 0.02),
    BGP = pmin(pmax(BGP_raw, LPP + 1e-3), HPP - 1e-3)
  ) %>%
  ungroup() %>%
  transmute(
    Question, Participant,
    Lowest_Plausible_Pr   = LPP,
    Best_Guess_Pr         = BGP,
    Highest_Plausible_Pr  = HPP
  ) %>%
  # Add demo Degree of Belief scores (0-100); varied per participant
  mutate(
    Degree_of_Belief = sample(c(50, 60, 70, 80, 90, 95), n(), replace = TRUE)
  )

############################################################################# 
# Choose a reasonable default for "Question" column if multiple exist
############################################################################
best_question_col <- function(df) {
  cols <- names(df)
  cand <- grep("^question(\\.{3}\\d+)?$", tolower(cols), value = TRUE)
  if (length(cand) == 0) {
    cand <- grep("^q(uestion)?", tolower(cols), value = TRUE)
  }
  if (length(cand) <= 1) {
    return(ifelse(length(cand) == 1, cand, "<none>"))
  }
  # Prefer the one with most non-missing + unique values
  score <- sapply(cand, function(cc) {
    v <- df[[cc]]
    sum(!is.na(v)) + 0.001 * length(unique(na.omit(v)))
  })
  cand[order(score, decreasing = TRUE)][1]
}

############################################################################# 
# User interface
############################################################################
ui <- fluidPage(
  titlePanel("Expert Elicitation Application"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV", accept = c(".csv")),
      checkboxInput("use_demo", "Use demo data instead of upload", value = TRUE),
      tags$hr(),
      uiOutput("colmap_ui"),
      numericInput("lambda", "PERT shape (lambda)", value = 4, min = 1, step = 1),
      numericInput("Nsim", "Mixture draws", value = 10000, min = 1000, step = 1000),
      uiOutput("question_filter_ui"),  # multi-select
      checkboxInput("show_individual", "Show individual curves in overlays", TRUE),
      checkboxInput("show_beta",       "Show Beta fit overlay", TRUE),
      checkboxInput("show_dob_mix",    "Show DoB-weighted mixture overlay", TRUE),
      actionButton("run", "Run / Refresh", class = "btn-primary"),
      tags$hr(),
      downloadButton("download_summary", "Download Summary (CSV)"),
      tags$hr(),
      h4("Download plots"),
      downloadButton("download_participants_png",  "Expert Scores (PNG)"),
      downloadButton("download_density_png",  "Expert Distributions (PNG)"),
      downloadButton("download_hist_png",     "Mixture Histograms (PNG)"),
      downloadButton("download_cdf_png",      "CDFs (PNG)"),
      br(), br(),
      downloadButton("download_all_plots_zip","All Summaries (ZIP)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Expert Scores", plotOutput("plot_participants", height = "600px")),
        tabPanel("Expert Distributions", plotOutput("plot_density", height = "600px")),
        tabPanel("Mixture Histograms", plotOutput("plot_hist", height = "600px")),
        tabPanel("CDF Comparisons", plotOutput("plot_cdf", height = "600px")),
        tabPanel("Summary Table", div(style = "font-size: 20px;", 
                                      tableOutput("summary_table")),
                 helpText("EqW = equal-weight mixture; DoB = Degree-of-Belief weighted mixture. Missing DoB values default to the group mean."))
      )
    )
  )
)

############################################################################# 
# Server
###########################################################################
server <- function(input, output, session) {
  
  # Load data (reactive) and clean names without emitting "New names" message
  raw_df <- reactive({
    if (isTRUE(input$use_demo) || is.null(input$file)) {
      demo_df
    } else {
      # Keep names "as-is" (no repair), then clean quietly ourselves
      df <- suppressMessages(
        read_csv(input$file$datapath, show_col_types = FALSE, name_repair = "minimal")
      )
    }
  })
  
  # Column mapping UI
  output$colmap_ui <- renderUI({
    req(raw_df())
    df   <- raw_df()
    cols <- names(df)
    default_question <- best_question_col(df)
    
    tagList(
      selectInput("col_question", "Question column", choices = c("Question", cols),
                  selected = if (default_question %in% cols) default_question else "<none>"),
      selectInput("col_id", "Participant ID column", choices = cols,
                  selected = if ("Participant" %in% cols) "Participant" else cols[1]),
      selectInput("col_lpp", "Lowest plausible (LPP)", choices = cols,
                  selected = grep("Lowest|LPP", cols, ignore.case = TRUE, value = TRUE)[1]),
      selectInput("col_bgp", "Best guess (BGP)", choices = cols,
                  selected = grep("Best|BGP", cols, ignore.case = TRUE, value = TRUE)[1]),
      selectInput("col_hpp", "Highest plausible (HPP)", choices = cols,
                  selected = grep("Highest|HPP", cols, ignore.case = TRUE, value = TRUE)[1]),
      selectInput("col_dob", "Degree of Belief (DoB; optional)", choices = c("<none>", cols),
                  selected = {
                    hit <- grep("Belief|DoB", cols, ignore.case = TRUE, value = TRUE)
                    if (length(hit) > 0) hit[1] else "<none>"
                  })
    )
  })
  
  # Multi-question filter UI (select many or "All")
  output$question_filter_ui <- renderUI({
    req(raw_df(), input$col_question)
    if (identical(input$col_question, "<none>")) return(NULL)
    qs <- sort(unique(raw_df()[[input$col_question]]))
    selectInput("question_multi", "Questions to display",
                choices = c("All", qs), selected = "All", multiple = TRUE)
  })
  
  # Run/Refresh
  results <- eventReactive(input$run, {
    df <- raw_df()
    req(input$col_id, input$col_lpp, input$col_bgp, input$col_hpp)
    
    # Attach a question column if none provided
    has_q <- !identical(input$col_question, "<none>")
    if (!has_q) {
      df <- df %>% mutate(`__Question__` = "Q1")
    }
    q_col <- if (has_q) input$col_question else "__Question__"
    
    # If one or more questions chosen (not "All"), filter to those
    if (has_q && !is.null(input$question_multi)) {
      sel <- setdiff(input$question_multi, "All")
      if (length(sel) > 0) {
        df <- df %>% filter(.data[[q_col]] %in% sel)
      }
    }
    
    qs <- unique(df[[q_col]])
    
    # Summarize pre-selected questions (random seed each run)
    res_list <- lapply(qs, function(q) {
      df_q <- df %>% filter(.data[[q_col]] == q)
      dob_col_val <- if (!is.null(input$col_dob) && !identical(input$col_dob, "<none>"))
        input$col_dob else NULL
      summarize_question_pert(
        df_q,
        id_col  = input$col_id,
        lpp_col = input$col_lpp,
        bgp_col = input$col_bgp,
        hpp_col = input$col_hpp,
        dob_col = dob_col_val,
        lambda  = input$lambda,
        Nsim    = input$Nsim,
        seed    = NULL,
        question_label = as.character(q)
      )
    })
    names(res_list) <- as.character(qs)
    
    # Bind everything
    summary_all    <- bind_rows(lapply(res_list, `[[`, "summary"))
    dens_mix_all   <- bind_rows(lapply(res_list, function(r) r$facet$mixture))
    dens_mix_w_all <- bind_rows(lapply(res_list, function(r) r$facet$mixture_w))
    dens_ind_all   <- bind_rows(lapply(res_list, function(r) r$facet$individual))
    beta_all       <- bind_rows(lapply(res_list, function(r) r$facet$beta))
    beta_w_all     <- bind_rows(lapply(res_list, function(r) r$facet$beta_w))
    
    cdf_mix_all    <- bind_rows(lapply(res_list, function(r) r$facet$cdf_mixture))
    cdf_mix_w_all  <- bind_rows(lapply(res_list, function(r) r$facet$cdf_mixture_w))
    cdf_emp_all    <- bind_rows(lapply(res_list, function(r) r$facet$cdf_emp))
    cdf_emp_w_all  <- bind_rows(lapply(res_list, function(r) r$facet$cdf_emp_w))
    cdf_beta_all   <- bind_rows(lapply(res_list, function(r) r$facet$cdf_beta))
    cdf_beta_w_all <- bind_rows(lapply(res_list, function(r) r$facet$cdf_beta_w))
    cdf_ind_all    <- bind_rows(lapply(res_list, function(r) r$facet$cdf_individual))
    
    samples_all  <- bind_rows(lapply(names(res_list), function(nm) {
      tibble(Question = nm, samples = res_list[[nm]]$samples)
    }))
    
    list(
      summaries      = summary_all,
      dens_mix_all   = dens_mix_all,
      dens_mix_w_all = dens_mix_w_all,
      dens_ind_all   = dens_ind_all,
      beta_all       = beta_all,
      beta_w_all     = beta_w_all,
      cdf_mix_all    = cdf_mix_all,
      cdf_mix_w_all  = cdf_mix_w_all,
      cdf_emp_all    = cdf_emp_all,
      cdf_emp_w_all  = cdf_emp_w_all,
      cdf_beta_all   = cdf_beta_all,
      cdf_beta_w_all = cdf_beta_w_all,
      cdf_ind_all    = cdf_ind_all,
      samples_all    = samples_all
    )
  }, ignoreInit = TRUE)
  
  ##########################################################################
  # Helpers: rebuild the ggplots from results() and inputs
  ##########################################################################
  # Build RAW LPP / BGP / HPP per participant, faceted by Question
  build_individuals_plot <- function(df_raw,
                                     id_col, lpp_col, bgp_col, hpp_col,
                                     question_col = NULL,
                                     selected_questions = NULL,
                                     use_export_theme = FALSE,
                                     theme_export = NULL, 
                                     facet_cols = 4) {
    # Ensure we have a Question column (or synthesize one)
    has_q <- !is.null(question_col) && !identical(question_col, "<none>")
    if (!has_q) {
      q_col <- "__Question__"
      df <- df_raw %>% mutate(`__Question__` = "Q1")
    } else {
      q_col <- question_col
      df <- df_raw
    }
    
    # Filter to the selected subset of questions (if provided)
    if (!is.null(selected_questions) && length(selected_questions) > 0) {
      # When app has an "All" option, remove it before filtering
      sel <- setdiff(selected_questions, "All")
      if (length(sel) > 0) {
        df <- df %>% filter(.data[[q_col]] %in% sel)
      }
    }
    
    # Build plotting frame using raw (unfixed) values
    dfp <- df %>%
      transmute(
        Question             = .data[[q_col]],
        Participant          = as.character(.data[[id_col]]),
        Lowest_Plausible_Pr  = suppressWarnings(as.numeric(.data[[lpp_col]])),
        Best_Guess_Pr        = suppressWarnings(as.numeric(.data[[bgp_col]])),
        Highest_Plausible_Pr = suppressWarnings(as.numeric(.data[[hpp_col]]))
      )
    
    # Long form for 3 raw points (LPP, BGP, HPP)
    df_long <- dfp %>%
      pivot_longer(
        cols = c(Lowest_Plausible_Pr, Best_Guess_Pr, Highest_Plausible_Pr),
        names_to = "Measure", values_to = "Value"
      )
    
    g <- ggplot() +
      geom_linerange(data = dfp, aes(x = Participant, 
                                     ymin = Lowest_Plausible_Pr, 
                                     ymax = Highest_Plausible_Pr),lwd = 0.5) +
      geom_point(data = df_long, aes(x = Participant, y = Value), size = 1) +
      coord_flip() + ylim(0,1)+
      labs(x = "Participant", y = "Probability") +
      facet_wrap(~ Question, ncol = facet_cols, scales = "fixed")+
      theme(axis.text.y = element_blank(), 
            axis.ticks.y = element_blank())
    
    # If you defined a smaller export theme and asked to use it, apply it
    if (isTRUE(use_export_theme) && !is.null(theme_export)) {
      g <- g + theme_export + theme(axis.text.y = element_blank(), 
                                    axis.ticks.y = element_blank())
    }
    g
  }
  
  build_density_plot <- function(r, show_individual = TRUE, show_beta = TRUE, show_dob_mix = TRUE, facet_cols = 4) {
    p <- ggplot() +
      geom_line(data = r$dens_mix_all, aes(x = p, y = density), lwd = 0.5) +
      labs(x = "Probability", y = "Density") + 
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    
    if (isTRUE(show_individual)) {
      p <- p +
        geom_line(data = r$dens_ind_all, aes(x = p, y = density, color = id),
                  alpha = 0.5, lwd = 0.5) + guides(color = "none") +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }
    
    if (isTRUE(show_dob_mix)) {
      p <- p + geom_line(data = r$dens_mix_w_all, aes(x = p, y = density),
                         color = "red", lwd = 0.5)
    }
    
    if (isTRUE(show_beta)) {
      p <- p + geom_line(data = r$beta_all, aes(x = p, y = density),
                         color = "blue", lwd = 0.5, lty = "dashed")
    }
    
    p + facet_wrap(~ Question, ncol = facet_cols, scales = "fixed") + theme_export +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) 
  }
  
  build_hist_plot <- function(r, show_beta = TRUE, show_dob_mix = TRUE, facet_cols = 4) {
    p <- ggplot() +
      geom_histogram(data = r$samples_all,
                     aes(x = samples, y = after_stat(density)),
                     bins = 50, fill = "grey85", color = "white") +
      geom_line(data = r$dens_mix_all, aes(x = p, y = density), lwd = 0.5) +
      labs(x = "Probability", y = "Density") + 
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    
    if (isTRUE(show_dob_mix)) {
      p <- p + geom_line(data = r$dens_mix_w_all, aes(x = p, y = density),
                         color = "red", lwd = 0.5)
    }
    if (isTRUE(show_beta)) {
      p <- p + geom_line(data = r$beta_all, aes(x = p, y = density),
                         color = "blue", lwd = 0.5, lty = "dashed")
    }
    p + facet_wrap(~ Question, ncol = facet_cols) + theme_export +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }
  
  build_cdf_plot <- function(r, show_individual = TRUE, show_beta = TRUE, show_dob_mix = TRUE, facet_cols = 4) {
    p <- ggplot() +
      geom_line(data = r$cdf_mix_all, aes(p, cdf), lwd = 0.5) +
      #geom_line(data = r$cdf_emp_all, aes(p, cdf),
      #          color = "darkgrey", lwd = 0.5, lty = "dotdash") +
      labs(x = "Probability", y = "Cumulative probability")
    
    if (isTRUE(show_dob_mix)) {
      p <- p + geom_line(data = r$cdf_mix_w_all, aes(p, cdf),
                         color = "red", lwd = 0.5)
    }
    if (isTRUE(show_beta)) {
      p <- p + geom_line(data = r$cdf_beta_all, aes(p, cdf),
                         color = "blue", lwd = 0.5, lty = "dashed")
    }
    if (isTRUE(show_individual)) {
      p <- p +
        geom_line(data = r$cdf_ind_all, aes(p, cdf, color = id),
                  alpha = 0.5, lwd = 0.5) + guides(color = "none")
    }
    
    p + facet_wrap(~ Question, ncol = facet_cols) + theme_export
  }
  
  ###########################################################################
  # Single PNG downloads (use current selections in results()) 
  ###########################################################################
  output$download_participants_png <- downloadHandler(
    filename = function() paste0("participants_raw_", Sys.Date(), ".png"),
    content = function(file) {
      req(results())
      df0 <- raw_df()
      req(input$col_id, input$col_lpp, input$col_bgp, input$col_hpp)
      
      has_q <- !identical(input$col_question, "<none>")
      if (!has_q) {
        q_col <- "__Question__"
        df0 <- df0 %>% mutate(`__Question__` = "Q1")
      } else {
        q_col <- input$col_question
      }
      if (has_q && !is.null(input$question_multi)) {
        sel <- setdiff(input$question_multi, "All")
        if (length(sel) > 0) {
          df0 <- df0 %>% filter(.data[[q_col]] %in% sel)
        }
      }
      
      dfp <- df0 %>%
        transmute(
          Question             = .data[[q_col]],
          Participant          = as.character(.data[[input$col_id]]),
          Lowest_Plausible_Pr  = suppressWarnings(as.numeric(.data[[input$col_lpp]])),
          Best_Guess_Pr        = suppressWarnings(as.numeric(.data[[input$col_bgp]])),
          Highest_Plausible_Pr = suppressWarnings(as.numeric(.data[[input$col_hpp]]))
        )
      
      df_long <- dfp %>%
        tidyr::pivot_longer(
          cols = c(Lowest_Plausible_Pr, Best_Guess_Pr, Highest_Plausible_Pr),
          names_to = "Measure", values_to = "Value"
        )
      facet_cols <- if (!is.null(input$facet_cols)) input$facet_cols else 4
      
      g <- ggplot() +
        coord_flip()+
        geom_linerange(data = dfp, aes(x = Participant, ymin = Lowest_Plausible_Pr, 
                                       ymax = Highest_Plausible_Pr), lwd = 0.5) +
        geom_point(data = df_long, aes(x = Participant, y = Value), size = 1) +
        labs(x = "Participant", y = "Probability") + ylim(0,1) +
        facet_wrap(~ Question, scales = "fixed") + theme_export +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      
      ggsave(file, g, width = 8, height = 4, dpi = 1200, units='in', limitsize = FALSE)
    }
  )
  
  output$download_density_png <- downloadHandler(
    filename = function() paste0("density_", Sys.Date(), ".png"),
    content = function(file) {
      req(results())
      r <- results()
      # use the same number of columns you show in the app; adjust if you added input$facet_cols
      facet_cols <- if (!is.null(input$facet_cols)) input$facet_cols else 4
      g <- build_density_plot(r, 
                              show_individual = isTRUE(input$show_individual),
                              show_beta       = isTRUE(input$show_beta),
                              show_dob_mix    = isTRUE(input$show_dob_mix),
                              facet_cols = facet_cols)
      g_small <- g + theme_export  # <-- apply smaller export theme
      ggsave(file, g_small, width = 8, height = 4, dpi = 1200, units='in', limitsize = FALSE)
    }
  )
  
  output$download_hist_png <- downloadHandler(
    filename = function() paste0("histogram_", Sys.Date(), ".png"),
    content = function(file) {
      req(results())
      r <- results()
      facet_cols <- if (!is.null(input$facet_cols)) input$facet_cols else 4
      g <- build_hist_plot(r,
                           show_beta    = isTRUE(input$show_beta),
                           show_dob_mix = isTRUE(input$show_dob_mix),
                           facet_cols = facet_cols)
      g_small <- g + theme_export  # <-- apply smaller export theme
      ggsave(file, g_small, width = 8, height = 4, dpi = 1200, units='in', limitsize = FALSE)
    }
  )
  
  output$download_cdf_png <- downloadHandler(
    filename = function() paste0("cdf_", Sys.Date(), ".png"),
    content = function(file) {
      req(results())
      r <- results()
      facet_cols <- if (!is.null(input$facet_cols)) input$facet_cols else 4
      g <- build_cdf_plot(r, show_individual = isTRUE(input$show_individual),
                          show_beta    = isTRUE(input$show_beta),
                          show_dob_mix = isTRUE(input$show_dob_mix),
                          facet_cols = facet_cols)
      g_small <- g + theme_export  # <-- apply smaller export theme
      ggsave(file, g_small, width = 8, height = 4, dpi = 1200, units='in', limitsize = FALSE)
    }
  )
  
  ###########################################################################
  # ZIP files
  ###########################################################################
  output$download_all_plots_zip <- downloadHandler(
    filename = function() paste0("elicitation_plots_", Sys.Date(), ".zip"),
    content = function(file) {
      req(results())
      r <- results()
      
      # facet columns if you have that input; otherwise set a default
      facet_cols   <- if (!is.null(input$facet_cols)) input$facet_cols else 4
      show_ind     <- isTRUE(input$show_individual)
      show_beta    <- isTRUE(input$show_beta)
      show_dob_mix <- isTRUE(input$show_dob_mix)
      
      # Build plots (respecting current toggle states)
      g1 <- build_density_plot(r, show_individual = show_ind,
                               show_beta = show_beta, show_dob_mix = show_dob_mix,
                               facet_cols = facet_cols)
      g2 <- build_hist_plot(r, show_beta = show_beta, show_dob_mix = show_dob_mix,
                            facet_cols = facet_cols)
      g3 <- build_cdf_plot(r, show_individual = show_ind,
                           show_beta = show_beta, show_dob_mix = show_dob_mix,
                           facet_cols = facet_cols)
      g4 <- build_individuals_plot(
        df_raw            = raw_df(),
        id_col            = input$col_id,
        lpp_col           = input$col_lpp,
        bgp_col           = input$col_bgp,
        hpp_col           = input$col_hpp,
        question_col      = input$col_question,
        selected_questions= input$question_multi,
        use_export_theme  = TRUE,           # set FALSE to use app theme
        theme_export      = if (exists("theme_export")) theme_export else NULL
      )
      
      # Save all figures to a temp directory
      outdir <- tempfile("plots_")
      dir.create(outdir, showWarnings = FALSE)
      
      f1 <- file.path(outdir, paste0("density_",   Sys.Date(), ".png"))
      f2 <- file.path(outdir, paste0("histogram_", Sys.Date(), ".png"))
      f3 <- file.path(outdir, paste0("cdf_",       Sys.Date(), ".png"))
      f4 <- file.path(outdir, paste0("individuals_", Sys.Date(), ".png"))
      
      ggsave(f1, g1, width = 8, height = 4, dpi = 1200, units = 'in', limitsize = FALSE)
      ggsave(f2, g2, width = 8, height = 4, dpi = 1200, units = 'in', limitsize = FALSE)
      ggsave(f3, g3, width = 8, height = 4, dpi = 1200, units = 'in', limitsize = FALSE)
      ggsave(f4, g4, width = 8, height = 4, dpi = 1200, units = 'in', limitsize = FALSE)
      
      # Write the summary CSV
      f_summary_csv <- file.path(outdir, paste0("summary_", Sys.Date(), ".csv"))
      write_csv(r$summaries, f_summary_csv)
      
      # Zip them up
      oldwd <- setwd(outdir); on.exit(setwd(oldwd), add = TRUE)
      zip(zipfile = file, files = basename(c(f1, f2, f3, f4,f_summary_csv)))
    }
  )
  
  output$plot_participants <- renderPlot({
    req(results())     # ensures your selection/filter is set
    df0 <- raw_df()    # original (uploaded or demo) data
    req(input$col_id, input$col_lpp, input$col_bgp, input$col_hpp)
    
    # Determine the Question column; if none, create a default
    has_q <- !identical(input$col_question, "<none>")
    if (!has_q) {
      q_col <- "__Question__"
      df0 <- df0 %>% mutate(`__Question__` = "Q1")
    } else {
      q_col <- input$col_question
    }
    
    # Filter to the current multi-select of questions (if any)
    if (has_q && !is.null(input$question_multi)) {
      sel <- setdiff(input$question_multi, "All")
      if (length(sel) > 0) {
        df0 <- df0 %>% filter(.data[[q_col]] %in% sel)
      }
    }
    
    # Build plotting frame using mapped columns — RAW values
    dfp <- df0 %>%
      transmute(
        Question             = .data[[q_col]],
        Participant          = as.character(.data[[input$col_id]]),
        Lowest_Plausible_Pr  = suppressWarnings(as.numeric(.data[[input$col_lpp]])),
        Best_Guess_Pr        = suppressWarnings(as.numeric(.data[[input$col_bgp]])),
        Highest_Plausible_Pr = suppressWarnings(as.numeric(.data[[input$col_hpp]]))
      )
    
    # Long form for the 3 raw points (so each is visible & labeled)
    df_long <- dfp %>%
      tidyr::pivot_longer(
        cols = c(Lowest_Plausible_Pr, Best_Guess_Pr, Highest_Plausible_Pr),
        names_to = "Measure", values_to = "Value"
      )
    
    # Plot: RAW interval (LPP→HPP) + RAW points at LPP, BGP, HPP
    ggplot() +
      coord_flip()+
      geom_linerange(data = dfp, aes(x = Participant, 
                                     ymin = Lowest_Plausible_Pr, 
                                     ymax = Highest_Plausible_Pr),lwd = 1) +
      geom_point(data = df_long, aes(x = Participant, y = Value), size = 2) +
      labs(x = "Participant", y = "Probability") + ylim(0,1) +
      facet_wrap(~ Question, scales = "fixed") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  })
  
  # Plots: Mixture density (faceted by full label)
  output$plot_density <- renderPlot({
    req(results())
    r <- results()
    
    p <- ggplot() +
      geom_line(data = r$dens_mix_all, aes(x = p, y = density), lwd = 1) + xlim(0,1) +
      labs(x = "Probability", y = "Density",
           title = "Mixture density: equal-weight (black), DoB-weighted (red), Beta fit (blue dashed)") + 
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    
    if (isTRUE(input$show_individual)) {
      p <- p +
        geom_line(data = r$dens_ind_all, aes(x = p, y = density, color = id),
                  alpha = 0.5, lwd = 0.7) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())+
        guides(color = "none")
    }
    
    if (isTRUE(input$show_dob_mix)) {
      p <- p + geom_line(data = r$dens_mix_w_all, aes(x = p, y = density),
                         color = "red", lwd = 1)
    }
    
    if (isTRUE(input$show_beta)) {
      p <- p + geom_line(data = r$beta_all, aes(x = p, y = density),
                         color = "blue", lwd = 1, lty = "dashed")
    }
    
    p + facet_wrap(~ Question, scales = "free_y")
  })
  
  # Plots: Histogram + Beta fit (faceted by full label)
  output$plot_hist <- renderPlot({
    req(results())
    r <- results()
    
    p <- ggplot() +
      geom_histogram(data = r$samples_all,
                     aes(x = samples, y = after_stat(density)),
                     bins = 50, fill = "grey85", color = "white") +
      geom_line(data = r$dens_mix_all, aes(x = p, y = density), lwd = 1) +
      labs(x = "Probability", y = "Density",
           title = "Histogram of mixture samples") +
      theme(axis.text.y=element_blank(), axis.ticks.y = element_blank())
    
    if (isTRUE(input$show_dob_mix)) {
      p <- p + geom_line(data = r$dens_mix_w_all, aes(x = p, y = density),
                         color = "red", lwd = 1)
    }
    
    if (isTRUE(input$show_beta)) {
      p <- p + geom_line(data = r$beta_all, aes(x = p, y = density),
                         color = "blue", lwd = 1, lty = "dashed")
    }
    
    p + facet_wrap(~ Question)
  })
  
  # Plots: CDF comparison (faceted by full label)
  output$plot_cdf <- renderPlot({
    req(results())
    r <- results()
    
    p <- ggplot() +
      geom_line(data = r$cdf_mix_all, aes(p, cdf), lwd = 1) +
      #geom_line(data = r$cdf_emp_all, aes(p, cdf),
      #          color = "darkgrey", lwd = 1, lty = "dotdash") +
      labs(x = "Probability", y = "Cumulative probability",
           title = "Cumulative distribution functions: equal-weight (black), DoB-weighted (red), Beta fit (blue dashed)")
    
    if (isTRUE(input$show_dob_mix)) {
      p <- p + geom_line(data = r$cdf_mix_w_all, aes(p, cdf),
                         color = "red", lwd = 1)
    }
    
    if (isTRUE(input$show_beta)) {
      p <- p + geom_line(data = r$cdf_beta_all, aes(p, cdf),
                         color = "blue", lwd = 1, lty = "dashed")
    }
    
    if (isTRUE(input$show_individual)) {
      p <- p +
        geom_line(data = r$cdf_ind_all, aes(p, cdf, color = id),
                  alpha = 0.5, lwd = 0.7) + guides(color = "none")
    }
    
    p + facet_wrap(~ Question)
  })
  
  # Summary table & download
  output$summary_table <- renderTable({
    req(results())
    results()$summaries
  })
  
  output$download_summary <- downloadHandler(
    filename = function() paste0("elicitation_summaries_", Sys.time(),".csv"),
    content = function(file) {
      req(results())
      readr::write_csv(results()$summaries, file)
    }
  )
}

# Run app
shinyApp(ui, server)
