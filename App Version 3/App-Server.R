############################################################################# 
# Server
###########################################################################
server <- function(input, output, session) {
  
  ###########################################################################
  # DATA SOURCES AND DATA INGESTION
  # Supports three data sources:
  #   - Built-in demo dataset
  #   - User-uploaded CSV files
  #   - Google Sheets facilitator worksheet
  ###########################################################################
  sheet_data <- reactiveVal(NULL)
  last_refresh <- reactiveVal(NULL)
  n_participants <- reactiveVal(NULL)
  
  ###########################################################################
  # GOOGLE SHEETS RETRIEVAL
  ###########################################################################
  observeEvent(input$refresh_sheet, {
    tryCatch({
      dat <- read_sheet(sheet_url, sheet = "Facilitator")
      sheet_data(dat)
      last_refresh(Sys.time())
      showNotification(
        paste("Retrieved", nrow(dat), "rows from Google Sheets."),
        type = "message", duration = 4)}, error = function(e) {showNotification(
        paste("Could not retrieve Google Sheet:", e$message),
        type = "error",duration = NULL)
    })
  })
  
  ############################################################################
  # Download response template
  # Provides an example dataset illustrating the recommended structure for
  # elicitation responses, including question identifiers, question text,
  # rounds, participant estimates, and assessment confidence.
  ############################################################################
  output$download_template <- downloadHandler(
    filename = function() {
      "expert_elicitation_response_template.csv"
    },
    content = function(file) {
      template <- data.frame(
        Question = c(1, 1, 1, 1),
        Question_Type = rep("probability",4),
        Question_Text = rep(
          "Assuming current environmental and management conditions continue, what is the probability that the target population will increase by at least 20% relative to its 2025 abundance within the next 10 years across the assessment area?",
          4),
        Round = c(1, 1, 2, 2),
        Participant = c("P1", "P2", "P1", "P2"),
        Lowest_Plausible_Pr = c(0.20, 0.30, 0.25, 0.35),
        Best_Guess_Pr = c(0.40, 0.50, 0.50, 0.55),
        Highest_Plausible_Pr = c(0.60, 0.75, 0.70, 0.75),
        Assessment_Confidence = c(70, 65, 85, 75)
      )
      write.csv(template, file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )
  
  ###########################################################################
  # DATA STATUS INDICATOR
  # Displays whether expert responses have been loaded and, when available, 
  # reports the number of unique participants and the most recent refresh time.
  ###########################################################################
  output$refresh_status <- renderUI({
    if (is.null(last_refresh())) {
      div(style = "
      background-color: #f5f5f5;
      border-left: 4px solid #999999;
      padding: 12px;
      border-radius: 6px;
      margin-bottom: 10px;
      ",
        tags$strong("Waiting for data"),
        tags$br(),
        tags$span(
          style = "color: #666666;",
         "Expert responses have not been retrieved."))
    } else {
      
      # Number of unique participants, if ID column is available
      n_exp <- NULL
      if (!is.null(input$col_id) &&
          input$col_id %in% names(sheet_data())) {
        n_exp <- dplyr::n_distinct(
          sheet_data()[[input$col_id]],
          na.rm = TRUE)}
      div(style = "
        background-color: #eef7f1;
        border-left: 4px solid #3c8c5a;
        padding: 12px;
        border-radius: 6px;
        margin-bottom: 10px;
      ",
        tags$div(style = "font-weight: 600; color: #267044;",
          "● Expert responses loaded"),
        if (!is.null(n_exp)) {
          tags$div(style = "margin-top: 5px;",
                   paste(n_exp, "experts"))
          },
        tags$div(style = "font-size: 12px; color: #666666; margin-top: 3px;",
                 paste("Updated",format(last_refresh(), "%I:%M:%S %p"))
        )
      )
    }
  })
  
  ###########################################################################
  # ACTIVE DATA SOURCE
  # Returns the currently selected dataset regardless of source
  # All downstream analyses use this reactive object.
  ###########################################################################
  raw_df <- reactive({
    req(input$data_source)
    if (identical(input$data_source, "demo")) {
      df <- demo_df
    } else if (identical(input$data_source, "csv")) {
      req(input$csv_file)
      df <- read.csv(input$csv_file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
    } else if (identical(input$data_source, "google")) {
      req(sheet_data())
      df <- sheet_data()
    } else {
      return(NULL)
    }
    # Version 3 compatibility
    if (!is.null(input$col_question_type) &&
        input$col_question_type != "<none>" &&
        input$col_question_type %in% names(df)) {
      
      names(df)[names(df) == input$col_question_type] <- "Question_Type"
    } else if (!"Question_Type" %in% names(df)) {
      df$Question_Type <- "probability"
    }
    df
  })
  
  ############################################################################
  # Selected Question Display
  # Display the elicitation question above analysis outputs when a single
  # question is selected. Question text is retrieved from the optional
  # Question_Text column in the input dataset.
  ############################################################################
  question_metadata <- reactive({
    df <- raw_df()
    req(df)
    if (is.null(input$col_question) ||
        input$col_question == "<none>") {
      return(NULL)
    }
    q_col <- input$col_question
    if (!is.null(input$col_question_type) &&
        input$col_question_type != "<none>" &&
        input$col_question_type %in% names(df)) {
      df %>%
        distinct(Question = .data[[q_col]],
                 Question_Type = .data[[input$col_question_type]])
    } else {
      df %>%
        distinct( Question = .data[[q_col]]) %>%
        mutate(Question_Type = "probability")
    }
  })

  get_question_type <- function(question_id, metadata) {
    idx <- match(question_id, metadata$Question)
    if (is.na(idx)) {
      return("probability")
    }
    metadata$Question_Type[idx]
  }
  
  # -------------------------------------------------------------------------
  # Extract question text from input data
  # -------------------------------------------------------------------------
  selected_question_text <- reactive({
    req(length(input$question_multi) == 1)
    df <- raw_df()
    if (!"Question_Text" %in% names(df)) {
      return(NULL)
    }
    q_selected <- input$question_multi
    txt <- df %>%
      dplyr::filter(as.character(.data[[input$col_question]]) ==
                      as.character(q_selected)) %>%
      dplyr::pull(Question_Text) %>%
      unique()
    if (length(txt) == 0) {
      return(NULL)
    }
    txt[1]
  })
  
  # -------------------------------------------------------------------------
  # Render question panels for analysis tabs
  # -------------------------------------------------------------------------
  question_panel_ui <- reactive({
    req(length(input$question_multi) == 1)
    txt <- selected_question_text()
    if (is.null(txt)) {
      return(NULL)
    }
    div(
      style = "
      background:#f8f9fa;
      border-top: 4px solid #1F5A7A;
      border-left:4px solid #1f5a7a;
      border-right: 1px solid #dee2e6;
      border-bottom: 1px solid #dee2e6;
      padding:8px 12px;
      border-radius:4px;
      margin-bottom:12px;
      font-size:0.9em;
      width:85%;
      line-height:1.5;",
      tags$span(style = "font-weight:600;color:#1f5a7a;", strong("Question:")),
      tags$span(style = "margin-left:6px;", txt))
  })
  
  output$question_panel_expert  <- renderUI(question_panel_ui())
  output$question_panel_density <- renderUI(question_panel_ui())
  output$question_panel_hist    <- renderUI(question_panel_ui())
  output$question_panel_cdf     <- renderUI(question_panel_ui())
  output$question_panel_summary <- renderUI(question_panel_ui())
  
  ###############################################################################
  # Display Options Visibility
  ###############################################################################
  
  output$show_display_options <- reactive({
    req(raw_df())
    df <- raw_df()
    # No question selected yet
    if (is.null(input$question_multi)) {
      return(TRUE)
    }
    # Filter to selected questions
    sel <- setdiff(input$question_multi, "All")
    if (length(sel) > 0) {
      df <- df %>%
        filter(as.character(.data[[input$col_question]]) %in%
                 as.character(sel))
    }
    # Backwards compatibility
    if (!"Question_Type" %in% names(df)) {
      return(TRUE)
    }
    any(df$Question_Type %in% c("probability", "numeric"))
  })
  outputOptions(output, "show_display_options", suspendWhenHidden = FALSE)
  
  ###############################################################################
  # Beta Option Visibility
  ###############################################################################
  
  output$show_beta_option <- reactive({
    req(raw_df())
    df <- raw_df()
    if (is.null(input$question_multi)) {
      return(TRUE)
    }
    sel <- setdiff(input$question_multi, "All")
    if (length(sel) > 0) {
      df <- df %>%
        filter(as.character(.data[[input$col_question]]) %in%
            as.character(sel))
    }
    if (!"Question_Type" %in% names(df)) {
      return(TRUE)
    }
    any(df$Question_Type == "probability")
  })
  outputOptions(output, "show_beta_option", suspendWhenHidden = FALSE)
  
  ###########################################################################
  # INPUT COLUMN MAPPING
  # Automatically detects likely question identifiers, question text,
  # participant identifiers, elicitation values, assessment confidence,
  # and round columns while allowing manual override.
  ###########################################################################
  output$colmap_ui <- renderUI({
    req(raw_df())
    df <- raw_df()
    cols <- names(df)
    default_question <- best_question_col(df)
    tagList(selectInput("col_question", "Question identifier column",
                        choices = c("<none>", cols),
                        selected = if (default_question %in% cols)
                          default_question else "<none>"),
            selectInput("col_question_type", "Question type column",
                        choices = c("<none>", cols), selected = {
                          hit <- grep("^Question_Type$|Question.*Type",
                                      cols, ignore.case = TRUE, value = TRUE)
                          if (length(hit) > 0) hit[1] else "<none>"
                          }),
            selectInput("col_question_text", "Question text column (optional)",
                        choices = c("<none>", cols),
                        selected = {
                          hit <- grep("^Question_Text$|Question.*Text",
                                      cols,ignore.case = TRUE, value = TRUE)
                          if (length(hit) > 0) hit[1] else "<none>"
                          }),
            selectInput("col_response", "Response column (Yes/No questions)",
                        choices = c("<none>", cols), selected = {
                          hit <- grep("^Response$|Answer|Vote", cols,
                                      ignore.case = TRUE, value = TRUE)
                if (length(hit) > 0) hit[1] else "<none>"
              }),
            selectInput("col_option", "Ranking option column",
                        choices = c("<none>", cols), selected = {
                          hit <- grep("Option|Alternative|Item|Category", cols,
                                      ignore.case = TRUE, value = TRUE)
                if (length(hit) > 0) hit[1] else "<none>"
              }),
            selectInput("col_rank", "Rank column", choices = c("<none>", cols),
                        selected = {
                          hit <- grep("^Rank$", cols, ignore.case = TRUE,
                                      value = TRUE)
                if (length(hit) > 0) hit[1] else "<none>"
              }),
            selectInput("col_id", "Participant identifier column",
                        choices = cols,
                        selected = if ("Participant" %in% cols)
                          "Participant" else cols[1]),
            selectInput("col_lpp", "Lowest plausible (LPP)",
                        choices = cols, 
                        selected = grep( "Lowest|LPP",cols,
                                         ignore.case = TRUE, value = TRUE)[1]),
            selectInput("col_bgp", "Best guess (BGP)",
                        choices = cols,
                        selected = grep("Best|BGP", cols, 
                                        ignore.case = TRUE, value = TRUE)[1]),
            selectInput("col_hpp","Highest plausible (HPP)",
                        choices = cols,
                        selected = grep("Highest|HPP",
                                        cols, ignore.case = TRUE, value = TRUE)[1]),
            selectInput("col_AC","Assessment confidence (AC; optional)",
                        choices = c("<none>", cols),
                        selected = {
                          hit <- grep("Assessment.*Confidence|Confidence|Belief|DoB",
                                      cols,ignore.case = TRUE, value = TRUE)
                          if (length(hit) > 0) hit[1] else "<none>"}),
            selectInput("col_round", "Elicitation round (optional)",
                        choices = c("<none>", cols),
                        selected = {
                          hit <- grep("^Round$|Elicitation.*Round|Response.*Round",
                                      cols, ignore.case = TRUE,value = TRUE)
                          if (length(hit) > 0) hit[1] else "<none>"}))
  })
  
  ###########################################################################
  # QUESTION SELECTION
  # Generates the question filter based on the detected question column.
  # Multiple questions may be selected simultaneously.
  ###########################################################################
  output$question_filter_ui <- renderUI({
    req(raw_df(), input$col_question)
    if (identical(input$col_question, "<none>")) return(NULL)
    qs <- unique(raw_df()[[input$col_question]])
    qs <- qs[order(as.numeric(qs))]
    selectInput("question_multi", "Questions to display",
                choices = c("All", qs), selected = "All", multiple = TRUE)
  })
  
  output$round_filter_ui <- renderUI({
    req(raw_df())
    if (is.null(input$col_round) ||
        identical(input$col_round, "<none>")) {
      return(NULL)
    }
    rounds <- sort(unique(raw_df()[[input$col_round]]))
    selectInput("round_filter", "Round", 
                choices = c("All", rounds),
                selected = "All")
  })
  
  ###########################################################################
  # ANALYSIS ENGINE
  # Triggered when the user runs the analysis.
  # Workflow:
  #   1. Retrieve and validate input data
  #   2. Apply question filters
  #   3. Detect single-round or multi-round structure
  #   4. Generate PERT distributions
  #   5. Calculate pooled summaries
  #   6. Evaluate confidence-weighted pooling feasibility
  #   7. Assemble outputs for plotting, reporting, and export
  ###########################################################################
  results <- eventReactive(input$run, {
    df <- raw_df()
    validate(need(!is.null(df), "No data available. Please upload data or select a data source."))
    req(input$col_id, input$col_lpp, input$col_bgp, input$col_hpp)
    
    # Attach a question column if none provided
    has_q <- !identical(input$col_question, "<none>")
    if (!has_q) {
      df <- df %>% mutate(`__Question__` = "Q1")
    }
    q_col <- if (has_q) input$col_question else "__Question__"
    
    # -------------------------------------------------------------------------
    # Validate input data before analysis
    # -------------------------------------------------------------------------
    AC_col_val <- if (
      !is.null(input$col_AC) &&
      !identical(input$col_AC, "<none>")) {
      input$col_AC
    } else {
      NULL
    }
    
    round_col_val <- if (
      !is.null(input$col_round) &&
      !identical(input$col_round, "<none>")) {
      input$col_round
    } else {
      NULL
    }
    
    validation <- validate_elicitation_data(
      df = df,
      id_col = input$col_id,
      question_col = q_col,
      lpp_col = input$col_lpp,
      bgp_col = input$col_bgp,
      hpp_col = input$col_hpp,
      response_col = input$col_response,
      option_col = input$col_option,
      rank_col = input$col_rank,
      AC_col = AC_col_val,
      round_col = round_col_val)
    
    if (!validation$valid) {
      
      showModal(
        modalDialog(
          title = "Input Data Validation Failed",
          tags$p("The analysis was not run because problems were detected in the input data."),
          tags$p("Please correct the following issue(s) and run the analysis again:"),
          tags$ul(lapply(
            validation$problems, function(problem) {
                tags$li(problem)})),
          footer = modalButton("Close"), easyClose = TRUE, size = "l"))
      return(NULL)
    }
    
    # If one or more questions chosen (not "All"), filter to those
    if (has_q && !is.null(input$question_multi)) {
      sel <- setdiff(input$question_multi, "All")
      if (length(sel) > 0) {
        df <- df %>% filter(.data[[q_col]] %in% sel)
      }
    }
    qs <- unique(df[[q_col]])
    
    question_types <- question_metadata()
    
    # -------------------------------------------------------------------------
    # Single-round analysis
    # -------------------------------------------------------------------------
    if (is.null(round_col_val)) {
      res_list <- lapply(qs, function(q) {
        
        q_type <- get_question_type(as.character(q), question_types)
        df_q <- df %>%
          filter(.data[[q_col]] == q)
        
        if (q_type == "yesno") {
          
          summarize_question_yesno(
            df_q,
            id_col = input$col_id,
            response_col = input$col_response,
            AC_col = AC_col_val,
            question_label = as.character(q),
            question_type = q_type)
          
        } else if (q_type == "ranking") {
          
          summarize_question_ranking(
            df_q,
            id_col = input$col_id,
            option_col = input$col_option,
            rank_col = input$col_rank,
            AC_col = AC_col_val,
            question_label = as.character(q),
            question_type = q_type)
        
        } else {
          
          summarize_question_pert(
            df_q,
            id_col = input$col_id,
            lpp_col = input$col_lpp,
            bgp_col = input$col_bgp,
            hpp_col = input$col_hpp,
            AC_col = AC_col_val,
            lambda = input$lambda,
            Nsim = input$Nsim,
            seed = NULL,
            question_label = as.character(q),
            question_type = q_type)
          
        }
      })
      names(res_list) <- as.character(qs)
      
      # -------------------------------------------------------------------------
      # Multi-round analysis
      # -------------------------------------------------------------------------
    } else {
      
      res_list <- lapply(qs, function(q) {
        q_type <- get_question_type(as.character(q), question_types)
        df_q <- df %>%
          filter(.data[[q_col]] == q)
        rounds_q <- sort(unique(df_q[[round_col_val]]))
        round_results <- lapply(rounds_q, function(r) {
          df_qr <- df_q %>%
            filter(.data[[round_col_val]] == r)
          
          if (q_type == "yesno") {
            
            summarize_question_yesno(
              df_qr,
              id_col = input$col_id,
              response_col = input$col_response,
              AC_col = AC_col_val,
              question_label = as.character(q),
              question_type = q_type)
            
           } else if (q_type == "ranking") {
              
              summarize_question_ranking(
                df_qr,
                id_col = input$col_id,
                option_col = input$col_option,
                rank_col = input$col_rank,
                AC_col = AC_col_val,
                question_label = as.character(q),
                question_type = q_type)
              
            } else {
            
            summarize_question_pert(
              df_qr,
              id_col = input$col_id,
              lpp_col = input$col_lpp,
              bgp_col = input$col_bgp,
              hpp_col = input$col_hpp,
              AC_col = AC_col_val,
              lambda = input$lambda,
              Nsim = input$Nsim,
              seed = NULL,
              question_label = as.character(q),
              question_type = q_type)
            
          }
            
          }
        )
        names(round_results) <- as.character(rounds_q)
        round_results
      })
      names(res_list) <- as.character(qs)
    }
    
    # -------------------------------------------------------------------------
    # Identify questions/rounds for which Assessment Confidence weighting
    # is unavailable
    # -------------------------------------------------------------------------
    
    # Only assess AC availability when an AC column was supplied
    if (!is.null(AC_col_val)) {
      affected_fewer_than_two <- character(0)
      affected_all_zero <- character(0)
      if (is.null(round_col_val)) {
        
        # ==============================================================
        # SINGLE-ROUND DATA
        # ==============================================================
        
        for (q in names(res_list)) {
          result_q <- res_list[[q]]
          if (!isTRUE(result_q$AC_available)) {
            if (identical(
              result_q$AC_unavailable_reason, "fewer_than_two")) {
              affected_fewer_than_two <- c(affected_fewer_than_two,
                                           paste0("Question ", q))
            } else if (identical(
              result_q$AC_unavailable_reason,"all_zero")) {
              affected_all_zero <- c(affected_all_zero, paste0("Question ", q))
            }
          }
        }
        
      } else {
        
        # ==============================================================
        # MULTI-ROUND DATA
        # ==============================================================
        
        for (q in names(res_list)) {
          for (r in names(res_list[[q]])) {
            result_qr <- res_list[[q]][[r]]
            if (!isTRUE(result_qr$AC_available)) {
              label <- paste0("Question ", q, " (Round ", r, ")")
              if (identical(result_qr$AC_unavailable_reason, "fewer_than_two")) {
                affected_fewer_than_two <- c(affected_fewer_than_two, label)
              } else if (identical(result_qr$AC_unavailable_reason, "all_zero")) {
                affected_all_zero <- c(affected_all_zero, label)}
            }
          }
        }
      }
      
      
      # ---------------------------------------------------------------
      # Fewer than two Assessment Confidence responses
      # ---------------------------------------------------------------
      
      if (length(affected_fewer_than_two) > 0) {
        showNotification(paste0(
            "Assessment Confidence-weighted results are unavailable for ",
            paste(affected_fewer_than_two, collapse = ", "),
            " because fewer than two participants provided Assessment Confidence. ",
            "Equal-weight results remain available."),  type = "warning",
          duration = 10)
      }
      
      # ---------------------------------------------------------------
      # All reported Assessment Confidence values are zero
      # ---------------------------------------------------------------
      
      if (length(affected_all_zero) > 0) {
        showNotification(
          paste0("Assessment Confidence-weighted results are unavailable for ",
            paste(affected_all_zero, collapse = ", "),
            " because all reported Assessment Confidence values are 0. ",
            "Equal-weight results remain available."),
          type = "warning", duration = 10)
      }
    }
    
    # -------------------------------------------------------------------------
    # Bind results for plotting, summaries, and downloads
    # -------------------------------------------------------------------------
    
    if (is.null(round_col_val)) {
      # ==============================================================
      # SINGLE-ROUND DATA
      # Preserve the original structure
      # ==============================================================
      summary_all <- bind_rows(lapply(res_list, `[[`, "summary"))
      ranking_summary_all <- bind_rows(lapply(names(res_list), function(q) {
          obj <- res_list[[q]]
          if (is.null(obj$option_summary)) {
            return(NULL)
          }
          obj$option_summary %>%
            mutate(Question = q)}))
      dens_mix_all <- bind_rows(
        lapply(res_list, function(r) r$facet$mixture))
      dens_mix_w_all <- bind_rows(
        lapply(res_list, function(r) r$facet$mixture_w))
      dens_ind_all <- bind_rows(
        lapply(res_list, function(r) r$facet$individual))
      beta_all <- bind_rows(
        lapply(res_list, function(r) r$facet$beta))
      beta_w_all <- bind_rows(
        lapply(res_list, function(r) r$facet$beta_w))
      cdf_mix_all <- bind_rows(
        lapply(res_list, function(r) r$facet$cdf_mixture))
      cdf_mix_w_all <- bind_rows(
        lapply(res_list, function(r) r$facet$cdf_mixture_w))
      cdf_emp_all <- bind_rows(
        lapply(res_list, function(r) r$facet$cdf_emp))
      cdf_emp_w_all <- bind_rows(
        lapply(res_list, function(r) r$facet$cdf_emp_w))
      cdf_beta_all <- bind_rows(
        lapply(res_list, function(r) r$facet$cdf_beta))
      cdf_beta_w_all <- bind_rows(
        lapply(res_list, function(r) r$facet$cdf_beta_w))
      cdf_ind_all <- bind_rows(
        lapply(res_list, function(r) r$facet$cdf_individual))
      samples_all <- bind_rows(
        lapply(names(res_list), function(q) {
          tibble(Question = q, samples = res_list[[q]]$samples)
        }))
    } else {
      # ==============================================================
      # MULTI-ROUND DATA
      # Flatten Question -> Round -> Result into data frames
      # ==============================================================
      
      summary_all <- bind_rows(
        lapply(names(res_list), function(q) {
          bind_rows(lapply(names(res_list[[q]]), function(rnd) {
              res_list[[q]][[rnd]]$summary %>%
                mutate(Round = rnd, .after = Question)}))}))

      ranking_summary_all <- bind_rows(
        lapply(names(res_list), function(q) {
          bind_rows(lapply(names(res_list[[q]]), function(rnd) {
              obj <- res_list[[q]][[rnd]]
              if (is.null(obj$option_summary)) {
                return(NULL)
              }
              obj$option_summary %>%
                mutate(Question = q, Round = rnd)}))}))
      
      dens_mix_all <- bind_rows(
        lapply(names(res_list), function(q) {
          bind_rows(lapply(names(res_list[[q]]), function(rnd) {
            extract_facet(res_list[[q]][[rnd]], "mixture", rnd)}))}))
      
      dens_mix_w_all <- bind_rows(
        lapply(names(res_list), function(q) {
          bind_rows(lapply(names(res_list[[q]]), function(rnd) {
            extract_facet(res_list[[q]][[rnd]], "mixture_w", rnd)}))}))

      dens_ind_all <- bind_rows(
        lapply(names(res_list), function(q) {
          bind_rows(lapply(names(res_list[[q]]), function(rnd) {
            extract_facet(res_list[[q]][[rnd]], "individual", rnd)}))}))
      
      beta_all <- bind_rows(
        lapply(names(res_list), function(q) {
          bind_rows(lapply(names(res_list[[q]]), function(rnd) {
            extract_facet(res_list[[q]][[rnd]], "beta", rnd)}))}))
      
      beta_w_all <- bind_rows(
        lapply(names(res_list), function(q) {
          bind_rows(lapply(names(res_list[[q]]), function(rnd) {
            extract_facet(res_list[[q]][[rnd]], "beta_w", rnd)}))}))
      
      cdf_mix_all <- bind_rows(
        lapply(names(res_list), function(q) {
          bind_rows(lapply(names(res_list[[q]]), function(rnd) {
            extract_facet(res_list[[q]][[rnd]], "cdf_mixture", rnd)}))}))
      
      cdf_mix_w_all <- bind_rows(
        lapply(names(res_list), function(q) {
          bind_rows(lapply(names(res_list[[q]]), function(rnd) {
            extract_facet(res_list[[q]][[rnd]], "cdf_mixture_w", rnd)}))}))
      
      cdf_emp_all <- bind_rows(
        lapply(names(res_list), function(q) {
          bind_rows(lapply(names(res_list[[q]]), function(rnd) {
            extract_facet(res_list[[q]][[rnd]], "cdf_emp", rnd)}))}))
      
      cdf_emp_w_all <- bind_rows(
        lapply(names(res_list), function(q) {
          bind_rows(lapply(names(res_list[[q]]), function(rnd) {
            extract_facet(res_list[[q]][[rnd]], "cdf_emp_w", rnd)}))}))
      
      cdf_beta_all <- bind_rows(
        lapply(names(res_list), function(q) {
          bind_rows(lapply(names(res_list[[q]]), function(rnd) {
            extract_facet(res_list[[q]][[rnd]], "cdf_beta", rnd)}))}))
      
      cdf_beta_w_all <- bind_rows(
        lapply(names(res_list), function(q) {
          bind_rows(lapply(names(res_list[[q]]), function(rnd) {
            extract_facet(res_list[[q]][[rnd]], "cdf_beta_w", rnd)}))}))
      
      cdf_ind_all <- bind_rows(
        lapply(names(res_list), function(q) {
          bind_rows(lapply(names(res_list[[q]]), function(rnd) {
            extract_facet(res_list[[q]][[rnd]], "cdf_individual", rnd)}))}))

      # Add round-specific facet labels
      plot_objects <- c(
        "dens_mix_all",
        "dens_mix_w_all",
        "dens_ind_all",
        "beta_all",
        "beta_w_all",
        "cdf_mix_all",
        "cdf_mix_w_all",
        "cdf_emp_all",
        "cdf_emp_w_all",
        "cdf_beta_all",
        "cdf_beta_w_all",
        "cdf_ind_all"
      )
      
      for (obj in plot_objects) {
        tmp <- get(obj)
        if (all(c("Facet_Label", "Round") %in% names(tmp))) {
          tmp <- tmp %>%
            mutate(Facet_Label_Round = paste0( Facet_Label, "\nRound ", Round) )
          assign(obj, tmp)
        }
      }
      
      samples_all <- bind_rows(
        lapply(names(res_list), function(q) {
          bind_rows(lapply(names(res_list[[q]]), function(rnd) {
              tibble(Question = q,
                     Round = rnd,
                     samples = res_list[[q]][[rnd]]$samples)}))}))
      
      # Create combined Question-Round labels for faceting
      # Define Question x Round facet order so Round 2 appears
      # directly below Round 1 for each question block
      questions_order <- unique(as.character(summary_all$Question))
      rounds_order <- sort(unique(as.character(summary_all$Round)))
      n_facet_cols <- choose_round_facet_cols(length(questions_order))
      
      question_round_levels <- unlist(
        lapply(seq(1,length(questions_order),by = n_facet_cols),
          function(i) {
            qs_block <- questions_order[
              i:min(i + n_facet_cols - 1, length(questions_order))]
            unlist(lapply(rounds_order,
                          function(rnd) {
                            paste0("Question ", qs_block, "\nRound ", rnd)}))}))
      
      add_question_round <- function(df, levels) {
        if (is.null(df) ||
            nrow(df) == 0 ||
            !all(c("Question", "Round") %in% names(df))) {
          return(df)
        }
        
        df %>%
          mutate(Question_Round = factor(paste0(
            "Question ", Question, "\nRound ", Round), levels = levels))
      }
      
      cdf_mix_all   <- add_question_round(cdf_mix_all, question_round_levels)
      cdf_mix_w_all <- add_question_round(cdf_mix_w_all, question_round_levels)
      cdf_emp_all   <- add_question_round(cdf_emp_all, question_round_levels)
      cdf_emp_w_all <- add_question_round(cdf_emp_w_all, question_round_levels)
      
      cdf_beta_all  <- add_question_round(cdf_beta_all, question_round_levels)
      cdf_beta_w_all <- add_question_round(cdf_beta_w_all, question_round_levels)
      cdf_ind_all   <- add_question_round(cdf_ind_all, question_round_levels)
      
      dens_mix_all  <- add_question_round(dens_mix_all, question_round_levels)
      dens_mix_w_all <- add_question_round(dens_mix_w_all, question_round_levels)
      dens_ind_all  <- add_question_round(dens_ind_all, question_round_levels)
      
      beta_all      <- add_question_round(beta_all, question_round_levels)
      beta_w_all    <- add_question_round(beta_w_all, question_round_levels)
    }
    
    list(
      summaries      = summary_all,
      ranking_summary_all = ranking_summary_all,
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
  
  filter_round <- function(df, selected_round) {
    if (is.null(df) ||
        nrow(df) == 0 ||
        !"Round" %in% names(df)) {
      return(df)
    }
    df %>%
      filter(as.character(Round) == selected_round)
  }
  
  plot_results <- reactive({
    req(results())
    r <- results()
    if (is.null(input$round_filter) ||
        input$round_filter == "All") {
      return(r)
    }
    selected_round <- as.character(input$round_filter)
    if ("Round" %in% names(r$summaries)) {
      
      r$summaries <- r$summaries %>%
        filter(as.character(Round) == selected_round)
      
      r$dens_mix_all   <- filter_round(r$dens_mix_all, selected_round)
      r$dens_mix_w_all <- filter_round(r$dens_mix_w_all, selected_round)
      r$dens_ind_all   <- filter_round(r$dens_ind_all, selected_round)
      
      r$beta_all       <- filter_round(r$beta_all, selected_round)
      r$beta_w_all     <- filter_round(r$beta_w_all, selected_round)
      
      r$cdf_mix_all    <- filter_round(r$cdf_mix_all, selected_round)
      r$cdf_mix_w_all  <- filter_round(r$cdf_mix_w_all, selected_round)
      
      r$cdf_emp_all    <- filter_round(r$cdf_emp_all, selected_round)
      r$cdf_emp_w_all  <- filter_round(r$cdf_emp_w_all, selected_round)
      
      r$cdf_beta_all   <- filter_round(r$cdf_beta_all, selected_round)
      r$cdf_beta_w_all <- filter_round(r$cdf_beta_w_all, selected_round)
      
      r$cdf_ind_all    <- filter_round(r$cdf_ind_all, selected_round)
      
      r$samples_all    <- filter_round(r$samples_all, selected_round)
    }
    r
  })
  
  ###########################################################################
  # Single PNG downloads (use current selections in results()) 
  ###########################################################################
  # Determine export dimensions from the number of displayed questions
  export_dimensions <- function(r) {
    # Count unique questions, not Question x Round rows
    n_questions <- length(unique(r$summaries$Question))
    # Determine whether this is a multi-round analysis
    has_round <- "Round" %in% names(r$summaries) &&
      length(unique(r$summaries$Round)) > 1
    if (has_round) {
      # Multi-round plots:
      # one column per question, with rounds stacked vertically
      n_cols <- n_questions
      n_rounds <- length(unique(r$summaries$Round))
      n_rows <- n_rounds
      list(facet_cols = n_cols,
           width = max(8, 2.8 * n_cols),
           height = max(5.5, 3.0 * n_rows))
    } else {
      # Single-round plots:
      # retain the original maximum of four columns
      n_cols <- min(4, n_questions)
      n_rows <- ceiling(n_questions / n_cols)
      list(facet_cols = n_cols,
           width = max(8, 2.8 * n_cols),
           height = max(4.5, 3.0 * n_rows))}
  }
  
  ###############################################################################
  # EXPORTS
  ###############################################################################
  output$download_participants_png <- downloadHandler(
    filename = function() {
      paste0("expert_estimates_", format(Sys.time(), "%Y%m%d_%H%M"), ".png")
    },
    
    content = function(file) {
      req(results())
      dims <- export_dimensions(results())
      g <- build_expert_estimates_plot(export=TRUE)
      ggsave(filename = file, plot = g, width = dims$width, height = dims$height,
             dpi = 600, units = "in", bg = "white", limitsize = FALSE)
    }
  )
  
  output$download_density_png <- downloadHandler(
    filename = function() {
      paste0("density_", format(Sys.time(), "%Y%m%d_%H%M"), ".png")
    },
    
    content = function(file) {
      req(results())
      r <- results()
      dims <- export_dimensions(r)
      g <- build_density_panel_plot(export=TRUE) 
      
      ggsave(filename = file, plot = g, width = dims$width, height = dims$height, 
             dpi = 600, units = "in", bg = "white", limitsize = FALSE)
    }
  )
  
  output$download_hist_png <- downloadHandler(
    filename = function() {
      paste0("histogram_", format(Sys.time(), "%Y%m%d_%H%M"), ".png")
    },
    content = function(file) {
      req(results())
      r <- results()
      dims <- export_dimensions(r)
      g <- build_hist_panel_plot(export=TRUE)
      
      ggsave(filename = file, plot = g, width = dims$width,
             height = dims$height, dpi = 600, units = "in", bg = "white",
             limitsize = FALSE)
    }
  )
  
  output$download_cdf_png <- downloadHandler(
    filename = function() {
      paste0("cdf_", format(Sys.time(), "%Y%m%d_%H%M"), ".png")
    },
    content = function(file) {
      req(results())
      r <- results()
      dims <- export_dimensions(r)
      g <- build_cdf_panel_plot(export=TRUE)
      
      ggsave(filename = file,plot = g,width = dims$width,
             height = dims$height,dpi = 600,units = "in",bg = "white",
             limitsize = FALSE)
    }
  )
  
  ###########################################################################
  # ZIP files
  ###########################################################################
  output$download_all_plots_zip <- downloadHandler(
    filename = function() paste0("elicitation_plots_", format(Sys.time(), "%Y%m%d_%H%M"), ".zip"),
    content = function(file) {
      req(results())
      r <- results()
      
      # Determine appropriate export layout
      dims <- export_dimensions(r)
      
      # Build plots (respecting current toggle states)
      g1 <- build_density_panel_plot(export=TRUE)
      #g2 <- build_hist_panel_plot(export=TRUE)
      g3 <- build_cdf_panel_plot(export=TRUE)
      g4 <- build_expert_estimates_plot(export=TRUE)
      
      # Save all figures to a temp directory
      outdir <- tempfile("plots_")
      dir.create(outdir, showWarnings = FALSE)
      
      f1 <- file.path(outdir, paste0("density_",   Sys.Date(), ".png"))
      #f2 <- file.path(outdir, paste0("histogram_", Sys.Date(), ".png"))
      f3 <- file.path(outdir, paste0("cdf_",       Sys.Date(), ".png"))
      f4 <- file.path(outdir, paste0("individuals_", Sys.Date(), ".png"))
      
      ggsave(f1, g1,width = dims$width,height = dims$height,
             dpi = 600,units = "in",bg = "white",limitsize = FALSE)
      
      #ggsave(f2, g2,width = dims$width,height = dims$height,dpi = 600,
      #       units = "in",bg = "white",limitsize = FALSE)
      
      ggsave(f3, g3,width = dims$width,height = dims$height,dpi = 600,
             units = "in",bg = "white",limitsize = FALSE)
      
      ggsave(f4, g4, width = dims$width, height = dims$height, dpi = 600,
             units = "in", bg = "white", limitsize = FALSE)
      
      # Write the summary CSV
      f_summary_csv <- file.path(outdir, paste0("summary_", Sys.Date(), ".csv"))
      write_csv(r$summaries, f_summary_csv)
      
      if (!is.null(r$ranking_summary_all) &&
          nrow(r$ranking_summary_all) > 0) {
        f_ranking_csv <- file.path(outdir, paste0("ranking_summary_", Sys.Date(), ".csv"))
        write_csv(r$ranking_summary_all,f_ranking_csv)
      }
      
      # Zip them up
      oldwd <- setwd(outdir); on.exit(setwd(oldwd), add = TRUE)
      zip(zipfile = file, files = basename(c(f1, #f2, 
                                             f3, f4,f_summary_csv)))
    }
  )
  
  ############################################################################
  # Dynamic Plot Layout
  # When a single question is selected, reduce the plot container width to
  # improve readability and avoid excessive horizontal whitespace. When
  # multiple questions are displayed, plots expand to the full available width.
  ############################################################################
  plot_container_ui <- function(plot_id) {
    n_questions <- length(unique(plot_results()$summaries$Question))
    single_round_view <-
      !is.null(input$round_filter) &&
      input$round_filter != "All"
    width_style <- if (n_questions == 1) {
      "width:35%;"
    } else {
      "width:100%;"
    }
    height <- if (single_round_view) {
      "400px"
    } else {
      "650px"
    }
    div(style = width_style, plotOutput(plot_id, height = height))
  }
  
  output$plot_participants_container <- renderUI({
    plot_container_ui("plot_participants")
  })
  
  output$plot_density_ui <- renderUI({
    plot_container_ui("plot_density")
  })
  
  output$plot_hist_ui <- renderUI({
    plot_container_ui("plot_hist")
  })
  
  output$plot_cdf_ui <- renderUI({
    plot_container_ui("plot_cdf")
  })
  
  ############################################################################
  # Individual Expert Estimates Plot
  # Displays lowest plausible, best guess, and highest plausible estimates
  # for each participant, optionally filtered to a selected elicitation round.
  ###########################################################################
  # helper first
  build_expert_estimates_plot <- function(export=FALSE) {
    df_plot <- raw_df()
    if (!is.null(input$round_filter) &&
        input$round_filter != "All" &&
        !is.null(input$col_round) &&
        input$col_round != "<none>") {
      df_plot <- df_plot %>%
        filter(as.character(.data[[input$col_round]]) ==
                 as.character(input$round_filter))
    }
    df_plot <- filter_selected_questions(df_plot,
                                         input$question_multi,
                                         input$col_question)
    question_list <- split(df_plot, as.character(df_plot[[input$col_question]]))
    plots <- list()
    
    for (q in names(question_list)) {
      df_q <- question_list[[q]]
      q_type <- if ("Question_Type" %in% names(df_q)) {
        unique(df_q$Question_Type)[1]
      } else {
        "probability"
      }
      
      if (q_type == "probability") {
        plots[[length(plots) + 1]] <- build_probability_individuals_plot(
            df = df_q,
            id_col = input$col_id,
            lpp_col = input$col_lpp,
            bgp_col = input$col_bgp,
            hpp_col = input$col_hpp,
            export = export,
            question_col = input$col_question,
            round_col = if (
              !is.null(input$col_round) &&
              input$col_round != "<none>"
            ) {
              input$col_round
            } else {
              NULL
            }
          )
        
      } else if (q_type == "numeric") {
        
        plots[[length(plots) + 1]] <-
          build_numeric_individuals_plot(
            df = df_q,
            id_col = input$col_id,
            lpp_col = input$col_lpp,
            bgp_col = input$col_bgp,
            hpp_col = input$col_hpp,
            question_col = input$col_question,
            export = export,
            round_col = if (
              !is.null(input$col_round) &&
              input$col_round != "<none>"
            ) {
              input$col_round
            } else {
              NULL
            }
          )
        
      } else if (q_type == "yesno") {
        
        plots[[length(plots) + 1]] <-
          build_yesno_individuals_plot(
            df = df_q,
            id_col = input$col_id,
            response_col = input$col_response,
            export = export,
            round_col = if (
              !is.null(input$col_round) &&
              input$col_round != "<none>"
            ) {
              input$col_round
            } else {
              NULL
            }
          )
        
      } else if (q_type == "ranking") {
        
        plots[[length(plots) + 1]] <-
          build_ranking_individuals_plot(
            df = df_q,
            id_col = input$col_id,
            option_col = input$col_option,
            rank_col = input$col_rank,
            export = export,
            round_col = if (
              !is.null(input$col_round) &&
              input$col_round != "<none>"
            ) {
              input$col_round
            } else {
              NULL
            }
          )
      }
    }
    
    patchwork::wrap_plots(plots, ncol = length(plots)) +
      patchwork::plot_layout(guides = "collect") 
  }
  
    output$plot_participants <- renderPlot({
      req(results())
      build_expert_estimates_plot()
    })

  
    #######################################################
    # Shared Plot Helpers
    #######################################################
    
  filter_questions <- function(df, sel) {
    if (is.null(df) ||
        nrow(df) == 0 ||
        !"Question" %in% names(df)) {
      return(df)
    }
    df %>%
      filter(as.character(Question) %in% as.character(sel))
  }
  
  filter_question <- function(df, q) {
    if (is.null(df) ||
        nrow(df) == 0 ||
        !"Question" %in% names(df)) {
      return(df)
    }
    df %>%
      filter(as.character(Question) == q)
  }
  
  #####################################################
  # Plots: Mixture density
  #####################################################
  # helper
  build_density_panel_plot <- function(export=FALSE) {
    r <- plot_results()
    plots <- list()
    
    # Filter selected questions
    if (!is.null(input$question_multi)) {
      sel <- setdiff(input$question_multi, "All")
      
      if (length(sel) > 0) {
        r$summaries <- r$summaries %>%
          filter(as.character(Question) %in% as.character(sel))
        
        r$samples_all    <- filter_questions(r$samples_all, sel)
        r$dens_mix_all   <- filter_questions(r$dens_mix_all, sel)
        r$dens_mix_w_all <- filter_questions(r$dens_mix_w_all, sel)
        r$dens_ind_all   <- filter_questions(r$dens_ind_all, sel)
        r$beta_all       <- filter_questions(r$beta_all, sel)
      }
    }
    
    questions <- unique(as.character(r$summaries$Question))
    for (q in questions) {
      if (nrow(r$summaries %>%
        filter(as.character(Question) == q)) == 0) {
        next
      }
      
      q_type <- r$summaries %>%
        filter(as.character(Question) == q) %>%
        pull(Question_Type) %>%
        unique() %>%
        .[1]
      
      if (is.na(q_type)) {
        next
      }
      dens_mix_q   <- filter_question(r$dens_mix_all, q)
      dens_mix_w_q <- filter_question(r$dens_mix_w_all, q)
      dens_ind_q   <- filter_question(r$dens_ind_all, q)
      beta_q       <- filter_question(r$beta_all, q)
      
      if (q_type == "probability") {
        plots[[length(plots) + 1]] <-
          build_probability_density_plot(
            df_mix = dens_mix_q,
            df_mix_w = dens_mix_w_q,
            df_ind = dens_ind_q,
            df_beta = beta_q,
            show_individual = isTRUE(input$show_individual),
            show_beta = isTRUE(input$show_beta),
            show_AC_mix = isTRUE(input$show_AC_mix),
            export = export)
        
      } else if (q_type == "numeric") {
        plots[[length(plots) + 1]] <-
          build_numeric_density_plot(
            df_mix = dens_mix_q,
            df_mix_w = dens_mix_w_q,
            df_ind = dens_ind_q,
            show_individual = isTRUE(input$show_individual),
            show_AC_mix = isTRUE(input$show_AC_mix),
            export = export)
        
      } else if (q_type == "yesno") {
        
        summary_q <- r$summaries %>%
          filter(as.character(Question) == q)
        plots[[length(plots) + 1]] <-build_yesno_plot(summary_q, export = export)
        
      } else if (q_type == "ranking") {
        
        ranking_q <- r$ranking_summary_all %>%
          filter(as.character(Question) == q)
        plots[[length(plots) + 1]] <- build_ranking_pooled_plot(ranking_q, export = export)
        
      }
    }
    
    if (length(plots) == 0) {
      
      return(ggplot() + annotate("text", x = 1, y = 1,
                                 label = paste(
                                   "No distribution-based visualizations are available",
                                   "for the selected question type."),
                                 size = 6) + theme_void())
    }
    
    patchwork::wrap_plots(plots, ncol = length(plots)) 
  }
  
  output$plot_density <- renderPlot({
    req(plot_results())
    build_density_panel_plot()
  })
  
  #################################################
  # Plots: Histogram + Beta fit (faceted by full label)
  #################################################
  build_hist_panel_plot <- function(export=FALSE) {
    r <- plot_results()
    plots <- list()
    if (!is.null(input$question_multi)) {
      sel <- setdiff(input$question_multi, "All")
      if (length(sel) > 0) {
        r$summaries <- r$summaries %>%
          filter(as.character(Question) %in% as.character(sel))
        r$samples_all    <- filter_questions(r$samples_all, sel)
        r$dens_mix_all   <- filter_questions(r$dens_mix_all, sel)
        r$dens_mix_w_all <- filter_questions(r$dens_mix_w_all, sel)
        r$dens_ind_all   <- filter_questions(r$dens_ind_all, sel)
        r$beta_all       <- filter_questions(r$beta_all, sel)
      }
    }
    questions <- unique(as.character(r$summaries$Question))
    for (q in questions) {
      if (nrow(
        r$summaries %>%
        filter(as.character(Question) == q)
      ) == 0) {
        next
      }
      q_type <- r$summaries %>%
        filter(as.character(Question) == q) %>%
        pull(Question_Type) %>%
        unique() %>%
        .[1]
      
      if (is.na(q_type)) {
        next
      }
      dens_mix_q   <- filter_question(r$dens_mix_all, q)
      dens_mix_w_q <- filter_question(r$dens_mix_w_all, q)
      dens_ind_q   <- filter_question(r$dens_ind_all, q)
      beta_q       <- filter_question(r$beta_all, q)
      samples_q    <- filter_question(r$samples_all, q)
      
      if (q_type == "probability") {
        plots[[length(plots) + 1]] <-
          build_probability_hist_plot(
            samples = samples_q,
            dens_mix = dens_mix_q,
            dens_mix_w = dens_mix_w_q,
            df_beta = beta_q,
            show_AC_mix = isTRUE(input$show_AC_mix),
            show_beta = isTRUE(input$show_beta),
            export = export )
        
      } else if (q_type == "numeric") {
        plots[[length(plots) + 1]] <-
          build_numeric_hist_plot(
            samples = samples_q,
            dens_mix = dens_mix_q,
            dens_mix_w = dens_mix_w_q,
            show_AC_mix = isTRUE(input$show_AC_mix),
            export = export)
        
      } else if (q_type %in% c("yesno", "ranking")) {
        next
      }
    }
    
    if (length(plots) == 0) {
      
      return(ggplot() + annotate("text", x = 1, y = 1,
                                 label = paste(
                                   "No distribution-based visualizations are available",
                                   "for the selected question type."),
                                 size = 6) + theme_void())
    }
    patchwork::wrap_plots(plots, ncol = length(plots)) 
  }
  
  output$plot_hist <- renderPlot({
    req(plot_results())
    build_hist_panel_plot()
  })
  
  #################################################
  # Plots: CDF comparison (faceted by full label)
  #################################################
  build_cdf_panel_plot <- function(export=FALSE) {
    r <- plot_results()
    plots <- list()
    if (!is.null(input$question_multi)) {
      sel <- setdiff(input$question_multi, "All")
      if (length(sel) > 0) {
        r$summaries <- r$summaries %>%
          filter(as.character(Question) %in% as.character(sel))
        r$samples_all   <- filter_questions(r$samples_all, sel)
        r$cdf_mix_all   <- filter_questions(r$cdf_mix_all, sel)
        r$cdf_mix_w_all <- filter_questions(r$cdf_mix_w_all, sel)
        r$cdf_ind_all   <- filter_questions(r$cdf_ind_all, sel)
        r$cdf_beta_all  <- filter_questions(r$cdf_beta_all, sel)
        
      }
    }
    
    questions <- unique(as.character(r$summaries$Question))
    
    for (q in questions) {
      
      if (
        nrow(
          r$summaries %>%
          filter(as.character(Question) == q)
        ) == 0
      ) {
        next
      }
      
      q_type <- r$summaries %>%
        filter(as.character(Question) == q) %>%
        pull(Question_Type) %>%
        unique() %>%
        .[1]
      
      if (is.na(q_type)) {
        next
      }
      
      cdf_mix_q   <- filter_question(r$cdf_mix_all, q)
      cdf_mix_w_q <- filter_question(r$cdf_mix_w_all, q)
      cdf_ind_q   <- filter_question(r$cdf_ind_all, q)
      cdf_beta_q  <- filter_question(r$cdf_beta_all, q)
      
      if (q_type == "probability") {
        
        plots[[length(plots) + 1]] <-
          build_probability_cdf_plot(
            df_mix = cdf_mix_q,
            df_mix_w = cdf_mix_w_q,
            df_ind = cdf_ind_q,
            df_beta = cdf_beta_q,
            show_individual = isTRUE(input$show_individual),
            show_beta = isTRUE(input$show_beta),
            show_AC_mix = isTRUE(input$show_AC_mix),
            export = export)
        
      } else if (q_type == "numeric") {
        
        plots[[length(plots) + 1]] <-
          build_numeric_cdf_plot(
            df_mix = cdf_mix_q,
            df_mix_w = cdf_mix_w_q,
            df_ind = cdf_ind_q,
            show_individual = isTRUE(input$show_individual),
            show_AC_mix = isTRUE(input$show_AC_mix),
            export = export)
        
      } else if (q_type %in% c("yesno", "ranking")) {
        next
      }
    }
    
    if (length(plots) == 0) {
      
      return(ggplot() + annotate("text", x = 1, y = 1,
                                 label = paste(
                                   "No distribution-based visualizations are available",
                                   "for the selected question type."),
                                 size = 6) + theme_void())
    }
    patchwork::wrap_plots(plots, ncol = length(plots)) 
  }
  
  output$plot_cdf <- renderPlot({
    req(plot_results())
    build_cdf_panel_plot()
  })
  
  ###############################################################################
  # SUMMARY DASHBOARD
  ###############################################################################
  # Visual summary cards
  output$summary_dashboard <- renderUI({
    req(plot_results())
    s <- plot_results()$summaries
    
    q_type <- unique(na.omit(as.character(s$Question_Type)))
    if (length(q_type) == 0) {
      return(NULL)
    }
    q_type <- q_type[1]
    
    if (q_type == "ranking") {
      return(NULL)
    }
    
    if (q_type == "yesno") {
      return(NULL)
    }
    
    single_round_view <-
      !is.null(input$round_filter) &&
      input$round_filter != "All"
    # Summary cards are intended for a single selected question
    n_questions <- length(unique(s$Question))
    if (n_questions != 1) {
      return(
        div(
          style = "
      background-color: #f5f5f5;
      border-left: 4px solid #888888;
      padding: 12px;
      border-radius: 6px;
      margin-bottom: 15px;
      ",
          tags$strong("Multiple questions selected"),
          tags$br(),
          tags$span(style = "color: #666666;",
            "Select a single question to display the visual summary.")
        )
      )
    }
    
    if (!single_round_view &&
        "Round" %in% names(s) &&
        all(c("1", "2") %in% as.character(s$Round))) {
      multi_round <- TRUE
      s1 <- s %>% filter(as.character(Round) == "1")
      s2 <- s %>% filter(as.character(Round) == "2")
    } else {
      multi_round <- FALSE
      s1 <- s
      s2 <- s
    }
    
    delta_colour <- function(x) {
      if (is.na(x)) {
        "#666666"
      } else if (x > 0) {
        "#4C956C"   # green
      } else if (x < 0) {
        "#D55E00"   # orange/red
      } else {
        "#245674"   # blue
      }
    }
    
    # Reusable card styles
    card_style <- "
    background-color: #f7f9fa;
    border: 1px solid #dddddd;
    border-radius: 8px;
    padding: 18px 8px;
    text-align: center;
    margin-bottom: 12px;
    min-height: 110px;
  "
    
    value_style <- function(colour) {
      paste0(
        "
    font-size: 28px;
    font-weight: 600;
    color: ", colour,  ";")
    }
    
    label_style <- "
    color: #666666;
    font-size: 13px;
    margin-top: 5px;
  "
    
    # Use Round 2 metrics in cards for now
    if (multi_round) {
    s <- s2
    eqw_delta <- s2$EqW_Mean - s1$EqW_Mean
    AC_delta <- s2$ACW_Mean - s1$ACW_Mean
    conf_delta <- s2$Mean_AC - s1$Mean_AC
    
    fluidRow(
      # Pooled mean
      column(4, div(style = card_style,
                    div(style = value_style(delta_colour(eqw_delta)),
                        sprintf("%+.3f", eqw_delta)),
                    div(style = label_style, "Δ Equal-weight mean"))),
      # AC weighted mean
      column(4, div(style = card_style, div(
        style = value_style(delta_colour(AC_delta)),
        sprintf("%+.3f", AC_delta)),
        div(style = label_style, "Δ AC-weighted mean"))),
      
      # Mean Assessment confidence
      column(4, div(style = card_style,
                    div(style = value_style(delta_colour(conf_delta)),
                        paste0(sprintf("%+.0f", conf_delta), "%")),
                    div(style = label_style, "Δ Assessment confidence"))),
      
      # Equal-weight means across rounds
      column(4, div(style = card_style,
                    div(style = value_style("#245674"),
                        paste0(sprintf("%.3f", s1$EqW_Mean)," \u2192 ",
                               sprintf("%.3f", s2$EqW_Mean))),
          div(style = label_style, "Equal-weight mean (R1 \u2192 R2)"))),
      
      # AC-weighted means across rounds
      column(4, div(
        style = card_style,
        div(style = value_style("#245674"),
            paste0(sprintf("%.3f", s1$ACW_Mean)," \u2192 ",
                   sprintf("%.3f", s2$ACW_Mean))),
          div(style = label_style,"AC-weighted mean (R1 \u2192 R2)"))),
      
      # Number of experts
      column(4,div(style = card_style,
                   div(
                     style = value_style("#245674"), s$N_Participants),
                   div(style = label_style,"Experts"))))
    } else {
      s <- s1
      fluidRow(column(3,
                      div(style = card_style,
                          div(style = value_style("#245674"),
                              sprintf("%.3f", s$EqW_Mean)),
                          div(style = label_style, "Equal-weight mean"))),
               column(3,
                      div(style = card_style,
                          div(style = value_style("#245674"),
                              sprintf("%.3f", s$ACW_Mean)),
                          div(style = label_style, "AC-weighted mean"))),
               column(3,
                      div(style = card_style,
                          div(style = value_style("#245674"),
                              paste0(round(s$Mean_AC), "%")),
                          div(style = label_style, "Average assessment confidence"))),
               column(3, 
                      div(style = card_style,
                          div(style = value_style("#245674"),
                              s$N_Participants),
                          div(style = label_style, "Experts"))))
    }})
  
  ###############################################################################
  # SUMMARY TABLES AND REPORTING
  ################################################################################
  output$round_comparison_ui <- renderUI({
    req(results())
    summ <- results()$summaries %>%
      filter(Question_Type %in% c("probability", "numeric"))
    
    if (nrow(summ) == 0) {
      return(NULL)
    }
    
    tagList( h4("Round Comparison"),
      p("Changes in pooled estimates and assessment confidence between Round 1 and Round 2."),
      tableOutput("round_comparison_table")
    )
  })
  
  output$yesno_results_ui <- renderUI({
    req(results())
    yesno_tbl <- results()$summaries %>%
      filter(Question_Type == "yesno")
    if (nrow(yesno_tbl) == 0) {
      return(NULL)
    }
    tagList(
      h4("Yes/No Results"),
      p("Pooled response proportions and assessment confidence."),
      tableOutput("yesno_summary_table")
    )
  })
  
  output$yesno_summary_table <- renderTable({
    req(plot_results())
    yesno_tbl <- plot_results()$summaries %>%
      filter(Question_Type == "yesno")
    if (nrow(yesno_tbl) == 0) {
      return(NULL)
    }
    yesno_tbl %>%
      mutate(across(where(is.numeric), ~ round(.x, 2)))
  })
  
  output$ranking_results_ui <- renderUI({
    req(results())
    ranking_tbl <- results()$ranking_summary_all
    if (is.null(ranking_tbl) || nrow(ranking_tbl) == 0) {
      return(NULL)
    }
    tagList(
      h4("Ranking Results"),
      tableOutput("ranking_summary_table")
    )
  })
  
  output$round_comparison_table <- renderTable({
    req(results())
    
    # Round comparison only makes sense when viewing all rounds
    if (!is.null(input$round_filter) &&
        input$round_filter != "All") {
      return(NULL)
    }
    
    summ <- results()$summaries %>%
      filter(Question_Type %in% c("probability", "numeric"))
    
    if (is.null(summ) || nrow(summ) == 0) {
      return(NULL)
    }
    
    # Only build comparison if Round exists
    if (!"Round" %in% names(summ)) {
      return(NULL)
    }
    # Require at least two rounds
    if (!all(c("1", "2") %in% unique(as.character(summ$Round)))) {
      return(NULL)
    }
    r1 <- summ %>%
      filter(as.character(Round) == "1") %>%
      select(Question,
             EqW_Mean_R1 = EqW_Mean,
             ACW_Mean_R1 = ACW_Mean,
             Mean_AC_R1 = Mean_AC)
    
    r2 <- summ %>%
      filter(as.character(Round) == "2") %>%
      select(Question,
             EqW_Mean_R2 = EqW_Mean,
             ACW_Mean_R2 = ACW_Mean,
             Mean_AC_R2 = Mean_AC)
    
    left_join(r1, r2, by = "Question") %>%
      mutate(`Δ Equal-weight mean` = EqW_Mean_R2 - EqW_Mean_R1,
             `Δ AC-weighted mean` = ACW_Mean_R2 - ACW_Mean_R1,
             `Δ Assessment confidence` = Mean_AC_R2 - Mean_AC_R1) %>%
     
       rename(`Equal-weight mean (R1)` = EqW_Mean_R1,
              `Equal-weight mean (R2)` = EqW_Mean_R2,
              `AC-weighted mean (R1)` = ACW_Mean_R1,
              `AC-weighted mean (R2)` = ACW_Mean_R2,
              `Average Assessment confidence (R1)` = Mean_AC_R1,
              `Average Assessment confidence (R2)` = Mean_AC_R2) %>%
   
       mutate(across(where(is.numeric), ~ round(.x, 2)))
  })
  
  # Summary table & download
  output$summary_table <- renderTable({
    req(plot_results())
    plot_results()$summaries %>%
      filter(Question_Type %in% c("probability", "numeric", "yesno"))
  })
  
  output$hasRankingResults <- reactive({
    req(results())
    !is.null(results()$ranking_summary_all) &&
      nrow(results()$ranking_summary_all) > 0
  })
  
  outputOptions(output, "hasRankingResults", suspendWhenHidden = FALSE)
  
  output$ranking_summary_table <- renderTable({
    req(plot_results())
    ranking_tbl <- plot_results()$ranking_summary_all
    if (is.null(ranking_tbl) || nrow(ranking_tbl) == 0) {
      return(NULL)
    }
    
    ranking_tbl %>%
      relocate(any_of(c("Question", "Round"))) %>%
      arrange(Question,Round, Mean_Rank, Median_Rank) %>%
      mutate(across(c(Mean_Rank, Median_Rank, Rank_SD), ~ round(.x, 2)))
  })
  
  output$download_summary <- downloadHandler(
    filename = function() {
      paste0("elicitation_summaries_", format(Sys.time(), "%Y%m%d_%H%M"), ".zip")
    },
    
    content = function(file) {
      outdir <- tempfile("summaries_")
      dir.create(outdir)
      out <- results()$summaries
      
      if (!is.null(input$col_question_text) &&
          !identical(input$col_question_text, "<none>")) {
        
        question_lookup <- raw_df() %>%
          group_by(Question = as.character(.data[[input$col_question]])) %>%
          summarise(Question_Text = first(.data[[input$col_question_text]]),
            .groups = "drop")
        
        out <- out %>%
          mutate(Question = as.character(Question)) %>%
          left_join(question_lookup, by = "Question") %>%
          relocate(Question_Text, .after = Question)
      }
      
      write_csv(out, file.path(outdir, "detailed_results.csv"))
      
      ranking_tbl <- results()$ranking_summary_all
      if (!is.null(ranking_tbl) &&
          nrow(ranking_tbl) > 0) {
        write_csv(ranking_tbl, file.path( outdir, "ranking_results.csv"))
      }
      
      yesno_tbl <- results()$summaries %>%
        filter(Question_Type == "yesno")
      
      if (nrow(yesno_tbl) > 0) {
        write_csv(yesno_tbl, file.path( outdir, "yesno_results.csv"))
      }
      
      oldwd <- setwd(outdir)
      on.exit(setwd(oldwd), add = TRUE)
      
      zip(zipfile = file, files = list.files(outdir))
    }
  )
  
  output$download_report_html <- downloadHandler(
    filename = function() {
      paste0("Expert_Elicitation_Report_",
             format(Sys.time(), "%Y%m%d_%H%M"), ".html")},
    content = function(file) {
      req(results())
      report_data <- build_report_data(results_obj = results())
      render_facilitator_report(report_data,file)})
}

# Run app
shinyApp(ui, server)
