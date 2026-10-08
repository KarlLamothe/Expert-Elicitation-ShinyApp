############################################################################# 
# Server
###########################################################################
server <- function(input, output, session) {
  
  sheet_data <- reactiveVal(NULL)
  last_refresh <- reactiveVal(NULL)
  n_participants <- reactiveVal(NULL)
  
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
  output$download_template <- downloadHandler(
    filename = function() {
      "expert_elicitation_response_template.csv"
    },
    content = function(file) {
      template <- data.frame(
        Question = c(1, 1, 1, 1),
        Round = c(1, 1, 2, 2),
        Participant = c("P1", "P2", "P1", "P2"),
        Lowest_Plausible_Pr = c(0.20, 0.30, 0.25, 0.35),
        Best_Guess_Pr = c(0.40, 0.50, 0.50, 0.55),
        Highest_Plausible_Pr = c(0.60, 0.75, 0.70, 0.75),
        Assessment_Confidence = c(70, 65, 85, 75)
      )
      write.csv(template, file, row.names = FALSE, na = "")},
    contentType = "text/csv"
  )
  
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
      div(
        style = "
        background-color: #eef7f1;
        border-left: 4px solid #3c8c5a;
        padding: 12px;
        border-radius: 6px;
        margin-bottom: 10px;
      ",
        tags$div(
          style = "font-weight: 600; color: #267044;",
          "● Expert responses loaded"),
        if (!is.null(n_exp)) {
          tags$div(
            style = "margin-top: 5px;",
            paste(n_exp, "experts"))
          },
        tags$div(
          style = "font-size: 12px; color: #666666; margin-top: 3px;",
          paste("Updated",format(last_refresh(), "%I:%M:%S %p"))
        )
      )
    }
  })
  
  # Load data from the selected source
  raw_df <- reactive({
    req(input$data_source)
    #Demo data
    if (identical(input$data_source, "demo")) {
      return(demo_df)}
    # Uploaded CSV
    if (identical(input$data_source, "csv")) {
      req(input$csv_file)
      df <- read.csv(input$csv_file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      return(df)
    }
    
    # Google Sheets
    if (identical(input$data_source, "google")) {
      req(sheet_data())
      return(sheet_data())
    }
    NULL
  })
  
  # Column mapping UI
  output$colmap_ui <- renderUI({
    req(raw_df())
    df <- raw_df()
    cols <- names(df)
    default_question <- best_question_col(df)
    tagList(
      selectInput("col_question", "Question column", choices = c("Question", cols),
        selected = if (default_question %in% cols) default_question else "<none>"),
      
      selectInput("col_id", "Participant ID column", choices = cols,
        selected = if ("Participant" %in% cols) "Participant" else cols[1]),
      
      selectInput("col_lpp", "Lowest plausible (LPP)", choices = cols,
         selected = grep("Lowest|LPP", cols,ignore.case = TRUE, value = TRUE)[1]),
      
      selectInput("col_bgp", "Best guess (BGP)", choices = cols,
        selected = grep("Best|BGP",cols, ignore.case = TRUE, value = TRUE)[1]),
     
      selectInput("col_hpp", "Highest plausible (HPP)", choices = cols,
        selected = grep("Highest|HPP", cols, ignore.case = TRUE, value = TRUE)[1]),
      
      selectInput("col_AC", "Assessment confidence (AC; optional)", choices = c("<none>", cols),
        selected = {hit <- grep( "Assessment.*Confidence|Confidence|Belief|DoB",
                                 cols,ignore.case = TRUE, value = TRUE )
          if (length(hit) > 0) hit[1] else "<none>"
        }
      ),
      
      selectInput("col_round", "Elicitation round (optional)", choices = c("<none>", cols),
        selected = { hit <- grep("^Round$|Elicitation.*Round|Response.*Round",
            cols,ignore.case = TRUE, value = TRUE)
          if (length(hit) > 0) hit[1] else "<none>"
        }
      )
    )
  })
  
  # Multi-question filter UI (select many or "All")
  output$question_filter_ui <- renderUI({
    req(raw_df(), input$col_question)
    if (identical(input$col_question, "<none>")) return(NULL)
    qs <- unique(raw_df()[[input$col_question]])
    qs <- qs[order(as.numeric(qs))]
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
      AC_col = AC_col_val,
      round_col = round_col_val)
    
    if (!validation$valid) {
      
      showModal(
        modalDialog(
          title = "Input Data Validation Failed",
          
          tags$p(
            "The analysis was not run because problems were detected in the input data."
          ),
          
          tags$p(
            "Please correct the following issue(s) and run the analysis again:"
          ),
          
          tags$ul(
            lapply(
              validation$problems,
              function(problem) {
                tags$li(problem)
              }
            )
          ),
          footer = modalButton("Close"),
          easyClose = TRUE,
          size = "l"
        )
      )
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
    
    # -------------------------------------------------------------------------
    # Single-round analysis
    # -------------------------------------------------------------------------
    if (is.null(round_col_val)) {
      
      res_list <- lapply(qs, function(q) {
        df_q <- df %>%
          filter(.data[[q_col]] == q)
        summarize_question_pert(
          df_q,
          id_col  = input$col_id,
          lpp_col = input$col_lpp,
          bgp_col = input$col_bgp,
          hpp_col = input$col_hpp,
          AC_col = AC_col_val,
          lambda  = input$lambda,
          Nsim    = input$Nsim,
          seed    = NULL,
          question_label = as.character(q))
      })
      
      names(res_list) <- as.character(qs)
      
      # -------------------------------------------------------------------------
      # Multi-round analysis
      # -------------------------------------------------------------------------
    } else {
      
      res_list <- lapply(qs, function(q) {
        df_q <- df %>%
          filter(.data[[q_col]] == q)
        rounds_q <- sort(unique(df_q[[round_col_val]]))
        round_results <- lapply(rounds_q, function(r) {
          df_qr <- df_q %>%
            filter(.data[[round_col_val]] == r)
          summarize_question_pert(
            df_qr,
            id_col  = input$col_id,
            lpp_col = input$col_lpp,
            bgp_col = input$col_bgp,
            hpp_col = input$col_hpp,
            AC_col = AC_col_val,
            lambda  = input$lambda,
            Nsim    = input$Nsim,
            seed    = NULL,
            question_label = as.character(q))
        })
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
              result_q$AC_unavailable_reason,
              "fewer_than_two"
            )) {
              affected_fewer_than_two <- c(
                affected_fewer_than_two,
                paste0("Question ", q))
            } else if (identical(
              result_q$AC_unavailable_reason,
              "all_zero")) {
              affected_all_zero <- c(
                affected_all_zero,
                paste0("Question ", q))
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
              label <- paste0(
                "Question ", q,
                " (Round ", r, ")")
              if (identical(
                result_qr$AC_unavailable_reason,
                "fewer_than_two"
              )) {
                affected_fewer_than_two <- c(
                  affected_fewer_than_two,
                  label)
              } else if (identical(
                result_qr$AC_unavailable_reason,
                "all_zero"
              )) {
                affected_all_zero <- c(affected_all_zero, label)
              }
            }
          }
        }
      }
      
      
      # ---------------------------------------------------------------
      # Fewer than two Assessment Confidence responses
      # ---------------------------------------------------------------
      
      if (length(affected_fewer_than_two) > 0) {
        showNotification(
          paste0(
            "Assessment Confidence-weighted results are unavailable for ",
            paste(
              affected_fewer_than_two,
              collapse = ", "
            ),
            " because fewer than two participants provided Assessment Confidence. ",
            "Equal-weight results remain available."
          ),
          type = "warning",
          duration = 10
        )
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
        })
      )
    } else {
      # ==============================================================
      # MULTI-ROUND DATA
      # Flatten Question -> Round -> Result into data frames
      # ==============================================================
      
      summary_all <- bind_rows(
        lapply(names(res_list), function(q) {
          
          bind_rows(
            lapply(names(res_list[[q]]), function(rnd) {
              
              res_list[[q]][[rnd]]$summary %>%
                mutate(Round = rnd, .after = Question)
            })
          )
        })
      )
      
      dens_mix_all <- bind_rows(
        lapply(names(res_list), function(q) {
          
          bind_rows(
            lapply(names(res_list[[q]]), function(rnd) {
              
              res_list[[q]][[rnd]]$facet$mixture %>%
                mutate(Round = rnd, .after = Question)
            })
          )
        })
      )
      
      dens_mix_w_all <- bind_rows(
        lapply(names(res_list), function(q) {
          
          bind_rows(
            lapply(names(res_list[[q]]), function(rnd) {
              
              res_list[[q]][[rnd]]$facet$mixture_w %>%
                mutate(Round = rnd, .after = Question)
            })
          )
        })
      )
      
      dens_ind_all <- bind_rows(
        lapply(names(res_list), function(q) {
          
          bind_rows(
            lapply(names(res_list[[q]]), function(rnd) {
              
              res_list[[q]][[rnd]]$facet$individual %>%
                mutate(Round = rnd, .after = Question)
            })
          )
        })
      )
      
      beta_all <- bind_rows(
        lapply(names(res_list), function(q) {
          
          bind_rows(
            lapply(names(res_list[[q]]), function(rnd) {
              
              res_list[[q]][[rnd]]$facet$beta %>%
                mutate(Round = rnd, .after = Question)
            })
          )
        })
      )
      
      beta_w_all <- bind_rows(
        lapply(names(res_list), function(q) {
          
          bind_rows(
            lapply(names(res_list[[q]]), function(rnd) {
              
              res_list[[q]][[rnd]]$facet$beta_w %>%
                mutate(Round = rnd, .after = Question)
            })
          )
        })
      )
      
      cdf_mix_all <- bind_rows(
        lapply(names(res_list), function(q) {
          
          bind_rows(
            lapply(names(res_list[[q]]), function(rnd) {
              
              res_list[[q]][[rnd]]$facet$cdf_mixture %>%
                mutate(Round = rnd, .after = Question)
            })
          )
        })
      )
      
      cdf_mix_w_all <- bind_rows(
        lapply(names(res_list), function(q) {
          
          bind_rows(
            lapply(names(res_list[[q]]), function(rnd) {
              
              res_list[[q]][[rnd]]$facet$cdf_mixture_w %>%
                mutate(Round = rnd, .after = Question)
            })
          )
        })
      )
      
      cdf_emp_all <- bind_rows(
        lapply(names(res_list), function(q) {
          
          bind_rows(
            lapply(names(res_list[[q]]), function(rnd) {
              
              res_list[[q]][[rnd]]$facet$cdf_emp %>%
                mutate(Round = rnd, .after = Question)
            })
          )
        })
      )
      
      cdf_emp_w_all <- bind_rows(
        lapply(names(res_list), function(q) {
          
          bind_rows(
            lapply(names(res_list[[q]]), function(rnd) {
              
              res_list[[q]][[rnd]]$facet$cdf_emp_w %>%
                mutate(Round = rnd, .after = Question)
            })
          )
        })
      )
      
      cdf_beta_all <- bind_rows(
        lapply(names(res_list), function(q) {
          
          bind_rows(
            lapply(names(res_list[[q]]), function(rnd) {
              
              res_list[[q]][[rnd]]$facet$cdf_beta %>%
                mutate(Round = rnd, .after = Question)
            })
          )
        })
      )
      
      cdf_beta_w_all <- bind_rows(
        lapply(names(res_list), function(q) {
          
          bind_rows(
            lapply(names(res_list[[q]]), function(rnd) {
              
              res_list[[q]][[rnd]]$facet$cdf_beta_w %>%
                mutate(Round = rnd, .after = Question)
            })
          )
        })
      )
      
      cdf_ind_all <- bind_rows(
        lapply(names(res_list), function(q) {
          
          bind_rows(
            lapply(names(res_list[[q]]), function(rnd) {
              
              res_list[[q]][[rnd]]$facet$cdf_individual %>%
                mutate(Round = rnd, .after = Question)
            })
          )
        })
      )
      
      samples_all <- bind_rows(
        lapply(names(res_list), function(q) {
          
          bind_rows(
            lapply(names(res_list[[q]]), function(rnd) {
              
              tibble(
                Question = q,
                Round = rnd,
                samples = res_list[[q]][[rnd]]$samples
              )
            })
          )
        })
      )
      
      # Create combined Question-Round labels for faceting
      # Define Question x Round facet order so Round 2 appears
      # directly below Round 1 for each question block
      questions_order <- unique(as.character(summary_all$Question))
      
      rounds_order <- sort(
        unique(as.character(summary_all$Round))
      )
      
      n_facet_cols <- choose_round_facet_cols(
        length(questions_order)
      )
      
      question_round_levels <- unlist(
        lapply(
          seq(
            1,
            length(questions_order),
            by = n_facet_cols
          ),
          function(i) {
            qs_block <- questions_order[
              i:min(
                i + n_facet_cols - 1,
                length(questions_order)
              )
            ]
            unlist(
              lapply(
                rounds_order,
                function(rnd) {
                  paste0(
                    "Question ", qs_block,
                    "\nRound ", rnd
                  )
                }
              )
            )
          }
        )
      )
      
      cdf_mix_all <- cdf_mix_all %>%
        mutate(
          Question_Round = factor(
            paste0(
              "Question ", Question,
              "\nRound ", Round
            ),
            levels = question_round_levels
          )
        )
      
      cdf_mix_w_all <- cdf_mix_w_all %>%
        mutate(
          Question_Round = factor(
            paste0(
              "Question ", Question,
              "\nRound ", Round
            ),
            levels = question_round_levels
          )
        )
      
      cdf_emp_all <- cdf_emp_all %>%
        mutate(
          Question_Round = factor(
            paste0(
              "Question ", Question,
              "\nRound ", Round
            ),
            levels = question_round_levels
          )
        )
      
      cdf_emp_w_all <- cdf_emp_w_all %>%
        mutate(
          Question_Round = factor(
            paste0(
              "Question ", Question,
              "\nRound ", Round
            ),
            levels = question_round_levels
          )
        )
      
      cdf_beta_all <- cdf_beta_all %>%
        mutate(
          Question_Round = factor(
            paste0(
              "Question ", Question,
              "\nRound ", Round
            ),
            levels = question_round_levels
          )
        )
      
      cdf_beta_w_all <- cdf_beta_w_all %>%
        mutate(
          Question_Round = factor(
            paste0(
              "Question ", Question,
              "\nRound ", Round
            ),
            levels = question_round_levels
          )
        )
      
      cdf_ind_all <- cdf_ind_all %>%
        mutate(
          Question_Round = factor(
            paste0(
              "Question ", Question,
              "\nRound ", Round
            ),
            levels = question_round_levels
          )
        )
      
      dens_mix_all <- dens_mix_all %>%
        mutate(
          Question_Round = factor(
            paste0(
              "Question ", Question,
              "\nRound ", Round
            ),
            levels = question_round_levels
          )
        )
      
      dens_mix_w_all <- dens_mix_w_all %>%
        mutate(
          Question_Round = factor(
            paste0(
              "Question ", Question,
              "\nRound ", Round
            ),
            levels = question_round_levels
          )
        )
      
      dens_ind_all <- dens_ind_all %>%
        mutate(
          Question_Round = factor(
            paste0(
              "Question ", Question,
              "\nRound ", Round
            ),
            levels = question_round_levels
          )
        )
      
      beta_all <- beta_all %>%
        mutate(
          Question_Round = factor(
            paste0(
              "Question ", Question,
              "\nRound ", Round
            ),
            levels = question_round_levels
          )
        )
    }
    
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
  
  output$download_participants_png <- downloadHandler(
    
    filename = function() {
      paste0("expert_estimates_", Sys.Date(), ".png")
    },
    
    content = function(file) {
      
      req(results())
      
      r <- results()
      dims <- export_dimensions(r)
      
      g <- build_individuals_plot(
        df_raw = raw_df(),
        id_col = input$col_id,
        lpp_col = input$col_lpp,
        bgp_col = input$col_bgp,
        hpp_col = input$col_hpp,
        question_col = input$col_question,
        
        round_col = if (
          !is.null(input$col_round) &&
          !identical(input$col_round, "<none>")
        ) {
          input$col_round
        } else {
          NULL
        },
        
        selected_questions = input$question_multi,
        use_export_theme = TRUE,
        theme_export = theme_export,
        facet_cols = dims$facet_cols
      )
      
      ggsave(
        filename = file,
        plot = g,
        width = dims$width,
        height = dims$height,
        dpi = 600,
        units = "in",
        bg = "white",
        limitsize = FALSE
      )
    }
  )
  
  output$download_density_png <- downloadHandler(
    
    filename = function() {
      paste0("density_", Sys.Date(), ".png")
    },
    
    content = function(file) {
      
      req(results())
      
      r <- results()
      dims <- export_dimensions(r)
      
      g <- build_density_plot(
        r,
        show_individual = isTRUE(input$show_individual),
        show_beta = isTRUE(input$show_beta),
        show_AC_mix = isTRUE(input$show_AC_mix),
        facet_cols = dims$facet_cols
      )
      
      # Export-specific styling
      g_export <- g +
        theme_export +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.text = element_text(size = 9),
          legend.key.width = grid::unit(1.2, "cm")
        )
      
      ggsave(
        filename = file,
        plot = g_export,
        width = dims$width,
        height = dims$height,
        dpi = 600,
        units = "in",
        bg = "white",
        limitsize = FALSE
      )
    }
  )
  
  output$download_hist_png <- downloadHandler(
    
    filename = function() {
      paste0("histogram_", Sys.Date(), ".png")
    },
    
    content = function(file) {
      
      req(results())
      
      r <- results()
      dims <- export_dimensions(r)
      
      g <- build_hist_plot(
        r,
        show_beta = isTRUE(input$show_beta),
        show_AC_mix = isTRUE(input$show_AC_mix),
        facet_cols = dims$facet_cols
      )
      
      g_export <- g +
        theme_export +
        theme(
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.text = element_text(size = 9),
          legend.key.width = grid::unit(1.2, "cm")
        )
      
      ggsave(
        filename = file,
        plot = g_export,
        width = dims$width,
        height = dims$height,
        dpi = 600,
        units = "in",
        bg = "white",
        limitsize = FALSE
      )
    }
  )
  
  output$download_cdf_png <- downloadHandler(
    
    filename = function() {
      paste0("cdf_", Sys.Date(), ".png")
    },
    
    content = function(file) {
      
      req(results())
      
      r <- results()
      dims <- export_dimensions(r)
      
      g <- build_cdf_plot(
        r,
        show_individual = isTRUE(input$show_individual),
        show_beta = isTRUE(input$show_beta),
        show_AC_mix = isTRUE(input$show_AC_mix),
        facet_cols = dims$facet_cols
      )
      
      g_export <- g +
        theme_export +
        theme(
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.text = element_text(size = 9),
          legend.key.width = grid::unit(1.2, "cm")
        )
      
      ggsave(
        filename = file,
        plot = g_export,
        width = dims$width,
        height = dims$height,
        dpi = 600,
        units = "in",
        bg = "white",
        limitsize = FALSE
      )
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
      
      # Determine appropriate export layout
      dims <- export_dimensions(r)
      facet_cols <- dims$facet_cols
      show_ind     <- isTRUE(input$show_individual)
      show_beta    <- isTRUE(input$show_beta)
      show_AC_mix <- isTRUE(input$show_AC_mix)
      
      # Build plots (respecting current toggle states)
      g1 <- build_density_plot(r, show_individual = show_ind,
                               show_beta = show_beta, show_AC_mix = show_AC_mix,
                               facet_cols = facet_cols)
      g2 <- build_hist_plot(r, show_beta = show_beta, show_AC_mix = show_AC_mix,
                            facet_cols = facet_cols)
      g3 <- build_cdf_plot(r, show_individual = show_ind,
                           show_beta = show_beta, show_AC_mix = show_AC_mix,
                           facet_cols = facet_cols)
      g4 <- build_individuals_plot(
        df_raw = raw_df(),
        id_col = input$col_id,
        lpp_col = input$col_lpp,
        bgp_col = input$col_bgp,
        hpp_col = input$col_hpp,
        question_col = input$col_question,
        
        round_col = if (
          !is.null(input$col_round) &&
          !identical(input$col_round, "<none>")
        ) {
          input$col_round
        } else {
          NULL
        },
        
        selected_questions = input$question_multi,
        use_export_theme = TRUE,
        theme_export = theme_export,
        facet_cols = dims$facet_cols
      )
      
      g1 <- g1 +
        theme_export +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 9)
        )
      
      g2 <- g2 +
        theme_export +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 9)
        )
      
      g3 <- g3 +
        theme_export +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 9)
        )
      
      # Save all figures to a temp directory
      outdir <- tempfile("plots_")
      dir.create(outdir, showWarnings = FALSE)
      
      f1 <- file.path(outdir, paste0("density_",   Sys.Date(), ".png"))
      f2 <- file.path(outdir, paste0("histogram_", Sys.Date(), ".png"))
      f3 <- file.path(outdir, paste0("cdf_",       Sys.Date(), ".png"))
      f4 <- file.path(outdir, paste0("individuals_", Sys.Date(), ".png"))
      
      ggsave(
        f1, g1,
        width = dims$width,
        height = dims$height,
        dpi = 600,
        units = "in",
        bg = "white",
        limitsize = FALSE
      )
      
      ggsave(
        f2, g2,
        width = dims$width,
        height = dims$height,
        dpi = 600,
        units = "in",
        bg = "white",
        limitsize = FALSE
      )
      
      ggsave(
        f3, g3,
        width = dims$width,
        height = dims$height,
        dpi = 600,
        units = "in",
        bg = "white",
        limitsize = FALSE
      )
      
      ggsave(
        f4, g4,
        width = dims$width,
        height = dims$height,
        dpi = 600,
        units = "in",
        bg = "white",
        limitsize = FALSE
      )
      
      # Write the summary CSV
      f_summary_csv <- file.path(outdir, paste0("summary_", Sys.Date(), ".csv"))
      write_csv(r$summaries, f_summary_csv)
      
      # Zip them up
      oldwd <- setwd(outdir); on.exit(setwd(oldwd), add = TRUE)
      zip(zipfile = file, files = basename(c(f1, f2, f3, f4,f_summary_csv)))
    }
  )
  
  output$plot_participants <- renderPlot({
    req(results())
    
    build_individuals_plot(
      df_raw = raw_df(),
      id_col = input$col_id,
      lpp_col = input$col_lpp,
      bgp_col = input$col_bgp,
      hpp_col = input$col_hpp,
      question_col = input$col_question,
      round_col = if (
        !is.null(input$col_round) &&
        !identical(input$col_round, "<none>")
      ) {
        input$col_round
      } else {
        NULL
      },
      selected_questions = input$question_multi
    )
  })
  
  # Plots: Mixture density (faceted by full label)
  output$plot_density <- renderPlot({
    req(results())
    
    build_density_plot(
      r = results(),
      show_individual = isTRUE(input$show_individual),
      show_beta       = isTRUE(input$show_beta),
      show_AC_mix    = isTRUE(input$show_AC_mix),
      facet_cols      = NULL   
    )
  })
  
  # Plots: Histogram + Beta fit (faceted by full label)
  output$plot_hist <- renderPlot({
    req(results())
    
    build_hist_plot(
      r = results(),
      show_beta       = isTRUE(input$show_beta),
      show_AC_mix    = isTRUE(input$show_AC_mix),
      facet_cols      = NULL
    )
  })
  
  # Plots: CDF comparison (faceted by full label)
  output$plot_cdf <- renderPlot({
    req(results())
    
    build_cdf_plot(
      r = results(),
      show_individual = isTRUE(input$show_individual),
      show_beta       = isTRUE(input$show_beta),
      show_AC_mix    = isTRUE(input$show_AC_mix),
      facet_cols      = NULL
    )
  })
  
  # Visual summary cards
  output$summary_dashboard <- renderUI({
    
    req(results())
    
    s <- results()$summaries
    
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
          
          tags$span(
            style = "color: #666666;",
            "Select a single question to display the visual summary."
          )
        )
      )
    }
    
    if ("Round" %in% names(s) && nrow(s) == 2) {
      multi_round <- TRUE
      s1 <- s %>% filter(as.character(Round) == "1")
      s2 <- s %>% filter(as.character(Round) == "2")
      
    } else {
      multi_round <- FALSE
      s1 <- s
      s2 <- s
    }
    if (!multi_round) {
      
      # original cards
      
    } else {
      
      # round comparison cards
      
    }
    
    # Use Round 2 metrics in cards for now
    s <- s2
    eqw_delta <- s2$EqW_Mean - s1$EqW_Mean
    AC_delta <- s2$AC_Mean - s1$AC_Mean
    conf_delta <- s2$Mean_AC - s1$Mean_AC
    
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
    color: ",
        colour,
        ";
    "
      )
    }
    
    label_style <- "
    color: #666666;
    font-size: 13px;
    margin-top: 5px;
  "
    
    fluidRow(
      # Pooled mean
      column(
        4,
        div(
          style = card_style,
          div(
            style = value_style(delta_colour(eqw_delta)),
            sprintf("%+.3f", eqw_delta)
          ),
          div(
            style = label_style,
            "Δ Equal-weight mean"
          )
        )
      ),
      
      # AC weighted mean
      column(
        4,
        div(
          style = card_style,
          div(
            style = value_style(delta_colour(AC_delta)),
            sprintf("%+.3f", AC_delta)
          ),
          div(
            style = label_style,
            "Δ AC-weighted mean"
          )
        )
      ),
      
      # Mean Assessment confidence
      column(
        4,
        div(
          style = card_style,
          div(
            style = value_style(delta_colour(conf_delta)),
            paste0(
              sprintf("%+.0f", conf_delta),
              "%"
            )
          ),
          div(
            style = label_style,
            "Δ Assessment confidence"
          )
        )
      ),
      
      # Equal-weight means across rounds
      column(
        4,
        div(
          style = card_style,
          div(
            style = value_style("#245674"),
            paste0(
              sprintf("%.3f", s1$EqW_Mean),
              " \u2192 ",
              sprintf("%.3f", s2$EqW_Mean)
            )
          ),
          div(
            style = label_style,
            "Equal-weight mean (R1 \u2192 R2)"
          )
        )
      ),
      
      # AC-weighted means across rounds
      column(
        4,
        div(
          style = card_style,
          div(
            style = value_style("#245674"),
            paste0(
              sprintf("%.3f", s1$AC_Mean),
              " \u2192 ",
              sprintf("%.3f", s2$AC_Mean)
            )
          ),
          div(
            style = label_style,
            "AC-weighted mean (R1 \u2192 R2)"
          )
        )
      ),
      
      # Number of experts
      column(4,div(style = card_style,
                   div(
                     style = value_style("#245674"), s$N_Participants),
                   div(style = label_style,"Experts"))))
  })
  
  output$round_comparison_table <- renderTable({
    
    req(results())
    
    summ <- results()$summaries
    
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
      select(
        Question,
        EqW_Mean_R1 = EqW_Mean,
        AC_Mean_R1 = AC_Mean,
        Mean_AC_R1 = Mean_AC
      )
    
    r2 <- summ %>%
      filter(as.character(Round) == "2") %>%
      select(
        Question,
        EqW_Mean_R2 = EqW_Mean,
        AC_Mean_R2 = AC_Mean,
        Mean_AC_R2 = Mean_AC
      )
    
    left_join(r1, r2, by = "Question") %>%
      mutate(
        `Δ Equal-weight mean` =
          EqW_Mean_R2 - EqW_Mean_R1,
        
        `Δ AC-weighted mean` =
          AC_Mean_R2 - AC_Mean_R1,
        
        `Δ Assessment confidence` =
          Mean_AC_R2 - Mean_AC_R1
      ) %>%
      rename(
        `Equal-weight mean (R1)` = EqW_Mean_R1,
        `Equal-weight mean (R2)` = EqW_Mean_R2,
        
        `AC-weighted mean (R1)` = AC_Mean_R1,
        `AC-weighted mean (R2)` = AC_Mean_R2,
        
        `Average Assessment confidence (R1)` = Mean_AC_R1,
        `Average Assessment confidence (R2)` = Mean_AC_R2
      ) %>%
    mutate(
      across(where(is.numeric), ~ round(.x, 2))
    )
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
