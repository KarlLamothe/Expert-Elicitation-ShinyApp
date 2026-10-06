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
  
  # Load either demo data or latest Google Sheet data
  raw_df <- reactive({
    if (isTRUE(input$use_demo)) {
      demo_df
    } else {
      req(sheet_data())
      sheet_data()
    }
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
    
    # If one or more questions chosen (not "All"), filter to those
    if (has_q && !is.null(input$question_multi)) {
      sel <- setdiff(input$question_multi, "All")
      if (length(sel) > 0) {
        df <- df %>% filter(.data[[q_col]] %in% sel)
      }
    }
    
    qs <- unique(df[[q_col]])
    
    # Summarize per selected questions (random seed each run)
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
        seed    = NULL,                # random seed inside
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
  
  
  ###########################################################################
  # Single PNG downloads (use current selections in results()) 
  ###########################################################################
  # Determine export dimensions from the number of displayed questions
  export_dimensions <- function(r, facet_cols = 4) {
    n_questions <- nrow(r$summaries)
    # Never use more facet columns than there are questions
    n_cols <- min(facet_cols, n_questions)
    n_rows <- ceiling(n_questions / n_cols)
    list(
      facet_cols = n_cols,
      width = max(8, 2.6 * n_cols),
      height = max(4.5, 3.0 * n_rows)
    )
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
        show_dob_mix = isTRUE(input$show_dob_mix),
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
        show_dob_mix = isTRUE(input$show_dob_mix),
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
        show_dob_mix = isTRUE(input$show_dob_mix),
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
        df_raw = raw_df(),
        id_col = input$col_id,
        lpp_col = input$col_lpp,
        bgp_col = input$col_bgp,
        hpp_col = input$col_hpp,
        question_col = input$col_question,
        selected_questions = input$question_multi,
        use_export_theme = TRUE,
        theme_export = theme_export,
        facet_cols = facet_cols
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
      selected_questions = input$question_multi,
      facet_cols = NULL   # ✅ adaptive
    )
  })
  
  # Plots: Mixture density (faceted by full label)
  output$plot_density <- renderPlot({
    req(results())
    
    build_density_plot(
      r = results(),
      show_individual = isTRUE(input$show_individual),
      show_beta       = isTRUE(input$show_beta),
      show_dob_mix    = isTRUE(input$show_dob_mix),
      facet_cols      = NULL   # ✅ adaptive
    )
  })
  
  # Plots: Histogram + Beta fit (faceted by full label)
  output$plot_hist <- renderPlot({
    req(results())
    
    build_hist_plot(
      r = results(),
      show_beta       = isTRUE(input$show_beta),
      show_dob_mix    = isTRUE(input$show_dob_mix),
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
      show_dob_mix    = isTRUE(input$show_dob_mix),
      facet_cols      = NULL
    )
  })
  
  # Visual summary cards
  output$summary_dashboard <- renderUI({
    
    req(results())
    
    s <- results()$summaries
    
    # Summary cards are intended for a single selected question
    if (nrow(s) != 1) {
      
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
    
    value_style <- "
    font-size: 28px;
    font-weight: 600;
    color: #245674;
  "
    
    label_style <- "
    color: #666666;
    font-size: 13px;
    margin-top: 5px;
  "
    
    fluidRow(
      # Pooled mean
      column(3,div(style = card_style, 
                   div(style = value_style,sprintf("%.2f", s$EqW_Mean)),
                   div(style = label_style, "Pooled mean"))),
      # 90% interval
      column(3, div(style = card_style,
                    div(style = value_style,
                        paste0(sprintf("%.2f", s$EqW_5th), " – ",
                               sprintf("%.2f", s$EqW_95th))),
                    div(style = label_style, "90% interval"))),
      # DoB weighted mean
      column(2,div(style = card_style,
                   div(style = value_style,sprintf("%.2f", s$DoB_Mean)),
                   div(style = label_style, "DoB-weighted mean"))),
      
      # Mean Degree of Belief
      column(2, div(style = card_style,
                    div(style = value_style,
                        paste0(round(s$Mean_DoB),"%")),
                    div(style = label_style, "Mean DoB"))),
      
      # Number of experts
      column(2,div(style = card_style,
                   div(style = value_style, s$N_Participants),
                   div(style = label_style,"Experts"))))
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
