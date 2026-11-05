# Card helper
card_box <- function(...) {
  div(style = "background: white;
      border-top: 4px solid #1F5A7A;
      border-left: 1px solid #dee2e6;
      border-right: 1px solid #dee2e6;
      border-bottom: 1px solid #dee2e6;
      border-radius: 8px;
      padding: 12px;
      margin-bottom: 10px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15);",...)
  }

title_card <- function(title, subtitle) {
  div(style = "background:#f8f9fa;
      border:1px solid #dee2e6;
      border-left:4px solid #1F5A7A;
      border-radius:8px;
      padding:12px;
      margin-top:10px;
      margin-bottom:15px;",
      h3(title, style = "margin-top:0; margin-bottom:5px;"),
      p(subtitle, style = "margin-bottom:0;"))}

############################################################################# 
# User interface
############################################################################
ui <- fluidPage(
  tags$head(tags$style(HTML("
  .nav-link.active {
  font-weight: 700 !important;
  font-size: 1.05em;}
  .nav-pills .nav-link {
  color: #1F5A7A !important;
  }
  .nav-pills .nav-link:hover {
  background-color: #eef4f7;
  }
  .nav-pills .nav-link.active {
  background-color: #1F5A7A !important;
  color: white !important;
  }"))),
  
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#1F5A7A",
                   secondary = "#6C757D", success = "#4C956C"),
  
  div(style = "
    background-color: #f5f5f5;
    border:1px solid #dee2e6;
    border-radius:8px;
    padding:15px;
    margin-bottom:15px;
    margin-top:15px;
  ",
    
    h2("Expert Elicitation Application", style = "margin-top:0; margin-bottom:5px;"),
    p(class = "text-muted",
      "Visualize expert estimates, pooled distributions, and elicitation results.",
      style = "margin-bottom:0;")),
  
  sidebarLayout(sidebarPanel(
    width=3,
    conditionalPanel(
      condition = "input.main_tab == 'expert'",
      card_box(tags$details(
        open = TRUE,
        tags$summary(style = "cursor:pointer; font-weight:600; 
                     font-size:18px; user-select:none;",
                     icon("database"), " Data"),
        br(),
        radioButtons("data_source", "Data source",
                     choices = c("Demo data" = "demo",
                                 "Upload CSV" = "csv",
                                 "Google Sheets" = "google"),
                     selected = "demo"),
        conditionalPanel(condition = "input.data_source == 'csv'",
                         fileInput("csv_file", "Upload Expert Responses",
                                   accept = c(".csv", "text/csv")),
                         downloadButton("download_template",
                                        "Download Response Template",
                                        class = "btn-default btn-sm"),
                         br(),
                         helpText(tags$small(tags$b("Note: "),
                           "Question_Text is optional but recommended. When provided, the question is displayed within the application when a single question is selected."))),
        conditionalPanel(condition = "input.data_source == 'google'",
                         actionButton("refresh_sheet",
                                      strong("Get Latest Expert Responses"),
                                      class = "btn-primary btn-sm"))))),
    
    # ============================================================
    # QUESTIONS
    # ============================================================
    card_box(tags$details(open = TRUE,
        tags$summary(style = "cursor:pointer; font-weight:600; 
          font-size:18px; user-select:none;",
          icon("list-check"), " Questions"),
        br(),
        uiOutput("question_filter_ui"),
        uiOutput("round_filter_ui"))),
    
    # ============================================================
    # ANALYSIS OPTIONS
    # ============================================================
    conditionalPanel(condition = "input.main_tab != 'expert' &&
    input.main_tab != 'summary'",
    card_box(tags$details(
      open = TRUE,
      tags$summary(
      style = "cursor: pointer; font-weight: 600; 
      font-size: 18px; user-select:none;",
      icon("sliders"), "Analysis Options"),
      br(),
      conditionalPanel(condition = "input.main_tab != 'hist'",
                       checkboxInput("show_individual",
                                     "Show individual expert curves", TRUE)),
      checkboxInput("show_AC_mix",
                    "Show assessment confidence-weighted mixture", TRUE),
      checkboxInput("show_beta", "Show Beta approximation", TRUE),
      tags$details(tags$summary(
        style = "cursor: pointer; font-weight: 600; user-select:none;",
        "Advanced analysis settings"),
        br(),
        numericInput("lambda", "PERT shape (lambda)", value = 4, min = 1, step = 1),
        numericInput("Nsim", "Mixture draws", value = 10000, min = 1000, step = 1000))))),
    
    # ============================================================
    # ADVANCED COLUMN MAPPING
    # ============================================================
    conditionalPanel(condition = "input.main_tab == 'expert'",
                     card_box(tags$details(tags$summary(
      style = "cursor: pointer; font-weight: 600; font-size: 18px; user-select:none;",
      icon("gear"),"Column Mapping"),
      br(),
      helpText(
        "Change these settings only if the input data columns are not detected correctly."),
      uiOutput("colmap_ui")))),
    
    # ============================================================
    # DOWNLOADS
    # ============================================================
    card_box(tags$details(tags$summary(
      style = "cursor: pointer; font-weight: 600; 
      font-size: 18px; user-select:none;", icon("download"),"Downloads"),
      tags$strong("Individual plots"),
      tags$div(style = "margin-bottom: 6px;"),
      downloadButton("download_participants_png", "Expert Estimates (PNG)", width = "100%"),
      tags$div(style = "margin-bottom: 6px;"),
      downloadButton("download_density_png", "Pooled Distributions (PNG)", width = "100%"),
      tags$div(style = "margin-bottom: 6px;"),
      downloadButton("download_hist_png", "Simulated Estimates (PNG)", width = "100%"),
      tags$div(style = "margin-bottom: 6px;"),
      downloadButton("download_cdf_png", "CDFs (PNG)", width = "100%"),
      tags$hr(),      
      tags$strong("Reporting & Exporting"),
      tags$div(style = "margin-bottom: 6px;"),
      downloadButton("download_summary", "Question Results (CSV)", width="100%"),
      tags$div(style = "margin-bottom: 6px;"),
      downloadButton("download_all_plots_zip", "Analysis Outputs (ZIP)", width="100%"),
      tags$div(style = "margin-bottom: 6px;"),
      downloadButton("download_report_html", "Facilitator Report (HTML)", width="100%"))),
    actionButton("run", icon = icon("play"), "Run / Refresh Analysis",
                 class = "btn-primary btn-lg", width = "100%"),
    br(),
    div(style = "color: #6c757d;
    font-size: 12px;
    border-top:1px solid #dee2e6;
    margin-top:20px;
    padding-top:12px;
    padding-bottom:12px;",
        tags$strong("Expert Elicitation Application"),
        tags$br(),
        "Developed by Karl Lamothe, Fisheries and Oceans Canada",
        tags$br(),
        "Version 2.2.0",
        " | Last updated: November 2026")),
    
    ############################################################################
    ############################################################################
    # main panel
    ############################################################################
    ############################################################################
    mainPanel(width=9, tabsetPanel(
      id = "main_tab",type="pills",
      tabPanel(tagList(icon("users"),"Expert Estimates"), value='expert',
               title_card("Individual Expert Estimates",
                          "Displays individual expert responses and estimates for each question and elicitation round."),
               uiOutput("question_panel_expert"),
               uiOutput("plot_participants_container")),
      
      tabPanel(tagList(icon("chart-area"),"Pooled Distributions"),
               title_card("Pooled Expert Distributions",
                          paste("Displays pooled group results derived from expert responses. Visualization type depends on the question type.")),
               uiOutput("question_panel_density"),
               uiOutput("plot_density_ui")),
      
      tabPanel(tagList(icon("chart-bar"),"Simulated Pooled Estimates"),
               value="hist",
               title_card("Simulated Pooled Estimates",
                          "Displays pooled outcomes generated from the selected pooling method and question type."),
               uiOutput("question_panel_hist"),
               uiOutput("plot_hist_ui")),
      
      tabPanel(tagList(icon("chart-line"),"CDF"),
               title_card("Cumulative Probability",
                          "Displays cumulative pooled results where applicable. Visualization type depends on the question type."),
               uiOutput("question_panel_cdf"),
               uiOutput("plot_cdf_ui")),
      
      tabPanel(tagList(icon("clipboard-list"),"Summary"),
               value="summary",
               title_card("Elicitation Summary",
                          paste("Summary statistics and pooled results for each question and elicitation round.",
                                "For multi-round elicitations, changes represent Round 2 minus Round 1.")),
               uiOutput("question_panel_summary"),
               uiOutput("summary_dashboard"),
               uiOutput("round_comparison_ui"),
               uiOutput("yesno_results_ui"),
               uiOutput("ranking_results_ui"),
               tags$hr(),
               tags$details(tags$summary(
               style = "cursor: pointer;font-weight: 600;font-size: 20px;","Detailed Results" ),
               br(),
               div(style = "font-size: 15px;", tableOutput("summary_table"))),
              
               tags$hr(),
         
              h4("Definitions"),
              tags$ul(tags$li(
                tags$b("Equal-weight mean: "),
                "Pooled estimate when all experts contribute equally."),
                tags$li(tags$b("Assessment confidence: "),
                        paste("A participant's self-reported confidence in the probability assessment.")),

                tags$li(tags$b(
                  "Confidence-weighted mean: "),
                  paste("The pooled estimate obtained when participants' relative contributions",
                        "are weighted according to their reported assessment confidence.")),
                
                tags$li(tags$b("Average assessment confidence: "),
                        "Mean assessment confidence reported by participating experts."),
                
                tags$li(tags$b("Δ Equal-weight mean: "),
                        "Round 2 minus Round 1 equal-weight pooled mean."),
                
                tags$li(tags$b("Δ Confidence-weighted mean: "),
                        "Round 2 minus Round 1 assessment confidence-weighted pooled mean."),
                
                tags$li(tags$b("Δ Assessment confidence: "),
                        "Round 2 minus Round 1 average assessment confidence.")))))))
