# Card helper
card_box <- function(...) {
  div(style = "
      background: white;
      border-top: 4px solid #1F5A7A;
      border-left: 1px solid #dee2e6;
      border-right: 1px solid #dee2e6;
      border-bottom: 1px solid #dee2e6;
      border-radius: 8px;
      padding: 15px;
      margin-bottom: 15px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.08); ",
    ...
  )
}
############################################################################# 
# User interface
############################################################################
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#1F5A7A",
                   secondary = "#6C757D", success = "#4C956C"),
  titlePanel("Expert Elicitation Application"),
  sidebarLayout(sidebarPanel(width=3,
    conditionalPanel(
      condition = "input.main_tab == 'Expert Estimates'", 
      card_box(h4(icon("database"),"Data"),
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
                                               class = "btn-default btn-sm")),
               conditionalPanel(condition = "input.data_source == 'google'",
                                actionButton("refresh_sheet",
                                             "Get Latest Expert Responses",
                                             class = "btn-success")))),
    # ============================================================
    # QUESTIONS
    # ============================================================
    card_box(h4(icon("list-check"), "Questions"),
    uiOutput("question_filter_ui"),
    uiOutput("round_filter_ui")),
    
    # ============================================================
    # ANALYSIS OPTIONS
    # ============================================================
    card_box(tags$details(tags$summary(
          style = "cursor: pointer; font-weight: 600; font-size: 18px;",
          icon("sliders"), "Analysis Options"),
          br(),
          checkboxInput("show_individual",
                        "Show individual expert curves", TRUE),
          checkboxInput("show_AC_mix",
                        "Show assessment confidence-weighted mixture", TRUE),
          checkboxInput("show_beta", "Show Beta approximation", TRUE),
          tags$details(tags$summary(
            style = "cursor: pointer; font-weight: 600;",
            "Advanced analysis settings"),
          br(),
          numericInput("lambda", "PERT shape (lambda)",
                       value = 4, min = 1, step = 1),
          numericInput("Nsim", "Mixture draws",
                       value = 10000, min = 1000, step = 1000)))),
    
    actionButton("run", icon = icon("play"), "Run / Refresh Analysis",
                 class = "btn-primary btn-lg", width = "100%"),
    br(),  
    br(),  
    
    # ============================================================
    # ADVANCED COLUMN MAPPING
    # ============================================================
    conditionalPanel(condition = "input.main_tab == 'Expert Estimates'",
                     card_box(tags$details(tags$summary(
      style = "cursor: pointer; font-weight: 600; font-size: 18px;",
      icon("gear"),"Column Mapping"),
      br(),
      helpText(
        "Change these settings only if the input data columns are not detected correctly."),
      uiOutput("colmap_ui")))),
    # ============================================================
    # DOWNLOADS
    # ============================================================
    card_box(tags$details(tags$summary(
      style = "cursor: pointer; font-weight: 600; font-size: 18px;", icon("download"),"Downloads"),
      tags$strong("Individual plots"),
      br(),
      fluidRow(column(6,downloadButton("download_participants_png",
                                       "Scores",
                                       class = "btn-default btn-sm",
                                       width = "100%")),
               column(6, downloadButton("download_density_png",
                                        "Distributions",
                                        class = "btn-default btn-sm",
                                        width = "100%"))),
        br(),
        fluidRow(column(6,downloadButton("download_hist_png","Histograms",
                                          class = "btn-default btn-sm",
                                         width = "100%")),
                 column(6, downloadButton("download_cdf_png","CDFs",
                                          class = "btn-default btn-sm",
                                          width = "100%"))),
        br(),
        tags$strong("Reporting & Export"),
        br(),
        downloadButton("download_summary", "Question Results (CSV)",
                       class = "btn-default btn-sm"),
        downloadButton("download_all_plots_zip", "Analysis Outputs (ZIP)",
                       class = "btn-default btn-sm"),
        downloadButton("download_report_html", "Facilitator Report (HTML)",
                       class = "btn-default btn-sm")))),
    
    mainPanel(width=9,
      tabsetPanel(id = "main_tab",type="pills",
        tabPanel("Expert Estimates",
          br(),
          h3("Individual Expert Estimates"),
          helpText(
            "Lowest plausible, best guess, and highest plausible estimates provided by each expert."),
          plotOutput("plot_participants",height = "600px")),
        
        tabPanel("Pooled Distributions",
          br(),
          h3("Pooled Expert Distributions"),
          helpText(
            paste(
              "PERT distributions derived from expert LPP, BGP, and HPP estimates.",
              "Use the sidebar options to display individual expert distributions,",
              "Assessment confidence weighting, and Beta approximations.")),
          plotOutput("plot_density",height = "600px")),
        
        tabPanel("Simulated Pooled Estimates",
          br(),
          h3("Simulated Pooled Estimates"),
          helpText("Monte Carlo draws from the pooled expert distributions."),
          plotOutput("plot_hist",height = "600px")),
        
        tabPanel("CDF",
          br(),
          h3("Cumulative Probability"),
          helpText(
            "Cumulative distribution functions for the pooled expert judgments."),
          plotOutput("plot_cdf",height = "600px")),
        
        tabPanel( "Summary",
          br(),
          h3("Elicitation Summary"),
          helpText(
            paste(
              "Equal-weight and assessment confidence-weighted pooled estimates are shown separately.",
              "For multi-round elicitations, changes represent Round 2 minus Round 1.")),
          br(),
          uiOutput("summary_dashboard"),
          tags$hr(),
          h4("Round Comparison"),
          
          helpText(
            "Changes in pooled estimates and assessment confidence between Round 1 and Round 2."),
          
          tableOutput("round_comparison_table"),
          
          tags$hr(),
          tags$details(
            tags$summary(style = "cursor: pointer;
                                  font-weight: 600;
                                  font-size: 20px;",
                         "Detailed Results" ),
            br(),
            div(style = "font-size: 15px;", tableOutput("summary_table"))),
          tags$hr(),
          
          h4("Definitions"),
          tags$ul(tags$li(
            tags$b("Equal-weight mean: "),
            "Pooled estimate when all experts contribute equally."),
            tags$li(tags$b("Assessment confidence: "),
                    paste(
                "A participant's self-reported confidence in the probability assessment",
                "provided for a question, considering the available evidence, relevant",
                "expertise, assumptions, and remaining uncertainty. Assessment confidence",
                "is separate from the uncertainty represented by the lowest and highest",
                "plausible probabilities.")),
            
            tags$li(tags$b("Confidence-weighted mean: "),
              paste(
                "The pooled estimate obtained when participants' relative contributions",
                "are weighted according to their reported assessment confidence.")),
            
            tags$li(tags$b("Average assessment confidence: "),
              "Mean assessment confidence reported by participating experts."),
            
            tags$li(tags$b("Δ Equal-weight mean: "),
              "Round 2 minus Round 1 equal-weight pooled mean."),
            
            tags$li(tags$b("Δ Confidence-weighted mean: "),
              "Round 2 minus Round 1 assessment confidence-weighted pooled mean."),
            
            tags$li(tags$b("Δ Assessment confidence: "),
              "Round 2 minus Round 1 average assessment confidence.")))
      )
    )
  )
)
