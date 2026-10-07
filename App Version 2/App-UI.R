############################################################################# 
# User interface
############################################################################
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#1F5A7A",
                   secondary = "#6C757D", success = "#4C956C"),
  titlePanel("Expert Elicitation Application"),
  sidebarLayout(
    sidebarPanel(
      h4("Data"),
      
      radioButtons(
        "data_source",
        "Data source",
        choices = c(
          "Demo data" = "demo",
          "Upload CSV" = "csv",
          "Google Sheets" = "google"
        ),
        selected = "demo"
      ),
      
      conditionalPanel(
        condition = "input.data_source == 'csv'",
        
        fileInput(
          "csv_file",
          "Upload Expert Responses",
          accept = c(".csv", "text/csv")
        ),
        
        downloadButton(
          "download_template",
          "Download Response Template",
          class = "btn-default btn-sm"
        )
      ),
      
      conditionalPanel(
        condition = "input.data_source == 'google'",
        
        actionButton(
          "refresh_sheet",
          "Get Latest Expert Responses",
          class = "btn-success"
        )
      ),
      
      br(),
      br(),
      
      # ============================================================
      # QUESTIONS
      # ============================================================
      h4("Questions"),
      uiOutput("question_filter_ui"),
      tags$hr(),
      
      # ============================================================
      # ANALYSIS OPTIONS
      # ============================================================
      tags$details(
        tags$summary(
          style = "cursor: pointer; font-weight: 600; font-size: 18px;",
          "Analysis Options"
        ),
        
        br(),
        
        checkboxInput(
          "show_individual",
          "Show individual expert curves",
          TRUE
        ),
        
        checkboxInput(
          "show_dob_mix",
          "Show Degree-of-Belief weighted mixture",
          TRUE
        ),
        
        checkboxInput(
          "show_beta",
          "Show Beta approximation",
          FALSE
        ),
        
        tags$details(
          tags$summary(
            style = "cursor: pointer; font-weight: 600;",
            "Advanced analysis settings"
          ),
          
          br(),
          
          numericInput(
            "lambda",
            "PERT shape (lambda)",
            value = 4,
            min = 1,
            step = 1
          ),
          
          numericInput(
            "Nsim",
            "Mixture draws",
            value = 10000,
            min = 1000,
            step = 1000
          )
        )
      ),
      
      br(),
      
      actionButton(
        "run",
        "Run / Refresh Analysis",
        class = "btn-primary",
        width = "100%"
      ),
      
      tags$hr(),
      
      # ============================================================
      # ADVANCED COLUMN MAPPING
      # ============================================================
      tags$details(
        tags$summary(style = "cursor: pointer; font-weight: 600; font-size: 18px;",
                     "Column Mapping"),
        br(),
        helpText(
          "Change these settings only if the input data columns are not detected correctly."),
        uiOutput("colmap_ui")),
      tags$hr(),

      # ============================================================
      # DOWNLOADS
      # ============================================================
      tags$details(tags$summary(style = "cursor: pointer; font-weight: 600; font-size: 18px;",
                                "Downloads"),
        br(),
        downloadButton("download_summary", "Summary CSV",
                       class = "btn-default btn-sm"),
        downloadButton("download_all_plots_zip", "All Outputs (ZIP)",
                       class = "btn-default btn-sm"),
        br(), br(),
        tags$strong("Individual plots"),
        br(), br(),
        fluidRow(column(6,downloadButton("download_participants_png",
                                         "Scores",class = "btn-default btn-sm",
                                         width = "100%")),
          column(6, downloadButton("download_density_png",
                                   "Distributions",
                                   class = "btn-default btn-sm",
                                   width = "100%"))),
        br(),
        fluidRow(column( 6,downloadButton( "download_hist_png","Histograms",
              class = "btn-default btn-sm",width = "100%")),
          column(6, downloadButton("download_cdf_png","CDFs",
                                   class = "btn-default btn-sm",
                                   width = "100%"))))),
    mainPanel(
      tabsetPanel(
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
              "Degree-of-Belief weighting, and Beta approximations.")),
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
              "Equal-weight and Degree-of-Belief-weighted pooled estimates are shown separately.",
              "For multi-round elicitations, changes represent Round 2 minus Round 1.")),
          br(),
          uiOutput("summary_dashboard"),
          tags$hr(),
          h4("Round Comparison"),
          
          helpText(
            "Changes in pooled estimates and confidence between Round 1 and Round 2."
          ),
          
          tableOutput("round_comparison_table"),
          
          tags$hr(),
          tags$details(
            tags$summary(
              style = "
      cursor: pointer;
      font-weight: 600;
      font-size: 20px;
    ",
              "Detailed Results"
            ),
            br(),
            
            div(
              style = "font-size: 15px;",
              tableOutput("summary_table")
            )
          ),
          
          tags$hr(),
          
          h4("Definitions"),
          
          tags$ul(
            tags$li(
              tags$b("Equal-weight mean: "),
              "Pooled estimate when all experts contribute equally."
            ),
            
            tags$li(
              tags$b("DoB-weighted mean: "),
              "Pooled estimate after weighting experts according to their reported Degree of Belief."
            ),
            
            tags$li(
              tags$b("Average Degree of Belief: "),
              "Mean Degree of Belief reported by experts."
            ),
            
            tags$li(
              tags$b("Δ Equal-weight mean: "),
              "Round 2 minus Round 1 equal-weight pooled estimate."
            ),
            
            tags$li(
              tags$b("Δ DoB-weighted mean: "),
              "Round 2 minus Round 1 DoB-weighted pooled estimate."
            ),
            
            tags$li(
              tags$b("Δ Degree of Belief: "),
              "Round 2 minus Round 1 average Degree of Belief."
            )
          )
          )
      )
    )
  )
)
