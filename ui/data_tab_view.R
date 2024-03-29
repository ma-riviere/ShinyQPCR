tabItem(
  tabName = "data",
  
  tabsetPanel(
    tabPanel(
      "Raw Data",
      br(),
      fluidRow(
        box(
          width = 12,
          title = "Raw data",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          DT::dataTableOutput("data") %>% withSpinner(),
          downloadButton("dl.raw.all", label = "All"),
          downloadButton("dl.raw.filtered", label = "Filtered")
        )
      )
    ),
    
    
    # tabPanel(
    #   "Entrez Info",
    #   br(),
    #   fluidRow(
    #     box(
    #       width = 12,
    #       title = "NCBI (Entrez) data",
    #       status = "primary",
    #       solidHeader = TRUE,
    #       collapsible = TRUE,
    #       DT::dataTableOutput("ncbi") %>% withSpinner()
    #     )
    #   )
    # ),
    
    
    tabPanel(
      "Summary",
      br(),
      fluidRow(
        box(
          width = 12,
          title = "Summary statistics",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          DT::dataTableOutput("summary") %>% withSpinner()
        )
      )
    ),
    
    
    tabPanel(
      "Visualizations",
      br(),
      
      fluidRow(
        ### Histograms
        box(
          width = 6,
          title = "Histograms",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          fluidRow(
            box(width = 6, sliderInput("histograms_nbin", "Bins:", 1, 50, 10)),
            box(width = 6, sliderInput("histograms_kde", "Smoothing:", min = 0.1, max = 2, value = 1, step = 0.1))
          ),
          fluidRow(
            uiOutput("hist_stack") %>% withSpinner()
          )
        ),
        ### Violins
        box(
          width = 6,
          title = "Violins",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          fluidRow(
            box(
              width = 12,
              sliderInput("violins_kde", "Smoothing:", min = 0.1, max = 2, value = 1, step = 0.1)
            )
          ),
          fluidRow(
            uiOutput("violin_stack") %>% withSpinner()
          )
        )
      )
    )
  )
)