tabItem(
  tabName = "analysis",
  
  tabsetPanel(
    tabPanel(
      "Fit",
      br(),
      fluidRow(
        box(
          width = 12,
          title = "Fit criteria",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          DT::dataTableOutput("fit") %>% withSpinner(),
          downloadButton("dl.fit.all", label = "All"),
          downloadButton("dl.fit.filtered", label = "Filtered")
        )
      ),
      fluidRow(
        box(
          width = 6,
          title = "QQ Plot of Residuals",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          fluidRow(
            uiOutput("qq_stack") %>% withSpinner()
          )
        ),
        box(
          width = 6,
          title = "Histogram of Residuals",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          fluidRow(
            uiOutput("fit_stack") %>% withSpinner()
          )
        )
      )
    ),
    
    
    tabPanel(
      "Statistics",
      br(),
      fluidRow(
        box(
          width = 12,
          title = "Statistics",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          DT::dataTableOutput("statistics") %>% withSpinner(),
          downloadButton("dl.stats.all", label = "All"),
          downloadButton("dl.stats.filtered", label = "Filtered")
        )
      ),
      br(),
      fluidRow(
        box(
          width = 12,
          title = "Box-plots",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          fluidRow(
            uiOutput("box_stack") %>% withSpinner()
          )
        )
      )
    ),
    
    tabPanel(
      "Heatmap",
      br(),
      fluidRow(
        box(
          width = 12,
          title = "Regulation Heatmap",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          fluidRow(
            plotOutput("heatmap") %>% withSpinner()
            # downloadButton("dl.hm.full", label = "All"),
            # downloadButton("dl.hm.filtered", label = "Filtered")
          )
        )
      )
    )
  )
)