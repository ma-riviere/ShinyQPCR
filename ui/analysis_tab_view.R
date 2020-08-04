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
          DT::dataTableOutput("fit") %>% withSpinner()
        )
      ),
      fluidRow(
        box(
          width = 6,
          title = "QQ Plot",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          fluidRow(
            uiOutput("qq_stack") %>% withSpinner()
          )
        ),
        box(
          width = 6,
          title = "Histogram fit",
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
          DT::dataTableOutput("statistics") %>% withSpinner()
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
    )
  )
)