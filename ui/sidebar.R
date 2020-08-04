sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar",
    
    menuItem("Data",
             tabName = "data",
             icon = icon("database")
             # menuSubItem("Sub-item 1", tabName = "subitem1")
    ),
    
    menuItem("Analysis",
             tabName = "analysis",
             icon = icon("chart-bar")
    ),
    
    hr(),
    h4("Controls:", align = "center"),
    #checkboxInput("filter_signi", "Regulated only", value = FALSE),
    uiOutput("dynamic_layer_selection"),
    uiOutput("dynamic_gene_selection"),
    br(),
    #TODO: list packages used and provide email for feedback/contact
    tags$div("© 2020 RIVIERE Marc-Aurèle, license MIT", align="bottom")
  )
)