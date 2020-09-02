source("global.R", echo = FALSE)

########################################[ UI ]########################################

header <- dashboardHeader(
  title = shinyDashboardLogo(
    theme = "blue_gradient",
    boldText = span(tagList(icon("cat"), "\  Q-PCR")),
    mainText = "Data explorer",
    badgeText = "v1"
  )
)

# ------------------------------------------------------------------

source(here("ui", "sidebar.R"), local = TRUE)$value

# ------------------------------------------------------------------

body <- dashboardBody(
  useShinyjs(),
  
  shinyDashboardThemes(
    theme = "onenote"
  ),
  
  tabItems(
  source(here("ui", "data_tab_view.R"), local = TRUE)$value,
  source(here("ui", "analysis_tab_view.R"), local = TRUE)$value
  )
)

ui <- dashboardPage(header = header, sidebar = sidebar, body = body) # skin = "blue"


#######################################[ Server ]#######################################

source(here("utils", "entrez.R"), echo = FALSE)
source(here("utils", "data.R"), local = TRUE, echo = FALSE)

source(here("ui", "modal.R"),  local = TRUE)$value

source(here("server", "data_tab_module.R"),  local = TRUE)$value
source(here("server", "analysis_tab_module.R"),  local = TRUE)$value

server <- function(input, output, session) {
  
  #shinyjs::onevent("beforeunload", "window", onStop()) #function (e) { e.preventDefault(); e.returnValue = ''; }
  
  # Loading data
  
  showModal(landing_modal())
  
  observe({
    toggleState(id = "start", condition = ifelse(input$data_source == "upload", !is.null(input$file), TRUE))
  })
  
  observeEvent(input$start, {
    removeModal()
    if (input$data_source == "upload") {
      if (is.null(input$file)) {
        showModal(landing_modal(failed = TRUE))
      }
      data_loaded <<- parse_data(input$file$datapath)
      if (!data_loaded) {
        showModal(landing_modal(failed = TRUE))
      }
    } else {
      data_loaded <<- parse_data(data_path)
      ## This should not happen, it means the default data is not read properly
      if (!data_loaded) {
        showModal(landing_modal(failed = TRUE))
      }
    }
  })
  
  # ----------------------------------------
  
  q <- queue()

  onStop <- function() {
    q$producer$fireEval(stop("Stop that child"))
    q$destroy()
    stopApp()
  }
  
  # Starting the application
  
  observeEvent(input$start, {
    req(data_loaded)

    source(here("server", "controls.R"),  local = TRUE)$value
    source(here("server", "utils.R"),  local = TRUE)$value
    
    source(here("server", "data_tab_controller.R"),  local = TRUE)$value
    source(here("server", "analysis_tab_controller.R"),  local = TRUE)$value
    
    session$onSessionEnded(function() {
      onStop()
    })
  })
}


shinyApp(ui = ui, server = server)