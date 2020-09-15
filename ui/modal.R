print("[DEBUG][SERVER] Loading Modal")

landing_modal <- function(failed = FALSE) {
  
  # print("[DEBUG][SERVER] Starting Modal")
  
  data_loaded <<- FALSE
  
  modalDialog(
    title = "Choose your data:",
    h3("Please choose a dataset to use :", align = "center"),
    if (failed) h4(tags$b("Error while loading file !", style = "color: red;")),
    hr(),
    fluidRow(
      align = "center",
      box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        radioButtons(
          "data_source",
          "Data source:",
          choices = c("Default dataset" = "default", "Upload a file" = "upload"),
          inline = TRUE
        )
      ),
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("file", "Choose CSV or Excel File to upload:",
                  multiple = TRUE,
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",
                    ".xls",
                    ".xlsx"
                  )
        )
      )
    ),
    easyClose = FALSE,
    footer = tagList(
      actionButton("start", "Start")
    )
  )
}