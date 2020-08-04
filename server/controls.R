# Reactive dataframes

df_react <- reactive({
  df %>% filter_data(input$layers, input$genes)
})


summary <- compute_summary(df)

summary_react <- reactive({
  summary %>% filter_data(input$layers, input$genes)
})


fit <- compute_fit(df)

fit_react <- reactive({
  fit %>% filter_data(input$layers, input$genes)
})


statistics <- compute_statistics(df)

statistics_react <- reactive({
  statistics %>% filter_data(input$layers, input$genes)
})


ncbi <- reactiveVal()

observe({
  
  ncbi(NULL)
  
  future({
    q$consumer$consume(throwErrors=FALSE)
    fetch_ncbi_info(df)
  }) %...>% (function(data) {
    ncbi(data)
  }) %...!% # Assign to data
    (function(e) {
      ncbi(NULL)
      print("[ERROR] Error in future process")
      warning(e)
    })

  # Hide the async operation from Shiny by not having the promise be the last expression.
  NULL
})

ncbi_react <- reactive({
  req(ncbi()) %>% filter_genes(input$genes)
})


# Controls

output$dynamic_layer_selection <- renderUI({
  current_layers <- c("All", unique(df$couche))

  selectInput(
    inputId = "layers",
    label = "Layers:",
    choices = current_layers,
    selected = current_layers[2],
    multiple = T
  )
})

observeEvent(input$layers, {
  current_genes <- c("All", get_genes(input$layers))

  output$dynamic_gene_selection <- renderUI({
    selectInput(
      inputId = "genes",
      label = "Genes:",
      choices = current_genes,
      selected = current_genes[2],
      multiple = T
    )
  })
})