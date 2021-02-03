# Reactive dataframes

print("[DEBUG][SERVER] Loading Controls")

df_react <- reactive({
  df %>% filter_data(input$layers, input$genes)
}) %>% bindEvent(input$go)


summary <- compute_summary(df)

summary_react <- reactive({
  summary %>% filter_data(input$layers, input$genes)
}) %>% bindEvent(input$go)

fit <- compute_fit(df)

fit_react <- reactive({
  fit %>% filter_data(input$layers, input$genes)
}) %>% bindEvent(input$go)

statistics <- compute_statistics(df)

statistics_react <- reactive({
  statistics %>% filter_data(input$layers, input$genes)
}) %>% bindEvent(input$go)

# ncbi <- reactiveVal()
# # being.fetched <- reactiveVal()
# 
# observe({
#   
#   ncbi.temp <- isolate(ncbi())
#   # being.fetched.temp <- isolate(being.fetched())
# 
#   # if(!is.null(being.fetched.temp)) print(glue("[INFO][NCBI] Ignoring, already being fetched : {unlist(being.fetched.temp)}"))
#   ncbi.to_fetch <- df_react() %>% filter(gene != "All" & gene %ni% ncbi.temp$gene) %>% distinct(gene) #& gene %ni% being.fetched.temp
#   # being.fetched <- append(being.fetched, ncbi.to_fetch$gene)
#   
#   if(length(ncbi.to_fetch$gene) > 0) {
#     print(glue("[INFO][NCBI] Fetching info for genes : {unlist(ncbi.to_fetch$gene)}"))
#     
#     # being.fetched(append(being.fetched.temp, ncbi.to_fetch$gene))
# 
#     ## Starting future
#     future({
#       # q$consumer$consume(throwErrors=FALSE)
#       fetch_ncbi_info(ncbi.to_fetch)
#     }) %...>% (function(data) {
#       
#       if(is.null(ncbi.temp)) {
#         # print("[DEBUG][NCBI] ncbi.temp is NULL !!")
#         ncbi(data)
#       } else {
#         if(is.null(data)) {
#           print("[ERROR][NCBI] Fetched data is null")
#         }
#         else {
#           ncbi(rbind(ncbi.temp, data))
#         }
#       }
#       
#       # being.fetched(being.fetched.temp[being.fetched.temp %ni% ncbi.to_fetch$gene])
#       
#     }) %...!% # Assign to data
#       (function(e) {
#         #ncbi(NULL)
#         # being.fetched(being.fetched.temp[being.fetched.temp %ni% ncbi.to_fetch$gene])
#         print("[ERROR][NCBI] Error in future process")
#         warning(e)
#       })
#     
#   } else {
#     # print("[DEBUG][NCBI] No new genes to fetch")
#   }
#   
#   # Hide the async operation from Shiny by not having the promise be the last expression.
#   NULL
# })
# 
# ncbi_react <- reactive({
#   req(ncbi()) %>% filter_genes(input$genes)
# })

# observe({
# 
#   ncbi(NULL)
# 
#   future({
#     q$consumer$consume(throwErrors=FALSE)
#     fetch_ncbi_info(df)
#   }) %...>% (function(data) {
#     ncbi(data)
#   }) %...!% # Assign to data
#     (function(e) {
#       ncbi(NULL)
#       print("[ERROR] Error in future process")
#       warning(e)
#     })
# 
#   # Hide the async operation from Shiny by not having the promise be the last expression.
#   NULL
# })


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