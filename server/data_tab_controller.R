# Data

## Tables

output$data <- DT::renderDT({
  df <- df_react() %>% select(couche, gene, sample, litter, condition, dct, fold, expression)
  datatable(
    df,
    class = "cell-border stripe compact",
    filter = "top",
    options = list(
      pageLength = 25,
      autoWidth = TRUE
    )
  ) %>% formatRound(6:7, digits = 4)
})

output$dl.raw.all <- downloadHandler(
  filename = function() {"raw_data_full.xlsx"},
  content = function(file) {writexl::write_xlsx(df %>% select(couche, gene, sample, litter, condition, dct, fold, expression), path = file)}
)

output$dl.raw.filtered <- downloadHandler(
  filename = function() {"raw_data_filtered.xlsx"},
  content = function(file) {writexl::write_xlsx(df_react() %>% select(couche, gene, sample, litter, condition, dct, fold, expression), path = file)}
)

# output$ncbi <- DT::renderDT({
#   datatable(
#     ncbi_react() %>% mutate(gene = get.gene_link(gene, gene.id)) %>% select(-gene.id),
#     escape = FALSE,
#     class = "cell-border stripe compact",
#     filter = "none",
#     options = list(
#       pageLength = 25,
#       autoWidth = TRUE
#     )
#   )
# })

output$summary <- DT::renderDT({
  datatable(
    summary_react(),
    class = "cell-border stripe compact",
    filter = "none",
    options = list(
      pageLength = 20,
      autoWidth = TRUE
    )
  ) %>%
    formatRound(5:ncol(summary_react()), 4) # %>% formatStyle("sd", color = styleInterval(0.7, c("gray", "orange")))
})


## Hist stack

observe({
  req(df_react())
  req(input$go, input$histograms_kde, input$histograms_nbin)

  df_temp <- df_react() %>%
    group_by(couche, gene, condition) %>%
    mutate(
      med = median(dct),
      mean = mean(dct),
    ) %>%
    ungroup() %>%
    mutate(uid = paste0(couche, " - ", gene))


  output$hist_stack <- renderUI({
    df_temp %>%
      group_by(couche, gene) %>%
      group_map(~ make_hist_stack(.x$uid[1]))
  })

  df_temp %>%
    group_by(couche, gene) %>%
    group_map(~ callModule(
      render_hist_stack,
      id = .x$uid[1],
      data = .x,
      options = list(nbins = input$histograms_nbin, kde = input$histograms_kde)
    ))
})

## Violin stack

observe({
  req(df_react())
  req(input$go, input$violins_kde)

  df_temp <- df_react() %>%
    group_by(couche, gene, condition) %>%
    mutate(med = median(dct)) %>%
    ungroup() %>%
    mutate(uid = paste0(couche, " - ", gene))


  output$violin_stack <- renderUI({
    df_temp %>%
      group_by(couche, gene) %>%
      group_map(~ make_violin_stack(.x$uid[1]))
  })

  df_temp %>%
    group_by(couche, gene) %>%
    group_map(~ callModule(
      render_violin_stack,
      id = .x$uid[1],
      data = .x,
      options = list(kde = input$violins_kde)
    ))
})