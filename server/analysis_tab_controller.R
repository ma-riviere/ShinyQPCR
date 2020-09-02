# Analysis

## Fit

### Fit table

#### TODO: add Normal & inverse.gaussian fits

output$fit <- DT::renderDT({
  datatable(
    fit_react(),
    class = "cell-border stripe compact",
    filter = "none",
    options = list(pageLength = 10, autoWidth = TRUE)
  ) %>%
    formatRound(5:ncol(fit_react()), 3) %>%
    formatStyle(
      c("SW.p", "AD.p", "SW.p.resid"),
      color = styleInterval(alpha, c("red", "gray")),
      fontWeight = styleInterval(alpha, c("bold", "normal")),
    ) %>%
    formatStyle(
      "mean.median",
      color = styleInterval(c(0.95, 1.05), c("orange", "gray", "orange")),
      fontWeight = styleInterval(c(0.95, 1.05), c("bold", "normal", "bold")),
    )
})

### QQ Plot stack

observe({
  req(df_react())
  req(input$layers, input$genes)
  
  df_temp <- df_react() %>%
    group_by(couche, gene, condition) %>%
    mutate(med = median(dct)) %>%
    ungroup() %>%
    mutate(uid = paste0(couche, " - ", gene))
  
  
  output$qq_stack <- renderUI({
    df_temp %>%
      group_by(couche, gene) %>%
      group_map(~ make_qq_stack(.x$uid[1]))
  })
  
  df_temp %>%
    group_by(couche, gene) %>%
    group_map(~ callModule(
      render_qq_stack,
      id = .x$uid[1],
      data = .x,
      options = list()
    ))
})

### Fit hist stack

observe({
  req(df_react())
  req(input$layers, input$genes)
  
  df_temp <- df_react() %>%
    group_by(couche, gene, condition) %>%
    mutate(med = median(dct)) %>%
    ungroup() %>%
    mutate(uid = paste0(couche, " - ", gene))
  
  
  output$fit_stack <- renderUI({
    df_temp %>%
      group_by(couche, gene) %>%
      group_map(~ make_fit_stack(.x$uid[1]))
  })
  
  df_temp %>%
    group_by(couche, gene) %>%
    group_map(~ callModule(
      render_fit_stack,
      id = .x$uid[1],
      data = .x,
      options = list()
    ))
})

## Statistics

### Significance

output$statistics <- DT::renderDT({
  datatable(
    statistics_react(),
    class = "cell-border stripe compact",
    filter = "none",
    options = list(pageLength = 25, autoWidth = TRUE)
  ) %>%
    formatRound(3:6, 3) %>%
    formatStyle("F.p",
                color = styleInterval(alpha, c("red", "gray")),
                fontWeight = styleInterval(alpha, c("bold", "normal"))
    ) %>%
    formatStyle("t.p",
                color = styleInterval(c(alpha, 0.1), c("green", "#C4DC17", "gray")),
                fontWeight = styleInterval(alpha, c("bold", "normal"))
    ) %>%
    formatStyle("Hedge.g",
                color = styleInterval(c(0.2, 0.5, 0.8), c("gray", "orange", "#C4DC17", "green")),
                fontWeight = styleInterval(0.8, c("normal", "bold"))
    ) %>%
    formatStyle("power",
                color = styleInterval(c(0.5, 0.8), c("gray", "#C4DC17", "green")),
                fontWeight = styleInterval(0.8, c("normal", "bold"))
    ) %>%
    formatStyle("add.n.for.nominal.power",
                color = styleInterval(c(1, 10), c("gray", "#C4DC17", "gray")),
                fontWeight = styleInterval(c(1, 10), c("normal", "bold", "normal"))
    )
})

### Box stack:

observe({
  req(df_react())
  req(input$layers, input$genes)
  
  df_temp <- df_react() %>%
    group_by(couche, gene, condition) %>%
    mutate(med = median(dct)) %>%
    ungroup() %>%
    mutate(uid = paste0(couche, " - ", gene))
  
  
  output$box_stack <- renderUI({
    df_temp %>%
      group_by(couche, gene) %>%
      group_map(~ make_box_stack(.x$uid[1]))
  })
  
  df_temp %>%
    group_by(couche, gene) %>%
    group_map(~ callModule(
      render_box_stack,
      id = .x$uid[1],
      data = .x,
      options = list()
    ))
})