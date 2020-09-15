# Analysis

## Fit

### Fit table

output$fit <- DT::renderDT({
  datatable(
    fit_react(),
    class = "cell-border stripe compact",
    filter = "none",
    options = list(pageLength = 10, autoWidth = TRUE)
  ) %>%
    formatRound(5:ncol(fit_react()), 3) %>%
    formatStyle(
      c("levene.p", "resid.SW.p", "resid.AD.p"),
      color = styleInterval(alpha, c("red", "gray")),
      fontWeight = styleInterval(alpha, c("bold", "normal")),
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
    formatRound(3:(ncol(statistics_react())-2), 3) %>%
    formatStyle("levene.p",
                color = styleInterval(alpha, c("red", "gray")),
                fontWeight = styleInterval(alpha, c("bold", "normal"))
    ) %>%
    formatStyle("p.val",
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

output$heatmap <- renderPlot(
  df_react() %>% 
    select(-sample) %>%
    filter(condition == "H") %>%
    ggplot(aes(x=couche, y=gene)) +
    geom_tile(data = . %>% filter(expression == regulation_type$NOT_REG), fill="grey70", colour="white") +
    geom_tile(data = . %>% filter(expression == regulation_type$MAYBE_UPREG), fill="darkolivegreen3", colour="white") +
    geom_tile(data = . %>% filter(expression == regulation_type$MAYBE_DOWNREG), fill="darkorange2", colour="white") +
    geom_tile(data = . %>% filter(expression == regulation_type$UPREG), aes(fill=fold), colour="white") +
    scale_fill_gradient(name=regulation_type$UPREG, low="seagreen1", high="seagreen4") + 
    new_scale_fill() +
    geom_tile(data = . %>% filter(expression == regulation_type$DOWNREG), aes(fill=fold), colour="white") +
    scale_fill_gradient(name=regulation_type$DOWNREG, low="firebrick4", high="firebrick1") + 
    geom_text(aes(label = paste0(round(fold, 3), get_stars(expression, p.val), sep="")), size = 2.5, colour = "white", fontface = "bold") +
    coord_fixed(ratio = 0.25) +
    theme_minimal() +
    theme(
      legend.text=element_text(face="bold"),
      axis.ticks=element_line(size=0.4),
      panel.grid.major = element_blank(),
    ) +
    ylab("Gene") +
    xlab("Couche")
)