# -------------------------------------------------------------------------
# QQ plots
# -------------------------------------------------------------------------

make_qq_stack <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      width = 12,
      title = id,
      plotlyOutput(ns("my_qq"))
    )
  )
}

render_qq_stack <- function(input, output, session, data, options) {
  output$my_qq <- renderPlotly({
    qq <- data %>%
      ggplot(aes(sample = dct, colour = condition)) + # group = gene
      geom_qq() +
      geom_qq_line() +
      scale_color_manual(values = colors) +
      facet_wrap(~condition) +
      theme_minimal() +
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
    
    ggplotly(qq) %>%
      layout(
        xaxis = list(title = "Theoretical"),
        yaxis = list(title = "Sample") # range = c(0,9)
      )
  })
}

# -------------------------------------------------------------------------
# Fit histograms
# -------------------------------------------------------------------------

make_fit_stack <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      width = 12,
      title = id,
      plotlyOutput(ns("my_fit"))
    )
  )
}

render_fit_stack <- function(input, output, session, data, options) {
  
  # norm.fit <- fitdist(data$dct, "norm")$estimate
  # gof.norm <- ad.test(data$dct, "norm", norm.fit[1], norm.fit[2])$p.value %>% round(4)
  
  output$my_fit <- renderPlotly({
    fit <- data %>%
      group_by(condition) %>%
      mutate(mean = mean(dct)) %>%
      ungroup() %>%
      ggplot(aes(x = dct, fill = condition, color = condition)) +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors) +
      geom_histogram(aes(y = ..density..), binwidth = 0.5, alpha = 0.8, fill = "white", boundary = 1) +
      # geom_density(kernel = "gaussian") + #adjust = 5
      # geom_function(fun = dnorm, args = norm.fit, size = 1.3) + #aes(color = glue("Norm : {gof.norm}"))
      # geom_function(fun = dlnorm, args = lognorm.fit, aes(color = glue("Log.Norm : {gof.lnorm}")), size = 1.3) +
      # geom_function(fun = dgamma, args = gamma.fit, aes(color = glue("Gamma : {gof.gamma}")), size = 1.3) +
      theme_minimal() +
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      facet_wrap(. ~ condition) +
      geom_vline(aes(xintercept = med, group = condition, color = condition, label = "Median"), linetype = "dashed", size = 1) +
      #ggrepel::geom_text_repel(aes(x = med + 0.1 * (max(dct) - min(dct)), label = round(med, 2), y = -0.05)) +
      geom_vline(aes(xintercept = mean, group = condition, color = condition, label = "Mean"), size = 1)
      #ggrepel::geom_text_repel(aes(x = mean - 0.1 * (max(dct) - min(dct)), label = round(mean, 2), y = -0.05))
    
    ggplotly(fit) %>%
      layout(
        xaxis = list(title = "Delta CT"),
        yaxis = list(title = "Density")
      )
  })
}

# -------------------------------------------------------------------------
# Box plots
# -------------------------------------------------------------------------

make_box_stack <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      width = 4,
      title = id,
      plotlyOutput(ns("my_box"))
    )
  )
}

render_box_stack <- function(input, output, session, data, options) {
  output$my_box <- renderPlotly({
    box <- ggboxplot(data, x = "condition", y = "dct", color = "condition", xlab = "Condition d'hypoxie", ylab = "Delta CT") +
      theme(legend.position = "none") +
      scale_color_manual(values = colors) +
      stat_compare_means(label = "p.signif", label.x = 1.5, method = "t.test") +
      geom_jitter(aes(color = condition, label = sample))
    # facet_wrap(~ couche + gene, ncol = wrap_ncol, scales = "free_y")
    
    ggplotly(box)
  })
}