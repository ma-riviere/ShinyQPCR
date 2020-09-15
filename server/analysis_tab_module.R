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
      ggplot(aes(sample = resid)) + # group = gene, colour = litter
      geom_qq(color = "skyblue2") +
      geom_qq_line(color = "skyblue2") +
      # scale_color_manual(values = colors) +
      # facet_wrap(~condition) +
      theme_minimal() +
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
    
    ggplotly(qq) %>%
      layout(
        xaxis = list(title = "Theoretical"),
        yaxis = list(title = "Observed") # range = c(0,9)
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
      plotOutput(ns("my_fit"))
    )
  )
}

render_fit_stack <- function(input, output, session, data, options) {
  
  norm.fit <- fitdistrplus::fitdist(data$resid, "norm")$estimate
  # gof.norm <- goftest::ad.test(data$resid, "norm", norm.fit[1], norm.fit[2])$p.value %>% round(4)
  
  output$my_fit <- renderPlot({
    data %>%
      ggplot(aes(x = resid)) +
      geom_histogram(aes(y = ..density..), binwidth = length(resid) / 5, alpha = 0.8, color = "skyblue2", fill = "skyblue2", boundary = 1) +
      geom_function(fun = dnorm, args = norm.fit, size = 1.3, color = "firebrick4") + #aes(color = glue("Norm : {gof.norm}"))
      theme_minimal() +
      labs(x = "Residuals", y = "Density") +
      theme(
        legend.position = "none",
        legend.title = element_blank(), 
        axis.title.x = element_text(size = (20)), 
        axis.title.y = element_text(size = (20)), 
        # legend.text = element_text(size = (16))
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