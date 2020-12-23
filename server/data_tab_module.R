# -------------------------------------------------------------------------
# Histograms
# -------------------------------------------------------------------------

make_hist_stack <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      width = 12,
      title = span(id, align = "center"),
      plotlyOutput(ns("my_hist"))
    )
  )
}

render_hist_stack <- function(input, output, session, data, options) {
  output$my_hist <- renderPlotly({
    hist <- data %>%
      ggplot(aes(x = X, fill = condition, color = condition)) +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors) +
      geom_histogram(aes(y = ..density..), bins = options$nbins, alpha = 0.8, fill = "white", boundary = 1) +
      geom_density(alpha = 0.6, adjust = options$kde) + #
      theme_minimal() +
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      facet_wrap(. ~ condition) +
      # geom_vline(aes(xintercept = med, group = condition, color = condition), linetype = "dashed", size = 1) +
      geom_vline(aes(xintercept = med, group = condition, color = condition, label = "Median"), linetype = "dashed", size = 1) +
      geom_vline(aes(xintercept = mean, group = condition, color = condition, label = "Mean"), size = 1) +
      geom_text(aes(x = med + 0.1 * (max(X) - min(X)), label = round(med, 2), y = -0.05)) # TODO: (get max of the density) + 0.1 * (max - min)
    
    ggplotly(hist) %>%
      layout(
        xaxis = list(title = ifelse(options$type == "dct", "Delta Ct", "Fold Change")),
        yaxis = list(title = "Density") # range = c(0,9)
      )
  })
}

# -------------------------------------------------------------------------
# Violins
# -------------------------------------------------------------------------

make_violin_stack <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      width = 12,
      title = span(id, align = "center"),
      plotlyOutput(ns("my_violin"))
    )
  )
}

render_violin_stack <- function(input, output, session, data, options) {
  output$my_violin <- renderPlotly({
    data %>%
      plot_ly(
        x = ~condition,
        y = ~dct,
        color = ~condition,
        text = ~sample,
        type = "violin",
        points = "all",
        box = list(visible = T),
        meanline = list(visible = T),
        bandwidth = options$kde,
        showlegend = FALSE,
        colors = colors
      ) %>%
      layout(
        xaxis = list(title = "Condition"),
        yaxis = list(title = "Delta Ct") # range = c(0,9)
      )
  })
}