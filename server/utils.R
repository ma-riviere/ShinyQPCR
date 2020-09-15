print("[DEBUG][SERVER] Loading Utils")

get_genes <- function(layers) {
  if (is.null(layers) || length(layers) == 0) layers <- "None"
  
  df %>%
    filter(if (length(layers) > 0 & "All" %ni% layers) {sapply(couche, toupper) %in% sapply(layers, toupper)} else TRUE) %>%
    arrange(gene) %>%
    distinct(gene) %>%
    pull(gene)
}

filter_data <- function(data, layers, genes) {
  if (is.null(genes) || length(genes) == 0) genes <- "None"
  if (is.null(layers) || length(layers) == 0) layers <- "None"

  data %>%
    filter(if (length(layers) > 0 & "All" %ni% layers) {sapply(couche, toupper) %in% sapply(layers, toupper)} else TRUE) %>%
    filter(if (length(genes) > 0 & "All" %ni% genes) {sapply(gene, toupper) %in% sapply(genes, toupper)} else TRUE)
}

filter_genes <- function(data, genes) {
  if (is.null(genes) || length(genes) == 0) genes <- "None"
  data %>% filter(if (length(genes) > 0 & "All" %ni% genes) {sapply(gene, toupper) %in% sapply(genes, toupper)} else TRUE)
}

regulated_count <- function(data) {
  length(unique(data$gene))
}