library(patchwork)

plot_loading <- function(loading_vector, pc_number) {
  df <- data.frame(
    variable = names(loading_vector),
    loading  = as.numeric(loading_vector)
  )
  
  ggplot(df, aes(
    x = reorder(variable, abs(loading)),
    y = loading
  )) +
    geom_bar(stat = "identity") +
    geom_col(width = 1) +
    coord_flip() +
    labs(
      title = paste("Loadings for PC", pc_number),
      x = "Variable",
      y = "Loading"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.y = element_text(size = 7),           # smaller label text
      plot.title = element_text(size = 14, face = "bold")
    )
}

plots <- lapply(1:5, function(i) {
  plot_loading(pca_result$rotation[, i], i)
})

plots[[1]] | plots[[2]] 
plots[[3]] | plots[[4]]
plots[[5]]