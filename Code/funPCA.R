pca_proximity_plot <- function(data, outcome_var, n_trees = 2000, mtry_value = 2, pc_pairs = 1:4) {
  # Ensure outcome_var is treated as a column name
  outcome_data <- data[[outcome_var]]
  
  # Build Random Forest model with proximity calculation
  rf_model <- randomForest(x = data[, !names(data) %in% outcome_var], 
                           mtry = mtry_value, 
                           ntree = n_trees, 
                           proximity = TRUE)
  
  # Extract proximity matrix and assign row/column names based on the outcome variable
  proximity_mat <- rf_model$proximity
  rownames(proximity_mat) <- outcome_data
  colnames(proximity_mat) <- outcome_data
  
  # Function to perform PCA and plot for specified PCs
  pca_plot <- function(data, pc_x, pc_y) {
    pca_res <- prcomp(data, center = TRUE, scale. = TRUE)
    
    # Create a data frame for the selected principal components
    pca_df <- data.frame(
      PCx = pca_res$x[, pc_x],
      PCy = pca_res$x[, pc_y],
      Outcome = outcome_data
    )
    
    # Calculate the explained variance
    explained_var <- pca_res$sdev^2 / sum(pca_res$sdev^2) * 100
    variance_labels <- paste0("PC", c(pc_x, pc_y), ": ", round(explained_var[c(pc_x, pc_y)], 2), "%")
    
    # Plot the PCA
    ggplot(pca_df, aes(x = PCx, y = PCy, color = Outcome)) +
      geom_point() +
      labs(
        title = paste("PCA Plot: PC", pc_x, "vs PC", pc_y),
        x = variance_labels[1], y = variance_labels[2]
      ) +
      theme_bw(base_size = 14) +
      theme(legend.position = "right")
  }
  
  # Store and create plots for each PCA pair
  pca_plots <- list()
  pc_combinations <- combn(pc_pairs, 2, simplify = FALSE)
  for (i in seq_along(pc_combinations)) {
    pc_x <- pc_combinations[[i]][1]
    pc_y <- pc_combinations[[i]][2]
    pca_plots[[i]] <- pca_plot(proximity_mat, pc_x, pc_y)
  }
  
  # Return the list of PCA plots
  return(pca_plots)
}