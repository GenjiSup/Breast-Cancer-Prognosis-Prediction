library(caret)
library(ggplot2)
library(RColorBrewer)

PCoA_proximity_plot <- function(data, outcome_var, n_trees = 1000, mtry_value = 2, pc_pairs = 1:4) {
  # Remove rows with NA values from the data
  data_complete <- data[complete.cases(data), ]
  
  # Ensure outcome_var is treated as a column name
  outcome_data <- data_complete[[outcome_var]]
  
  # Build Random Forest model with proximity calculation
  rf_model <- randomForest(x = data_complete[, !names(data_complete) %in% outcome_var], 
                           mtry = mtry_value, 
                           ntree = n_trees, 
                           proximity = TRUE)
  
  # Extract proximity matrix
  proximity_mat <- rf_model$proximity
  
  # Function to perform PCoA and plot for specified PCs
  PCoA_plot <- function(data, pc_x, pc_y) {
    PCoA_res <- prcomp(data, center = TRUE, scale. = TRUE)
    
    # Create a data frame for the selected principal components
    PCoA_df <- data.frame(
      PCx = PCoA_res$x[, pc_x],
      PCy = PCoA_res$x[, pc_y],
      Outcome = as.factor(outcome_data)
    )
    
    # Calculate the explained variance
    explained_var <- PCoA_res$sdev^2 / sum(PCoA_res$sdev^2) * 100
    variance_labels <- paste0("PCo", c(pc_x, pc_y), ": ", round(explained_var[c(pc_x, pc_y)], 2), "%")
    
    # Plot the PCoA with a qualitative color scale
    ggplot(PCoA_df, aes(x = PCx, y = PCy, color = Outcome)) +  # Treat Outcome as a factor
      geom_point() +
      labs(
        title = paste("PCoA Plot: PCo", pc_x, "vs PCo", pc_y),
        x = variance_labels[1], y = variance_labels[2]
      ) +
      theme_bw(base_size = 14) +
      theme(legend.position = "right") +
      scale_color_brewer(palette = "Set1")  # Use a qualitative color palette
  }
  
  # Store and create plots for each PCoA pair
  PCoA_plots <- list()
  pc_combinations <- combn(pc_pairs, 2, simplify = FALSE)
  for (i in seq_along(pc_combinations)) {
    pc_x <- pc_combinations[[i]][1]
    pc_y <- pc_combinations[[i]][2]
    PCoA_plots[[i]] <- PCoA_plot(proximity_mat, pc_x, pc_y)
  }
  
  # Return the list of PCoA plots
  return(PCoA_plots)
}
