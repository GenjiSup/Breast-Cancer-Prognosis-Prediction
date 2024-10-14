# List of additional required packages
additional_packages <- c("caret", "ggplot2", "reshape2", "wesanderson")

# Check and install missing packages
new_additional_packages <- additional_packages[!(additional_packages %in% installed.packages()[,"Package"])]
if (length(new_additional_packages)) {
  install.packages(new_additional_packages)
}

# Load necessary libraries
library(caret)
library(ggplot2)
library(reshape2)
library(wesanderson)

# Get a palette from wesanderson
palette <- wes_palette("Chevalier1", 4, type = "discrete")

# Function to plot a colored confusion matrix
confusionPlot <- function(conf_matrix, title = "Confusion Matrix") {
  # Convert the confusion matrix into a table
  conf_matrix_table <- as.table(conf_matrix)
  
  # Check if the confusion matrix has any data
  if (sum(conf_matrix_table) == 0) {
    stop("Confusion matrix is empty. Please check your inputs.")
  }
  
  # Convert the table into a data frame for ggplot
  conf_matrix_melt <- melt(conf_matrix_table)
  
  # Ensure that Var1 and Var2 have valid factor levels
  label_map <- c("will_die_within_10_years" = "Will die within 10 years", 
                 "will_die_within_5_years" = "Will die within 5 years")
  
  # Replace factor levels with readable labels only if matching levels exist
  if (all(levels(conf_matrix_melt$Var1) %in% names(label_map)) &&
      all(levels(conf_matrix_melt$Var2) %in% names(label_map))) {
    conf_matrix_melt$Var1 <- factor(conf_matrix_melt$Var1, levels = names(label_map), labels = label_map)
    conf_matrix_melt$Var2 <- factor(conf_matrix_melt$Var2, levels = names(label_map), labels = label_map)
  } else {
    stop("Factor levels in the confusion matrix do not match the expected labels.")
  }
  
  # Add a column for the type of prediction (TP, TN, FP, FN)
  conf_matrix_melt$PredictionType <- with(conf_matrix_melt, ifelse(Var1 == Var2 & Var1 == "Will die within 10 years", "True Positive",
                                                                   ifelse(Var1 == Var2 & Var1 == "Will die within 5 years", "True Negative",
                                                                          ifelse(Var1 != Var2 & Var1 == "Will die within 10 years", "False Positive",
                                                                                 "False Negative"))))
  
  # Plot the confusion matrix using ggplot2
  ggplot(conf_matrix_melt, aes(Var1, Var2, fill = PredictionType)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("True Positive" = palette[1], 
                                 "True Negative" = palette[2], 
                                 "False Positive" = palette[3], 
                                 "False Negative" = palette[4])) +
    geom_text(aes(label = value), color = "black", size = 5) +    # Add counts in the tiles
    labs(title = title,
         x = "Predicted Label", 
         y = "True Label") +
    theme_minimal() +                                             # Clean minimal theme
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
          axis.text.x = element_text(angle = 45, hjust = 1),       # Rotate x-axis labels
          axis.text.y = element_text(face = "bold"))               # Bold y-axis labels
}
