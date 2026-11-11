# Task 1: Data Cleaning and Preliminary Analysis with R
# Dataset: iris_with_missing.csv

# 1. Install packages if needed (run once)
# install.packages("ggplot2")

# 2. Load packages
library(ggplot2)

# 3. Load dataset (keep CSV in the same folder as this script)
data <- read.csv("iris_with_missing.csv", stringsAsFactors = FALSE)

# 4. Inspect the dataset
cat("---- Structure ----\\n")
str(data)
cat("\\n---- First 6 rows ----\\n")
print(head(data))
cat("\\n---- Summary before cleaning ----\\n")
print(summary(data))

# 5. Missing-value analysis
cat("\\n---- Missing values by column ----\\n")
print(colSums(is.na(data)))

# Fill missing numeric values with the median of each column
numeric_cols <- names(data)[sapply(data, is.numeric)]
for (col in numeric_cols) {
  data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
}

# Fill missing categorical values with the most frequent value (mode)
mode_value <- function(x) {
  x_non_missing <- x[!is.na(x) & x != ""]
  if (length(x_non_missing) == 0) return("Unknown")
  names(sort(table(x_non_missing), decreasing = TRUE))[1]
}
categorical_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
for (col in categorical_cols) {
  data[[col]][is.na(data[[col]]) | data[[col]] == ""] <- mode_value(data[[col]])
}

# 6. Remove duplicate rows
data <- data[!duplicated(data), ]

# 7. Outlier detection using the IQR rule (numeric columns)
outlier_counts <- sapply(numeric_cols, function(col) {
  x <- data[[col]]
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- IQR(x, na.rm = TRUE)
  sum(x < (q1 - 1.5 * iqr_value) | x > (q3 + 1.5 * iqr_value), na.rm = TRUE)
})
cat("\\n---- Outlier counts (IQR rule) ----\\n")
print(outlier_counts)

# 8. Normalize numeric columns using min-max scaling
normalized_data <- data
for (col in numeric_cols) {
  min_value <- min(data[[col]], na.rm = TRUE)
  max_value <- max(data[[col]], na.rm = TRUE)
  if (max_value != min_value) {
    normalized_data[[col]] <- (data[[col]] - min_value) / (max_value - min_value)
  } else {
    normalized_data[[col]] <- 0
  }
}

# 9. Encode categorical columns as factors
for (col in categorical_cols) {
  data[[col]] <- as.factor(data[[col]])
}

# 10. Exploratory Data Analysis
cat("\\n---- Summary after cleaning ----\\n")
print(summary(data))
cat("\\n---- Data dimensions ----\\n")
print(dim(data))

# Correlation matrix for numeric variables
if (length(numeric_cols) >= 2) {
  correlation_matrix <- cor(data[, numeric_cols, drop = FALSE], use = "complete.obs")
  cat("\\n---- Correlation matrix ----\\n")
  print(round(correlation_matrix, 3))
  write.csv(correlation_matrix, "correlation_matrix.csv")
}

# 11. Visualizations (save plots as PNG)
for (col in numeric_cols) {
  p <- ggplot(data, aes(x = .data[[col]])) +
    geom_histogram(bins = 20, fill = "orange", color = "black") +
    labs(title = paste("Distribution of", col), x = col, y = "Frequency") +
    theme_minimal()
  ggsave(paste0("histogram_", col, ".png"), plot = p, width = 7, height = 5)
}

# Boxplots for numeric columns
if (length(numeric_cols) > 0) {
  png("numeric_boxplots.png", width = 1000, height = 700)
  boxplot(data[, numeric_cols, drop = FALSE], main = "Boxplots of Numeric Variables",
          col = "orange", las = 2)
  dev.off()
}

# Iris-specific scatter plot (if expected columns exist)
if (all(c("Sepal.Length", "Petal.Length") %in% names(data))) {
  p <- ggplot(data, aes(x = Sepal.Length, y = Petal.Length)) +
    geom_point(color = "darkorange", alpha = 0.7) +
    labs(title = "Sepal Length vs Petal Length") +
    theme_minimal()
  ggsave("sepal_vs_petal_scatter.png", plot = p, width = 7, height = 5)
}

# 12. Save cleaned and normalized datasets
write.csv(data, "iris_cleaned.csv", row.names = FALSE)
write.csv(normalized_data, "iris_normalized.csv", row.names = FALSE)

cat("\\nAnalysis complete! Files saved: iris_cleaned.csv, iris_normalized.csv, plots, and correlation_matrix.csv (if applicable).\\n")
