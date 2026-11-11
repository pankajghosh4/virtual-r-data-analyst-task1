# Week 2: Data Visualization and Insight Communication using R
# Dataset: Iris (150 rows; 3 species)
# Install once if needed: install.packages("ggplot2")
library(ggplot2)

iris_data <- read.csv("iris_week2.csv")
str(iris_data)
summary(iris_data)

# 1. Bar chart: average petal length by species
ggplot(iris_data, aes(x = species, y = petal_length, fill = species)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(title = "Average Petal Length by Species",
       x = "Species", y = "Mean petal length (cm)") +
  theme_minimal() + theme(legend.position = "none")
ggsave("bar_chart_R.png", width = 7, height = 4)

# 2. Scatter plot: sepal length vs petal length
ggplot(iris_data, aes(x = sepal_length, y = petal_length, color = species)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Sepal Length vs Petal Length",
       x = "Sepal length (cm)", y = "Petal length (cm)") +
  theme_minimal()
ggsave("scatter_plot_R.png", width = 7, height = 4)

# 3. Histogram: petal length distribution
ggplot(iris_data, aes(x = petal_length)) +
  geom_histogram(bins = 18, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Petal Length",
       x = "Petal length (cm)", y = "Frequency") +
  theme_minimal()
ggsave("histogram_R.png", width = 7, height = 4)

# 4. Line chart: species-wise mean petal length
species_means <- aggregate(petal_length ~ species, data = iris_data, mean)
ggplot(species_means, aes(x = species, y = petal_length, group = 1)) +
  geom_line(color = "darkgreen") + geom_point(size = 3, color = "darkgreen") +
  labs(title = "Mean Petal Length Across Species",
       x = "Species", y = "Mean petal length (cm)") +
  theme_minimal()
ggsave("line_chart_R.png", width = 7, height = 4)

# Correlation (numeric variables only)
cor(iris_data[, c("sepal_length", "sepal_width", "petal_length", "petal_width")])
