---
title: "A quantitative approach to the municipal formation of political power in a hybrid regime: the case of La Banda in Santiago del Estero"
author: "Sabrina Victoria Corbacho"
date: "2022"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r cars, message=FALSE, warning=FALSE}
# Loading Required Libraries
pacman::p_load(ggplot2, factoextra, gridExtra)

# Dataset Preparation
data <- data.frame(
  Alignment = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Gender = c(1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1),
  Urbanization = c(1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Industrial.Profile = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)

# Summary Statistics
print("Summary Statistics:")
summary_stats <- summary(data)
print(summary_stats)


# Themes & Colors for Plotting
custom_theme <- theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
color_pal <- c("#AEDFF7", "#FFD3A5", "#A2EAC2", "#F5A9B8")

# Correlation Matrix
print("Correlation Matrix:")
correlation_matrix <- cor(data)
print(correlation_matrix)

# Principal Component Analysis (PCA)
scaled_data <- scale(data)
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# PCA Summary & Plots
print("PCA Summary:")
print(summary(pca_result))

print("PCA Plots:")
screeplot(pca_result, type="lines")
biplot(pca_result)

fviz_eig(pca_result)
fviz_pca_var(pca_result, col.var="contrib", gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"), repel=TRUE)
print("Rotation Matrix:")
print(pca_result$rotation)

explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumul_var <- cumsum(explained_var)
print(data.frame(PrincipalComponent = 1:length(cumul_var), CumulativeVariance = cumul_var))

# K-means Clustering
set.seed(123)
km.res <- kmeans(scaled_data, centers = 3, nstart = 25)
fviz_cluster(km.res, data = scaled_data)

# PCA Biplot using ggbiplot
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)

data$Gender <- as.factor(data$Gender)

biplot_gg <- ggbiplot(pca_result, obs.scale = 1, var.scale = 1, groups = data$Gender, ellipse = TRUE, circle = TRUE, ellipse.prob = 0.68)
biplot_gg <- biplot_gg + scale_color_discrete(name = '')
biplot_gg <- biplot_gg + theme(legend.direction = 'horizontal', legend.position = 'top')
print(biplot_gg)


# Convert binary variables to factors with explicit levels
data$Alignment <- factor(data$Alignment, levels = c(0, 1))
data$Gender <- factor(data$Gender, levels = c(0, 1))
data$Urbanization <- factor(data$Urbanization, levels = c(0, 1))
data$Industrial.Profile <- factor(data$Industrial.Profile, levels = c(0, 1))


# Exploratory Data Analysis (EDA) Visualizations
print("Exploratory Data Analysis Plots:")
plot_variables <- c("Alignment", "Gender", "Urbanization", "Industrial.Profile")
plots <- list()
for (var in plot_variables) {
  plots[[var]] <- ggplot(data, aes_string(x = var, fill = var)) +
    geom_bar(show.legend = FALSE) +
    labs(title = paste0(var)) +
    xlab(var) +
    ylab("Frequency") +
    scale_fill_manual(values = color_pal) +
    custom_theme
}
```
