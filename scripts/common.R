###
# This file contains re-usable functions
###

library(readr)

load_data <- function(min_n = 0) {
  combined_data <- read_delim("../derived_data/combined_data.csv", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
  
  combined_data = combined_data[combined_data$n >= min_n, ]
  
  # Subset the data by columns
  list(
    distric_id = unlist(combined_data[, 1]),
    turbine = combined_data[, 2:17],
    n = unlist(combined_data[, 18]),
    district_name = unlist(combined_data[, 19]),
    census = combined_data[, 20:38]
  )
}


# Simple linear regression
model_and_plot <- function(x, y, axis_labels = list("x", "y")) {
  # Read more at: http://r-statistics.co/Linear-Regression.html
  
  # Weights are now linearly proportional to the sample size.
  # TODO: It should somehow reflect the confidence
  model <- lm(y ~ x,
              weights = data$n)
  
  plot(x, y,
       cex = sqrt(data$n / max(data$n)) * 2,
       pch = 16,
       xlab = axis_labels[[1]],
       ylab = axis_labels[[2]])
  
  abline(model, col = "#44ee22")
  
  text(x, y,
       labels = data$district_name,
       col = "#404040",
       cex = 0.5,
       adj = c(-0.1, 0.5))
  
  # TODO: Weights
  title(paste("Correlation:", cor(y, x)))
  
  #print(summary(model))
  # TODO: p-value to title or somewhere
}

# Pretty plot of correlations
plot_correlations <- function() {
  # https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
  library(corrplot)
  library(weights)
  
  local_data <- data
  
  # TODO: The questions must be shorter
  colnames(local_data$turbine) <- paste0(substring(colnames(local_data$turbine), 1, 100))
  
  # Create a correlation matrix, turbine survey vs. census
  M <- with(local_data, wtd.cor(census, turbine, n))
  
  # Plot it
  corrplot(M$correlation,
           p.mat = M$p.value,
           insig = "blank",
           method = "circle",
           tl.cex = 0.7,
           tl.col = "black")
}
