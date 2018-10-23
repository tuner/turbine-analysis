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
    census = combined_data[, 20:39]
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
       ylab = axis_labels[[2]],
       cex.axis = 0.8, cex.lab = 0.8)
  
  abline(model, col = "#44ee22")
  
  text(x, y,
       labels = data$district_name,
       col = "#404040",
       cex = 0.5,
       adj = c(-0.1, 0.5))
  
  library(weights)
  
  # TODO: Weights
  title(paste("Correlation:", wtd.cor(y, x, data$n)[1]), cex.main = 0.7)
  
  #print(summary(model))
  # TODO: p-value to title or somewhere
}


explain_variable <- function(variable) {
  library(glmnet)
  model <- glmnet(as.matrix(scale(data$census)), variable)
  
  # Use cross-validation to find good lambda
  model.cv <- cv.glmnet(as.matrix(scale(data$census)), variable)
  lambda <- model.cv$lambda.min
  
  par(mar = c(9, 3, 2, 0.5))
  # Plot, leave intercept out
  barplot(t(as.matrix(coef(model, s = lambda)))[, -1],
          las = 2,
          cex.names = 0.7,
          cex.axis = 0.7)
}


# Pretty plot of correlations
plot_correlations <- function() {
  # https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
  library(corrplot)
  library(weights)
  
  local_data <- data
  
  # TODO: The questions must be shorter
  colnames(local_data$turbine) <- paste0(substring(colnames(local_data$turbine), 1, 90))
  
  # Create a correlation matrix, turbine survey vs. census
  M <- with(local_data, wtd.cor(census, turbine, n))
  
  # Plot it
  corrplot(M$correlation,
           p.mat = M$p.value,
           insig = "blank",
           method = "circle",
           #tl.cex = 0.7,
           tl.cex = 0.5,
           tl.col = "black")
}
