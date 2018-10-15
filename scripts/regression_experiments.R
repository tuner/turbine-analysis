###
# Playing with the data. Just some random hacks..
###


# Clear the workspace
rm(list=ls())

library(readr)


combined_data <- read_delim("../derived_data/combined_data.csv", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

# Subset the data by columns
turbines <- combined_data[, 1:18]
census <- combined_data[, 19:38]


#model <- lm(turbines$`Construction of wind turbines would have a positive impact on Helsinki's image.`
#            ~ ., data = census[, c(1, 2, 9, 10)])

#summary(model)


explain_variable <- function(variable) {
  model <- glmnet(as.matrix(census), variable)
  
  # Use cross-validation to find good lambda
  model.cv <- cv.glmnet(as.matrix(census), variable)
  lambda <- cvmodel$lambda.min
  
  # Plot, leave intercept out
  barplot(t(as.matrix(coef(model, s = lambda)))[, -1], las = 2)
}

explain_variable(turbines$`Local wind power should be produced in Helsinki`)


# Simple linear regression
model_and_plot <- function(x, y) {
  # Read more at: http://r-statistics.co/Linear-Regression.html
  model <- lm(y ~ x)
  plot(x, y)
  abline(model, col = "green")
  
  text(x, y,
       labels = combined_data$`District name`,
       col = "#404040",
       cex = 0.5,
       adj = c(-0.1, 0.5))
  
  print(summary(model))
  print(paste("Correlation:", cor(y, x)))
}

model_and_plot(census$VIHR, turbines$`Construction of wind turbines would have a positive impact on Helsinki's image.`)
model_and_plot(census$KOK, turbines$`Construction of wind turbines would have a positive impact on Helsinki's image.`)
model_and_plot(census$VASL, turbines$`Construction of wind turbines would have a positive impact on Helsinki's image.`)

model_and_plot(census$`Households without cars`, turbines$`I would like to buy local wind power in Helsinki, even if cheaper electricity would be available from elsewhere`)
model_and_plot(census$KOK, turbines$`What do you feel if wind turbines would be built at the outer territorial waters (8-10 km from the coastline) of Helsinki?`)


# Pretty plot of correlations
plot_correlations <- function() {
  # https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
  library(corrplot)
  
  # Remove non-numeric columns
  numeric_data <- combined_data[, -c(1, 18)]
  # TODO: The questions must be shorter
  colnames(numeric_data) <- paste0(substring(colnames(numeric_data), 1, 100))
  
  # Create a correlation matrix, turbine survey vs. census
  # TODO: Index based splitting breaks if we add/remove variables
  M <- cor(numeric_data[, 1:16], numeric_data[, 17:36])
  
  # Plot it
  corrplot(M,
           method = "circle",
           tl.cex = 0.6,
           tl.col = "black")
}

plot_correlations()
