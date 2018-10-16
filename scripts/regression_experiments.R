###
# Playing with the data. Just some random hacks..
###


# Clear the workspace
rm(list=ls())

source("common.R")

data <- load_data()


explain_variable <- function(variable) {
  library(glmnet)
  model <- glmnet(as.matrix(scale(data$census)), variable)
  
  # Use cross-validation to find good lambda
  model.cv <- cv.glmnet(as.matrix(scale(data$census)), variable)
  lambda <- model.cv$lambda.min
  
  # Plot, leave intercept out
  barplot(t(as.matrix(coef(model, s = lambda)))[, -1], las = 2)
}

explain_variable(data$turbine$`Local wind power should be produced in Helsinki`)
explain_variable(data$turbine$`I would like to buy local wind power in Helsinki, even if cheaper electricity would be available from elsewhere`)
explain_variable(data$turbine$`What do you feel if wind turbines would be built at the outer territorial waters (8-10 km from the coastline) of Helsinki?`)



model_and_plot(data$census$VIHR, data$turbine$`Construction of wind turbines would have a positive impact on Helsinki's image.`)
model_and_plot(data$census$KOK, data$turbine$`Construction of wind turbines would have a positive impact on Helsinki's image.`)
model_and_plot(data$census$VASL, data$turbine$`Construction of wind turbines would have a positive impact on Helsinki's image.`)

model_and_plot(data$census$`Households without cars`, data$turbine$`I would like to buy local wind power in Helsinki, even if cheaper electricity would be available from elsewhere`)
model_and_plot(data$census$KOK, data$turbine$`What do you feel if wind turbines would be built at the outer territorial waters (8-10 km from the coastline) of Helsinki?`)
model_and_plot(data$census$VASL, data$turbine$`What do you feel if wind turbines would be built at the outer territorial waters (8-10 km from the coastline) of Helsinki?`)


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
  # TODO: Weights
  M <- cor(numeric_data[, 1:16], numeric_data[, 17:36])
  
  # Plot it
  corrplot(M,
           method = "circle",
           tl.cex = 0.6,
           tl.col = "black")
}

plot_correlations()
