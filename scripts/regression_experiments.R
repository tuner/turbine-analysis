###
# Playing with the data. Just some random hacks..
###


# Clear the workspace
rm(list=ls())

source("common.R")

data <- load_data()

explain_variable(data$turbine$`Local wind power should be produced in Helsinki`)
explain_variable(data$turbine$`I would like to buy local wind power in Helsinki, even if cheaper electricity would be available from elsewhere`)
explain_variable(data$turbine$`What do you feel if wind turbines would be built at the outer territorial waters (8-10 km from the coastline) of Helsinki?`)
explain_variable(data$turbine$`How often do you spend your free time on the shores of Helsinki?`)


model_and_plot(data$census$VIHR, data$turbine$`Construction of wind turbines would have a positive impact on Helsinki's image.`)
model_and_plot(data$census$KOK, data$turbine$`Construction of wind turbines would have a positive impact on Helsinki's image.`)
model_and_plot(data$census$VASL, data$turbine$`Construction of wind turbines would have a positive impact on Helsinki's image.`)

model_and_plot(data$census$`Alko per 1000`, data$turbine$`Construction of wind turbines would have a positive impact on Helsinki's image.`)

model_and_plot(data$census$`Households without cars`, data$turbine$`I would like to buy local wind power in Helsinki, even if cheaper electricity would be available from elsewhere`)
model_and_plot(data$census$KOK, data$turbine$`What do you feel if wind turbines would be built at the outer territorial waters (8-10 km from the coastline) of Helsinki?`)
model_and_plot(data$census$VASL, data$turbine$`What do you feel if wind turbines would be built at the outer territorial waters (8-10 km from the coastline) of Helsinki?`)
model_and_plot(data$census$`Born in Helsinki`, data$turbine$`What do you feel if wind turbines would be built at the outer territorial waters (8-10 km from the coastline) of Helsinki?`)


plot_correlations()
