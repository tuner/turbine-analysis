library(readr)

combined_data <- read_delim("~/Documents/Koulu/ids/mini/combined_data.csv", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

turbines <- combined_data[, 1:18]
census <- combined_data[, 19:38]

#model <- lm(combined_data$`Construction of wind turbines would have a positive impact on Helsinki's image.` ~ combined_data[, 19:38])
model <- lm(turbines$`Construction of wind turbines would have a positive impact on Helsinki's image.`
            ~ ., data = census[, 14:20])

summary(model)


model_and_plot <- function(x, y) {
  model <- lm(x ~ y)
  plot(y, x)
  abline(model, col = "green")
  
  text(x = y,
       y = x,
       labels = combined_data$`District name`,
       col = "#404040",
       cex = 0.5,
       adj = c(-0.1, 0.5))
  
  print(summary(model))
  print(paste("Correlation:", cor(y, x)))
}

model_and_plot(turbines$`Construction of wind turbines would have a positive impact on Helsinki's image.`, census$VIHR)
model_and_plot(turbines$`Construction of wind turbines would have a positive impact on Helsinki's image.`, census$KOK)
model_and_plot(turbines$`Construction of wind turbines would have a positive impact on Helsinki's image.`, census$VASL)

model_and_plot(turbines$`I would like to buy local wind power in Helsinki, even if cheaper electricity would be available from elsewhere`, census$`Households without cars`)

