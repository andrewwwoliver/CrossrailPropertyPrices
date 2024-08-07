library(stargazer)

# Data frame for VIF values
vif_data <- data.frame(
  Predictor = c("Close to Crossrail", "Post-Treatment", "(Log) Underground Distance", "(Log) Population Density",
                "(Log) Distance to CAZ", "Semi-Detached", "Detached", "Flat",
                "Bedrooms", "Qualification Level 4"),
  GVIF = c(1.047715, 1.047715, 1.896870, 1.326660, 1.874719, 1.327243, 1.239580, 
           2.151198, 2.183821, 1.597268),
  Df = c(3, 3, 1, 1, 1, 1, 1, 1, 1, 1),
  GVIF_1_2Df = c(1.007799, 1.007799, 1.377269, 1.151807, 1.369204, 1.152060, 1.113364, 
                 1.466696, 1.477776, 1.263831)
)

# Using stargazer to create the table
stargazer(vif_data, type = "html", summary = FALSE, rownames = FALSE,
          title = "Variance Inflation Factors (VIF) for Predictors",
          column.labels = c("Predictor", "GVIF", "Df", "GVIF^(1/(2*Df))"),
          digits = 6,
          out="vif_table.htm")


# Create Q-Q plots for the four models in a 2x2 layout
par(mfrow = c(2, 2))  # Set up a 2x2 plotting layout

# Q-Q plot for non_detached_model
qqnorm(non_detached_model$residuals, main = "Q-Q Plot for Full Model")
qqline(non_detached_model$residuals, col = "red")

# Q-Q plot for western_non_detached_model
qqnorm(western_non_detached_model$residuals, main = "Q-Q Plot for Western London")
qqline(western_non_detached_model$residuals, col = "red")

# Q-Q plot for eastern_non_detached_model
qqnorm(eastern_non_detached_model$residuals, main = "Q-Q Plot for Eastern London")
qqline(eastern_non_detached_model$residuals, col = "red")

# Q-Q plot for central_non_detached_model
qqnorm(central_non_detached_model$residuals, main = "Q-Q Plot for Central London")
qqline(central_non_detached_model$residuals, col = "red")

# Reset to default layout
par(mfrow = c(1, 1))

