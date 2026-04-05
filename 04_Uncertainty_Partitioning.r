library(tidyverse)

forecast <- read_csv("outputs/milestone6_like/ALL_ref_2026-04-05_20260405T212408Z_forecast_ALLSITES.csv")

uncertainty_time <- forecast %>%
  group_by(datetime) %>%
  summarize(
    mu_t = mean(prediction),
    ensemble_uncertainty = var(prediction),
    .groups = "drop"
  )

var_IC <- var(uncertainty_time$mu_t)
var_ensemble <- mean(uncertainty_time$ensemble_uncertainty)
var_total <- var_IC + var_ensemble

uncertainty_decomp <- tibble(
  component = c("Initial Conditions", "Ensemble Spread"),
  variance = c(var_IC, var_ensemble),
  proportion = c(var_IC, var_ensemble) / var_total
)

print(uncertainty_decomp)

ggplot(uncertainty_time, aes(x = datetime)) +
  geom_line(aes(y = mu_t)) +
  geom_ribbon(
    aes(
      ymin = mu_t - sqrt(var_within),
      ymax = mu_t + sqrt(var_within)
    ),
    alpha = 0.3
  ) +
  labs(
    y = "PM2.5",
    title = "Forecast with Uncertainty"
  ) +
  theme_minimal()

ggplot(uncertainty_decomp, aes(x = component, y = variance, fill = component)) +
  geom_col() +
  labs(title = "Uncertainty Partitioning") +
  theme_minimal()
