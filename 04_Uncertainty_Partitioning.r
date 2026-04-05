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
      ymin = mu_t - sqrt(ensemble_uncertainty),
      ymax = mu_t + sqrt(ensemble_uncertainty)
    ),
    alpha = 0.3
  ) +
  labs(
    y = "PM2.5",
    title = "Forecast with Uncertainty"
  ) +
  theme_minimal()

V_rel <- uncertainty_time %>%
  mutate(var_total = var_IC + ensemble_uncertainty) %>%
  mutate(
    prop_IC = var_IC / var_total,
    prop_ensemble = ensemble_uncertainty / var_total
  )

ggplot(V_rel, aes(x = datetime)) +
  geom_area(aes(y = prop_IC + prop_ensemble), fill = "steelblue", alpha = 0.5) +
  geom_area(aes(y = prop_IC), fill = "tomato", alpha = 0.7) +
  labs(title = "Relative Variance Partitioning Over Time",
       y = "Proportion of Variance", x = "Datetime") +
  theme_minimal()