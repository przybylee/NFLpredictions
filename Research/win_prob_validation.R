# Analyze predictions from the 5 models in validation data
# Questions:
# 1. How do the models compare in terms of RMSE, accuracy, and log loss?
# 2. How do the distributions of predictions of the models compare (box plots)?
# 3. Are there any trends in residuals that follow these factors?:
#    - rest
#    - gameday or weekday
#    - divisional or non-divisional, times teams played each other
#    - roof, surface, teemp, wind
#    - logistic vs normal residuals

devtools::load_all()
library(ggplot2)

val_data <- readRDS("Research/win_prob_validation2025-02-05.rds")

val_data

names(val_data)

val_data %>%
  select(season, game_type, week, home_team, away_team,
         spread_line, result, p_normal1, sp_ols1, p_emp, p_normal2,
         sp_ols2, p_logistic, p_logistic2
  )


# Q1 Compare performance metrics ------------------------------------------

# small fudge factor to avoid division by zero
p_fudge <- 1E-10

val_performance <- val_data %>%
  tidyr::drop_na(season, game_type, week, home_team, away_team,
                 spread_line, result, p_normal1, sp_ols1, p_emp, p_normal2,
                 sp_ols2, p_logistic, p_logistic2
  ) %>%
  mutate(
    across(
      starts_with("p_"),
      ~case_when(.x < p_fudge ~ p_fudge, .x > 1-p_fudge ~ 1 - p_fudge, TRUE ~.x)
    ),
    res_ols1 = result - sp_ols1,
    res_ols2 = result - sp_ols2,
    res_spread = result - spread_line,
    p_moneyline = american_to_prob(home_moneyline)/
        (american_to_prob(home_moneyline) + american_to_prob(away_moneyline)),
    binary = as.numeric(result > 0),
    log_loss_normal1 = -binary*log(p_normal1) - (1-binary)*log(1-p_normal1),
    log_loss_normal2 = -binary*log(p_normal2) - (1-binary)*log(1-p_normal2),
    log_loss_emp = -binary*log(p_emp) - (1-binary)*log(1-p_emp),
    log_loss_logistic1 = -binary*log(p_logistic) - (1-binary)*log(1-p_logistic),
    log_loss_logistic2 = -binary*log(p_logistic2)-(1-binary)*log(1-p_logistic2),
    log_loss_moneyline = -binary*log(p_moneyline) - (1-binary)*log(1-p_moneyline),
    acc_ols1 = as.numeric(result*sp_ols1 > 0),
    acc_ols2 = as.numeric(result*sp_ols2 > 0),
    acc_logistic = as.numeric(result*(p_logistic - 0.5) > 0),
    acc_spread = as.numeric(result*spread_line > 0),
    acc_moneyline = as.numeric(result*(p_moneyline - 0.5)> 0)
  )

weekly <- val_performance %>%
  group_by(week) %>%
  summarise(
    n = n(),
    n_moneyline = sum(!is.na(p_moneyline)),
    n_missing = sum(is.na(result)),
    across(starts_with("res_"), ~sqrt(mean(.x^2))),
    across(starts_with("log_loss_"), ~mean(.x, na.rm = TRUE)),
    across(starts_with("acc_"), ~mean(.x)),
    .groups = "drop"
    ) %>%
  rename_with(~stringr::str_replace(., "res_", "rmse_"), starts_with("res_"))

game_type <- val_performance %>%
  group_by(game_type) %>%
  summarise(
    n = n(),
    n_moneyline = sum(!is.na(p_moneyline)),
    n_missing = sum(is.na(result)),
    across(starts_with("res_"), ~sqrt(mean(.x^2))),
    across(starts_with("log_loss_"), ~mean(.x, na.rm = TRUE)),
    across(starts_with("acc_"), ~mean(.x)),
    .groups = "drop"
  ) %>%
  rename_with(~stringr::str_replace(., "res_", "rmse_"), starts_with("res_")) %>%
  arrange(-n)

# plot weekly rmse
weekly %>%
  tidyr::pivot_longer(starts_with("rmse_"),
                      names_to = "model",
                      values_to = "rmse"
  ) %>%
  ggplot(aes(x = week, y = rmse, color = model)) +
    geom_point() +
    geom_smooth() +
    labs(x = "Week", y = "RMSE", title = "RMSE by Model")

# plot weekly accuracy
weekly %>%
  tidyr::pivot_longer(starts_with("acc_"),
                      names_to = "model",
                      values_to = "acc"
  ) %>%
  ggplot(aes(x = week, y = acc, color = model)) +
    geom_point() +
    geom_smooth() +
    labs(x = "Week", y = "Accuracy", title = "Accuracy by Model")

# plot weekly log loss by model
weekly %>%
  tidyr::pivot_longer(starts_with("log_loss_"),
                      names_to = "model",
                      values_to = "log_loss"
  ) %>%
  ggplot(aes(x = week, y = log_loss, color = model)) +
    geom_point() +
    geom_smooth(method = "loess", mapping = aes(weight = n)) +
    labs(x = "Model", y = "Log Loss", title = "Log Loss by Model")

# compare distributions of the predictions
val_data %>%
  tidyr::pivot_longer(
    cols = c(starts_with("sp_"), spread_line, result),
    names_to = "model",
    values_to = "prediction"
  ) %>%
  ggplot(aes(x = prediction, y = model, fill = model)) +
    geom_violin(alpha = 0.5, draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlim(-50, 50) +
    labs(x = "Point Differential", y = "Model", title = "Spread Distributions")
