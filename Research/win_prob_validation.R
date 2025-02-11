# Analyze predictions from the 5 models in validation data

devtools::load_all()

val_data <- readRDS("Research/win_prob_validation2025-02-05.rds")

val_data

names(val_data)

val_data %>%
  select(season, game_type, week, home_team, away_team,
         spread_line, result, p_normal1, sp_ols1, p_emp, p_normal2,
         sp_ols2, p_logistic, p_logistic2
  )

val_performance <- val_data %>%
  tidyr::drop_na(season, game_type, week, home_team, away_team,
                 spread_line, result, p_normal1, sp_ols1, p_emp, p_normal2,
                 sp_ols2, p_logistic, p_logistic2
  ) %>%
  mutate(
    res_ols1 = result - sp_ols1,
    res_ols2 = result - sp_ols2,
    res_spread = result - spread_line,
    p_moneyline = american_to_prob(home_moneyline)/
        (american_to_prob(home_moneyline) + american_to_prob(away_moneyline)),
    binary = as.numeric(result > 0),
    log_loss_normal1 = -binary*log(p_normal1) - (1-binary)*log(1-p_normal1),
    log_loss_normal2 = -binary*log(p_normal2) - (1-binary)*log(1-p_normal2),
    log_loss_emp = -binary*log(p_emp) - (1-binary)*log(1-p_emp),
    log_loss_logistic = -binary*log(p_logistic) - (1-binary)*log(1-p_logistic),
    log_loss_logistic2 = -binary*log(p_logistic2)-(1-binary)*log(1-p_logistic2),
    acc_ols1 = as.numeric(result*sp_ols1 > 0),
    acc_ols2 = as.numeric(result*sp_ols2 > 0),
    acc_logistic = as.numeric(result*(p_logistic - 0.5) > 0),
    acc_spread = as.numeric(result*spread_line > 0)
  )

val_performance %>%
  group_by(week) %>%
  summarise(
    n = n(),
    acc_spread = mean(acc_spread),
    acc_ols1 = mean(acc_ols1),
    acc_ols2 = mean(acc_ols2),
    acc_logistic = mean(acc_logistic),

    )

