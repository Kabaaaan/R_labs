# ===================================================================
# МОДЕЛЬ 1: Обычная линейная регрессия (lm) по регионам СКФО
# ===================================================================
lm_model_skfo <- lm(log(flat_count) ~ log(mean_salary) + log(Population) + latitude + distance_to_moscow_km,
                    data = df_SKFO)
summary(lm_model_skfo)
coef(lm_model_skfo)
vif(lm_model_skfo)
# Корреляционная матрица
df_numeric <- df_SKFO[, c("flat_count", "mean_salary", "Population", "latitude", "distance_to_moscow_km")]
cor_matrix_lm <- cor(df_numeric, use = "complete.obs")
print(round(cor_matrix_lm, 3))

# График остатков для lm
res_lm <- residuals(lm_model_skfo)
plot_data_lm <- data.frame(
  predicted = as.numeric(predict(lm_model_skfo)),
  residuals = as.numeric(res_lm)
)
ggplot(plot_data_lm, aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.9, size = 1.9, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  labs(
    title = "Остатки модели LM (СКФО)",
    subtitle = paste("Среднее остатков:", round(mean(res_lm), 6)),
    x = "Предсказанные значения log(flat_count)",
    y = "Остатки"
  ) +
  theme_minimal()
# ===================================================================
# МОДЕЛЬ 2: FE модель (plm) по регионам СКФО (within)
# ===================================================================
model2_fe_skfo <- plm(log(flat_count) ~ log(mean_salary) + log(Population) + latitude + distance_to_moscow_km,
                      data = df_SKFO,
                      index = c("Регион", "Год"),
                      model = "within")
summary(model2_fe_skfo)
coef(model2_fe_skfo)

# Анализ фиксированных эффектов для FE модели
fixef_df <- data.frame(
  Регион = names(fixef(model2_fe_skfo)),
  fixef_log = as.numeric(fixef(model2_fe_skfo)),
  fixef_real = exp(as.numeric(fixef(model2_fe_skfo)))
)
fixef_df <- fixef_df %>%
  arrange(desc(fixef_real)) %>%
  mutate(Регион = factor(Регион, levels = Регион))
cat("\n=== БАЗОВЫЕ УРОВНИ РЕГИОНОВ СКФО (exp(fixef)) ===\n")
print(fixef_df %>%
        mutate(`Множитель` = round(fixef_real, 0),
               `log-уровень` = round(fixef_log, 3)) %>%
        select(Регион, `Множитель`, `log-уровень`),
      row.names = FALSE)



# График фиксированных эффектов
ggplot(fixef_df, aes(x = Регион, y = fixef_real, fill = fixef_real)) +
  geom_col(width = 0.75, color = "white", alpha = 0.9) +
  geom_text(aes(label = round(fixef_real, 0)), 
            vjust = -0.4, size = 4.5, fontface = "bold") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Множитель") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Базовый уровень строительства по СКФО",
    subtitle = "(после контроля зарплаты, населения, широты и расстояния до Москвы)",
    x = NULL,
    y = "Относительный множитель (квартир «по умолчанию»)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    legend.position = "none",
    plot.subtitle = element_text(size = 11, color = "gray30")
  ) +
  coord_cartesian(ylim = c(0, max(fixef_df$fixef_real) * 1.15))

# =========================================================
# СРАВНЕНИЕ LM vs PLM (СКФО)
# Санкт-Петербург, 2010
# =========================================================


region_test = "Республика Дагестан"

# Сравнение MAPE для конкретного региона в СКФО
years <- 2000:2012
lm_errors <- numeric(length(years))
plm_errors <- numeric(length(years))
lm_mape <- numeric(length(years))
plm_mape <- numeric(length(years))
results_list <- list()
for(i in seq_along(years)) {
  year <- years[i]
  
  test_obs <- df_SKFO %>%
    filter(Регион == region_test, Год == year)
  
  if(nrow(test_obs) == 0) {
    lm_errors[i] <- NA
    plm_errors[i] <- NA
    lm_mape[i] <- NA
    plm_mape[i] <- NA
    next
  }
  
  pred_lm_log <- predict(lm_model_skfo, newdata = test_obs)
  pred_lm <- exp(pred_lm_log)
  
  b <- coef(model2_fe_skfo)
  fe_region <- fixef(model2_fe_skfo)[as.character(test_obs$Регион)]
  pred_plm_log <- b["log(mean_salary)"] * log(test_obs$mean_salary) +
    b["log(Population)"] * log(test_obs$Population) +
    fe_region
  pred_plm <- exp(pred_plm_log)
  
  actual <- test_obs$flat_count
  
  lm_error <- pred_lm - actual
  plm_error <- pred_plm - actual
  lm_mape_val <- mean(abs(lm_error / actual)) * 100
  plm_mape_val <- mean(abs(plm_error / actual)) * 100
  
  lm_errors[i] <- lm_error
  plm_errors[i] <- plm_error
  lm_mape[i] <- lm_mape_val
  plm_mape[i] <- plm_mape_val
  
  results_list[[as.character(year)]] <- data.frame(
    Регион = test_obs$Регион,
    Год = year,
    Факт = actual,
    LM_прогноз = pred_lm,
    PLM_прогноз = pred_plm,
    Ошибка_LM = lm_error,
    Ошибка_PLM = plm_error,
    MAPE_LM = lm_mape_val,
    MAPE_PLM = plm_mape_val
  )
}
all_results <- do.call(rbind, results_list)
avg_lm_mape <- mean(lm_mape, na.rm = TRUE)
avg_plm_mape <- mean(plm_mape, na.rm = TRUE)
mape_comparison <- data.frame(
  Год = years,
  LM_MAPE = lm_mape,
  PLM_MAPE = plm_mape
)
ggplot(mape_comparison, aes(x = Год)) +
  geom_line(aes(y = LM_MAPE, color = "LM"), linewidth = 1) +
  geom_line(aes(y = PLM_MAPE, color = "PLM"), linewidth = 1) +
  geom_point(aes(y = LM_MAPE, color = "LM")) +
  geom_point(aes(y = PLM_MAPE, color = "PLM")) +
  labs(title = "Сравнение MAPE: LM vs PLM модели (респ. Карелия)",
       y = "MAPE (%)",
       x = "Год",
       color = "Модель") +
  theme_minimal() +
  scale_color_manual(values = c("LM" = "blue", "PLM" = "red")) +
  scale_x_continuous(
    breaks = seq(min(df_norm$Год), max(df_norm$Год), by = 1)
  )
print(data.frame(
  Год = years,
  LM_MAPE = round(lm_mape, 2),
  PLM_MAPE = round(plm_mape, 2)
))
mean(lm_mape)
mean(plm_mape)



# Графики сравнение MAPE моделей для регионов СКФО + MAPE
calculate_mape_for_skfo_region <- function(region_name) {
  lm_mape_region <- numeric(length(years))
  plm_mape_region <- numeric(length(years))
  
  for(i in seq_along(years)) {
    year <- years[i]
    
    test_obs <- df_SKFO %>%
      filter(Регион == region_name, Год == year)
    
    if(nrow(test_obs) == 0) {
      lm_mape_region[i] <- NA
      plm_mape_region[i] <- NA
      next
    }
    
    pred_lm_log <- predict(lm_model_skfo, newdata = test_obs)
    pred_lm <- exp(pred_lm_log)
    
    b <- coef(model2_fe_skfo)
    fe_region <- fixef(model2_fe_skfo)[as.character(test_obs$Регион)]
    pred_plm_log <- b["log(mean_salary)"] * log(test_obs$mean_salary) +
      b["log(Population)"] * log(test_obs$Population) +
      fe_region
    pred_plm <- exp(pred_plm_log)
    
    actual <- test_obs$flat_count
    
    lm_mape_region[i] <- mean(abs((pred_lm - actual) / actual)) * 100
    plm_mape_region[i] <- mean(abs((pred_plm - actual) / actual)) * 100
  }
  
  return(data.frame(
    Год = years,
    Регион = region_name,
    LM_MAPE = lm_mape_region,
    PLM_MAPE = plm_mape_region
  ))
}
skfo_regions <- unique(df_SKFO$Регион)
all_skfo_regions_data <- do.call(rbind, lapply(skfo_regions, calculate_mape_for_skfo_region))
all_skfo_regions_long <- all_skfo_regions_data %>%
  tidyr::pivot_longer(
    cols = c(LM_MAPE, PLM_MAPE),
    names_to = "Модель",
    values_to = "MAPE"
  ) %>%
  mutate(Модель = ifelse(Модель == "LM_MAPE", "LM", "PLM"))
ggplot(all_skfo_regions_long, aes(x = Год, y = MAPE, color = Модель)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Регион, scales = "free_y", ncol = 2) +
  labs(title = "Сравнение MAPE: LM vs PLM по регионам СКФО (2000-2012)",
       y = "MAPE (%)",
       x = "Год") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(size = 8),
    axis.text.y = element_blank(),
  ) +
  scale_color_manual(values = c("LM" = "blue", "PLM" = "red")) +
  scale_x_continuous(breaks = seq(2000, 2012, by = 2))
overall_mape <- all_skfo_regions_data %>%
  summarise(
    LM_MAPE_средний = mean(LM_MAPE, na.rm = TRUE),
    PLM_MAPE_средний = mean(PLM_MAPE, na.rm = TRUE)
  )
cat("\nLM модель → средний MAPE =", round(overall_mape$LM_MAPE_средний, 2), "%\n")
cat("PLM модель → средний MAPE =", round(overall_mape$PLM_MAPE_средний, 2), "%\n")