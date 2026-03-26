# ===================================================================
# МОДЕЛЬ 1: Обычная линейная регрессия (lm) по регионам ЦФО
# ===================================================================
lm_model_cfo <- lm(log(flat_count) ~ log(mean_salary) + log(Population) + latitude + distance_to_moscow_km,
                   data = df_CFO)
summary(lm_model_cfo)
coef(lm_model_cfo)
vif(lm_model_cfo)

# Корреляционная матрица
df_numeric <- df_CFO[, c("flat_count", "mean_salary", "Population", "latitude", "distance_to_moscow_km")]
cor_matrix_lm <- cor(df_numeric, use = "complete.obs")
print(round(cor_matrix_lm, 3))

# Стандартизированные коэффициенты для lm
df_scaled_lm <- df_CFO %>%
  mutate(
    log_flat = scale(log(flat_count)),
    log_salary = scale(log(mean_salary)),
    log_pop = scale(log(Population)),
    latitude_s = scale(latitude),
    distance_s = scale(distance_to_moscow_km)
  )
lm_model_std <- lm(
  log_flat ~ log_salary + log_pop + latitude_s + distance_s,
  data = df_scaled_lm
)
summary(lm_model_std)
coef(lm_model_std)

# График: предсказанные vs фактические значения (в уровнях)
df_CFO$predicted_lm <- exp(predict(lm_model_cfo))
ggplot(df_CFO, aes(x = predicted_lm, y = flat_count)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Модель LM (ЦФО)",
       subtitle = "Предсказанные vs фактические значения (в уровнях)",
       x = "Предсказанные flat_count",
       y = "Фактические flat_count") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  annotate("text",
           x = min(df_CFO$predicted_lm, na.rm = TRUE) * 1.05,
           y = max(df_CFO$flat_count, na.rm = TRUE) * 0.95,
           label = paste("R² =", round(summary(lm_model_cfo)$r.squared, 3)),
           hjust = 0, size = 5)

# График: предсказанные vs фактические значения (в логарифмах)
df_CFO$predicted_log_lm <- predict(lm_model_cfo)
ggplot(df_CFO, aes(x = predicted_log_lm, y = log(flat_count))) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Модель LM (ЦФО)",
       subtitle = "Предсказанные vs фактические значения (в логарифмах)",
       x = "Предсказанные log(flat_count)",
       y = "Фактические log(flat_count)") +
  theme_minimal() +
  annotate("text",
           x = min(df_CFO$predicted_log_lm, na.rm = TRUE) * 1.05,
           y = max(log(df_CFO$flat_count), na.rm = TRUE) * 0.95,
           label = paste("R² =", round(summary(lm_model_cfo)$r.squared, 3)),
           hjust = 0, size = 5)

# График остатков для lm
res_lm <- residuals(lm_model_cfo)
plot_data_lm <- data.frame(
  predicted = as.numeric(predict(lm_model_cfo)),
  residuals = as.numeric(res_lm)
)
ggplot(plot_data_lm, aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.9, size = 1.9, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  labs(
    title = "Остатки модели LM (ЦФО)",
    subtitle = paste("Среднее остатков:", round(mean(res_lm), 6)),
    x = "Предсказанные значения log(flat_count)",
    y = "Остатки"
  ) +
  theme_minimal()

# ===================================================================
# МОДЕЛЬ 2: FE модель (plm) по регионам ЦФО (within)
# ===================================================================
model2_fe_cfo <- plm(log(flat_count) ~ log(mean_salary) + log(Population) + latitude + distance_to_moscow_km,
                     data = df_CFO,
                     index = c("Регион", "Год"),
                     model = "within")
summary(model2_fe_cfo)
coef(model2_fe_cfo)

# Стандартизированные коэффициенты для FE модели
df_scaled_fe <- df_CFO %>%
  mutate(
    log_flat = scale(log(flat_count)),
    log_salary = scale(log(mean_salary)),
    log_pop = scale(log(Population)),
    latitude_s = scale(latitude),
    distance_s = scale(distance_to_moscow_km)
  )
model2_fe_std <- plm(
  log_flat ~ log_salary + log_pop + latitude_s + distance_s,
  data = df_scaled_fe,
  index = c("Регион", "Год"),
  model = "within"
)
summary(model2_fe_std)
coef(model2_fe_std)

# График: предсказанные vs фактические значения (в уровнях) для FE модели
df_CFO$predicted2_fe <- exp(predict(model2_fe_cfo))
ggplot(df_CFO, aes(x = predicted2_fe, y = flat_count)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Модель FE: фиксированные эффекты по регионам ЦФО",
       subtitle = "Предсказанные vs фактические значения (в уровнях)",
       x = "Предсказанные flat_count",
       y = "Фактические flat_count") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  annotate("text",
           x = min(df_CFO$predicted2_fe, na.rm = TRUE) * 1.05,
           y = max(df_CFO$flat_count, na.rm = TRUE) * 0.95,
           label = paste("R² (within) =", round(summary(model2_fe_cfo)$r.squared, 3)),
           hjust = 0, size = 5)

# График: предсказанные vs фактические значения (в логарифмах) для FE модели
df_CFO$predicted_log_fe <- predict(model2_fe_cfo)
ggplot(df_CFO, aes(x = predicted_log_fe, y = log(flat_count))) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Модель FE: фиксированные эффекты по регионам ЦФО",
       subtitle = "Предсказанные vs фактические значения (в логарифмах)",
       x = "Предсказанные log(flat_count)",
       y = "Фактические log(flat_count)") +
  theme_minimal() +
  annotate("text",
           x = min(df_CFO$predicted_log_fe, na.rm = TRUE) * 1.05,
           y = max(log(df_CFO$flat_count), na.rm = TRUE) * 0.95,
           label = paste("R² (within) =", round(summary(model2_fe_cfo)$r.squared, 3)),
           hjust = 0, size = 5)

# График остатков для FE модели
res_fe <- residuals(model2_fe_cfo)
plot_data_fe <- data.frame(
  predicted = as.numeric(predict(model2_fe_cfo)),
  residuals = as.numeric(res_fe)
)
ggplot(plot_data_fe, aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.9, size = 1.9, color = "darkgreen") +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  labs(
    title = "Остатки модели FE (ЦФО)",
    subtitle = paste("Среднее остатков:", round(mean(res_fe), 6)),
    x = "Предсказанные значения log(flat_count)",
    y = "Остатки"
  ) +
  theme_minimal()

# Анализ фиксированных эффектов для FE модели
fixef_df <- data.frame(
  Регион = names(fixef(model2_fe_cfo)),
  fixef_log = as.numeric(fixef(model2_fe_cfo)),
  fixef_real = exp(as.numeric(fixef(model2_fe_cfo)))
)
fixef_df <- fixef_df %>%
  arrange(desc(fixef_real)) %>%
  mutate(Регион = factor(Регион, levels = Регион))
cat("\n=== БАЗОВЫЕ УРОВНИ РЕГИОНОВ ЦФО (exp(fixef)) ===\n")
print(fixef_df %>%
        mutate(`Множитель` = round(fixef_real, 0),
               `log-уровень` = round(fixef_log, 3)) %>%
        select(Регион, `Множитель`, `log-уровень`),
      row.names = FALSE)

# График фиксированных эффектов
ggplot(fixef_df, aes(x = Регион, y = fixef_log, fill = fixef_log)) +
  geom_col(width = 0.75, color = "white", alpha = 0.9) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Множитель") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Базовый уровень строительства по регионам ЦФО",
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
  coord_cartesian(ylim = c(0, max(fixef_df$fixef_log) * 1.15))

# =========================================================
# СРАВНЕНИЕ LM vs PLM (ЦФО)
# Липецкая область, 2004
# =========================================================
test_obs <- df_CFO %>%
  filter(Регион == "Липецкая область", Год == 2004)
pred_lm_log <- predict(lm_model_cfo, newdata = test_obs)
pred_lm <- exp(pred_lm_log)
b <- coef(model2_fe_cfo)
fe_region <- fixef(model2_fe_cfo)[as.character(test_obs$Регион)]
pred_plm_log <-
  b["log(mean_salary)"] * log(test_obs$mean_salary) +
  b["log(Population)"] * log(test_obs$Population) +
  fe_region
pred_plm <- exp(pred_plm_log)
actual <- test_obs$flat_count
comparison <- data.frame(
  Регион = test_obs$Регион,
  Год = test_obs$Год,
  Факт = actual,
  LM_прогноз = pred_lm,
  PLM_прогноз = pred_plm,
  Ошибка_LM = pred_lm - actual,
  Ошибка_PLM = pred_plm - actual
)
print(comparison)

comparison_long <- comparison %>%
  select(Факт, LM_прогноз, PLM_прогноз) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Тип", values_to = "Значение")
ggplot(comparison_long, aes(x = Тип, y = Значение, fill = Тип)) +
  geom_col(width = 0.6) +
  labs(title = "Сравнение LM и PLM (Москва, 2010)",
       y = "Количество квартир",
       x = "") +
  theme_minimal() +
  theme(legend.position = "none")

# Сравнение MAPE для конкретного региона в ЦФО 
years <- 2000:2012
lm_errors <- numeric(length(years))
plm_errors <- numeric(length(years))
lm_mape <- numeric(length(years))
plm_mape <- numeric(length(years))
results_list <- list()
for(i in seq_along(years)) {
  year <- years[i]
  
  test_obs <- df_CFO %>%
    filter(Регион == "Липецкая область", Год == year)
  
  if(nrow(test_obs) == 0) {
    lm_errors[i] <- NA
    plm_errors[i] <- NA
    lm_mape[i] <- NA
    plm_mape[i] <- NA
    next
  }
  
  pred_lm_log <- predict(lm_model_cfo, newdata = test_obs)
  pred_lm <- exp(pred_lm_log)
  
  b <- coef(model2_fe_cfo)
  fe_region <- fixef(model2_fe_cfo)[as.character(test_obs$Регион)]
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
  labs(title = "Сравнение MAPE: LM vs PLM модели (Москва, ЦФО)",
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



# Графики сравнение MAPE моделей для регионов ЦФО + MAPE
calculate_mape_for_cfo_region <- function(region_name) {
  lm_mape_region <- numeric(length(years))
  plm_mape_region <- numeric(length(years))
  
  for(i in seq_along(years)) {
    year <- years[i]
    
    test_obs <- df_CFO %>%
      filter(Регион == region_name, Год == year)
    
    if(nrow(test_obs) == 0) {
      lm_mape_region[i] <- NA
      plm_mape_region[i] <- NA
      next
    }
    
    pred_lm_log <- predict(lm_model_cfo, newdata = test_obs)
    pred_lm <- exp(pred_lm_log)
    
    b <- coef(model2_fe_cfo)
    fe_region <- fixef(model2_fe_cfo)[as.character(test_obs$Регион)]
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

cfo_regions <- unique(df_CFO$Регион)

all_cfo_regions_data <- do.call(rbind, lapply(cfo_regions, calculate_mape_for_cfo_region))

all_cfo_regions_long <- all_cfo_regions_data %>%
  tidyr::pivot_longer(
    cols = c(LM_MAPE, PLM_MAPE),
    names_to = "Модель",
    values_to = "MAPE"
  ) %>%
  mutate(Модель = ifelse(Модель == "LM_MAPE", "LM", "PLM"))

ggplot(all_cfo_regions_long, aes(x = Год, y = MAPE, color = Модель)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Регион, scales = "free_y", ncol = 2) +
  labs(title = "Сравнение MAPE: LM vs PLM по регионам ЦФО (2000-2012)",
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

overall_mape <- all_cfo_regions_data %>%
  summarise(
    LM_MAPE_средний  = mean(LM_MAPE, na.rm = TRUE),
    PLM_MAPE_средний = mean(PLM_MAPE, na.rm = TRUE)
  )
cat("\nLM  модель  → средний MAPE =", round(overall_mape$LM_MAPE_средний, 2), "%\n")
cat("PLM модель → средний MAPE =", round(overall_mape$PLM_MAPE_средний, 2), "%\n")


