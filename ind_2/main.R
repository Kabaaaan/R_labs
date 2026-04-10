library(tidyverse)
library(car)      
library(lmtest)    
library(sandwich)   
library(ggplot2)   
library(corrplot)
library(plm) 
library(dplyr)
library(scales)

data <- read.csv("result.csv")
data <- data[!is.na(data$Population), ]

district_names <- c(
  "Центральный федер...",
  "Северо-Западный ф...",
  "Южный федеральный...",
  "Северо-Кавказский...",
  "Приволжский федер...",
  "Уральский федерал...",
  "Сибирский федерал...",
  "Дальневосточный ф..."
)


df_all_districts <- data[data$Регион %in% district_names, ]
data_regions <- data[!(data$Регион %in% district_names), ]
data_regions_no_district <- data_regions[, !(names(data_regions) %in% c("district"))]

district_list <- split(data_regions_no_district, data_regions$district)

df_CFO  <- district_list[["ЦФО"]]
df_SZFO <- district_list[["СЗФО"]]
df_YUFO <- district_list[["ЮФО"]]
df_SKFO <- district_list[["СКФО"]]
df_PFO  <- district_list[["ПФО"]]
df_UFO  <- district_list[["УрФО"]]
df_SFO  <- district_list[["СФО"]]
df_DVFO <- district_list[["ДВФО"]]



# ===================================================================
# ПОЛЕЗНЫЕ ВИЗУАЛИЗАЦИИ
# ===================================================================



df_year <- data_regions %>%
  group_by(Год) %>%
  summarise(
    mean_salary = mean(mean_salary, na.rm = TRUE),
    flat_count = sum(flat_count, na.rm = TRUE),
    Population = sum(Population, na.rm = TRUE)
  )

# нормализация (база = первый год)
df_norm <- df_year %>%
  mutate(
    salary_index = mean_salary / first(mean_salary) * 100,
    flats_index = flat_count / first(flat_count) * 100,
    pop_index = Population / first(Population) * 100
  )

ggplot(df_norm, aes(x = Год)) +
  geom_line(aes(y = salary_index, color = "Доходы"), size = 1.2) +
  geom_point(aes(y = salary_index, color = "Доходы")) +
  
  geom_line(aes(y = flats_index, color = "Квартиры"), size = 1.2) +
  geom_point(aes(y = flats_index, color = "Квартиры")) +
  
  geom_line(aes(y = pop_index, color = "Население"), size = 1.2, linetype = "dashed") +
  geom_point(aes(y = pop_index, color = "Население")) +
  
  scale_x_continuous(
    breaks = seq(min(df_norm$Год), max(df_norm$Год), by = 1)
  ) +
  
  labs(title = "Динамика основных переменных",
       x = "Год",
       y = "Индекс",
       color = "Показатель") +
  theme_minimal()


ggplot(data_regions, aes(x = latitude, y = distance_to_moscow_km)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "red", 
              se = FALSE) +  
  labs(title = "Широта и расстояние до Москвы",
       x = "Широта",
       y = "Расстояние до Москвы (км)") +
  theme_minimal()


ggplot(data_regions, aes(x = Population, y = flat_count)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "darkgreen") +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Строительство и население",
       x = "Население (лог)",
       y = "Количество квартир (лог)") +
  theme_minimal()


ggplot(data_regions, aes(x = mean_salary, y = Population)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Доходы и население",
       x = "Доход",
       y = "Население") +
  theme_minimal()



data_regions$flats_per_capita <- data_regions$flat_count / data_regions$Population

ggplot(data_regions, aes(x = distance_to_moscow_km, y = flat_count)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  scale_y_log10() +
  labs(title = "Строительство vs расстояние до Москвы",
       x = "Расстояние до Москвы",
       y = "Квартиры") +
  theme_minimal()

ggplot(data_regions, aes(x = latitude, y = mean_salary)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Доход vs широта",
       x = "Широта",
       y = "Доход") +
  theme_minimal()

# ===================================================================
# МОДЕЛЬ 0: Обычная линейная регрессия по всем регионам
# ===================================================================

test_model <- lm(log(flat_count) ~ log(mean_salary) + log(Population) + latitude + distance_to_moscow_km + factor(district), 
                 data = data_regions)
summary(test_model)
coef(test_model)


# ===================================================================
# МОДЕЛЬ 1: Обычная линейная регрессия (lm) по ФО
# ===================================================================

lm_model_district <- lm(log(flat_count) ~ log(mean_salary) + log(Population) + latitude + distance_to_moscow_km, 
                        data = df_all_districts)
summary(lm_model_district)
coef(lm_model_district)
vif(lm_model_district)

# Корреляционная матрица
df_numeric <- df_all_districts[, c("flat_count", "mean_salary", "Population", "latitude", "distance_to_moscow_km")]
cor_matrix_lm <- cor(df_numeric, use = "complete.obs")
print(round(cor_matrix_lm, 3))

# Стандартизированные коэффициенты для lm
df_scaled_lm <- df_all_districts %>%
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
df_all_districts$predicted_lm <- exp(predict(lm_model_district))
ggplot(df_all_districts, aes(x = predicted_lm, y = flat_count)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Модель LM",
       subtitle = "Предсказанные vs фактические значения (в уровнях)",
       x = "Предсказанные flat_count",
       y = "Фактические flat_count") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  annotate("text", 
           x = min(df_all_districts$predicted_lm, na.rm = TRUE) * 1.05,
           y = max(df_all_districts$flat_count, na.rm = TRUE) * 0.95,
           label = paste("R² =", round(summary(lm_model_district)$r.squared, 3)), 
           hjust = 0, size = 5)

# График: предсказанные vs фактические значения (в логарифмах)
df_all_districts$predicted_log_lm <- predict(lm_model_district)
ggplot(df_all_districts, aes(x = predicted_log_lm, y = log(flat_count))) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Модель LM",
       subtitle = "Предсказанные vs фактические значения (в логарифмах)",
       x = "Предсказанные log(flat_count)",
       y = "Фактические log(flat_count)") +
  theme_minimal() +
  annotate("text", 
           x = min(df_all_districts$predicted_log_lm, na.rm = TRUE) * 1.05,
           y = max(log(df_all_districts$flat_count), na.rm = TRUE) * 0.95,
           label = paste("R² =", round(summary(lm_model_district)$r.squared, 3)), 
           hjust = 0, size = 5)

# График остатков для lm
res_lm <- residuals(lm_model_district)
plot_data_lm <- data.frame(
  predicted = as.numeric(predict(lm_model_district)),
  residuals = as.numeric(res_lm)
)

ggplot(plot_data_lm, aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.9, size = 1.9, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  labs(
    title = "Остатки модели LM",
    subtitle = paste("Среднее остатков:", round(mean(res_lm), 6)),
    x = "Предсказанные значения log(flat_count)",
    y = "Остатки"
  ) +
  theme_minimal()



coefs <- coef(lm_model_district)

intercept     <- round(coefs["(Intercept)"], 4)
b_salary      <- round(coefs["log(mean_salary)"], 4)
b_population  <- round(coefs["log(Population)"], 4)
b_latitude    <- round(coefs["latitude"], 4)
b_distance    <- round(coefs["distance_to_moscow_km"], 4)

equation <- paste0(
  "log(flat_count) = ", intercept,
  " + ", b_salary, " × log(mean_salary)",
  " + ", b_population, " × log(Population)",
  " + ", b_latitude, " × latitude",
  " + ", b_distance, " × distance_to_moscow_km"
)

mean_salary   <- mean(df_all_districts$mean_salary, na.rm = TRUE)
mean_pop      <- mean(df_all_districts$Population, na.rm = TRUE)
mean_lat      <- mean(df_all_districts$latitude, na.rm = TRUE)
mean_dist     <- mean(df_all_districts$distance_to_moscow_km, na.rm = TRUE)

# Расчёт эластичностей
elast_salary     <- b_salary                                      # log-log → прямая эластичность
elast_population <- b_population                                  # log-log → прямая эластичность
elast_latitude   <- round(b_latitude * mean_lat, 4)               # log-lin → β × mean(X)
elast_distance   <- round(b_distance * mean_dist, 4)              # log-lin → β × mean(X)

cat("1. Эластичность flat_count по mean_salary:\n")
cat("   ", elast_salary, "\n")
cat("2. Эластичность flat_count по Population:\n")
cat("   ", elast_population, "\n")
cat("3. Эластичность flat_count по latitude:\n")
cat("   ", elast_latitude, "\n")
cat("4. Эластичность flat_count по distance_to_moscow_km:\n")
cat("   ", elast_distance, "\n")



mean_salary_avg <- mean(log(df_all_districts$mean_salary), na.rm = TRUE)
mean_pop_avg    <- mean(log(df_all_districts$Population), na.rm = TRUE)
lat_avg         <- mean(df_all_districts$latitude, na.rm = TRUE)
dist_avg        <- mean(df_all_districts$distance_to_moscow_km, na.rm = TRUE)



# По зарплате
const_salary <- coefs[1] +
  coefs["log(Population)"] * mean_pop_avg +
  coefs["latitude"] * lat_avg +
  coefs["distance_to_moscow_km"] * dist_avg

eq_salary <- paste0(
  "1) ln(flat_count) = ",
  round(const_salary, 3),
  " + ",
  round(coefs["log(mean_salary)"], 3),
  " * ln(mean_salary)"
)

# По населению
const_pop <- coefs[1] +
  coefs["log(mean_salary)"] * mean_salary_avg +
  coefs["latitude"] * lat_avg +
  coefs["distance_to_moscow_km"] * dist_avg

eq_pop <- paste0(
  "2) ln(flat_count) = ",
  round(const_pop, 3),
  " + ",
  round(coefs["log(Population)"], 3),
  " * ln(Population)"
)

# По широте
const_lat <- coefs[1] +
  coefs["log(mean_salary)"] * mean_salary_avg +
  coefs["log(Population)"] * mean_pop_avg +
  coefs["distance_to_moscow_km"] * dist_avg

eq_lat <- paste0(
  "3) ln(flat_count) = ",
  round(const_lat, 3),
  " + ",
  round(coefs["latitude"], 3),
  " * latitude"
)

# По расстоянию
const_dist <- coefs[1] +
  coefs["log(mean_salary)"] * mean_salary_avg +
  coefs["log(Population)"] * mean_pop_avg +
  coefs["latitude"] * lat_avg

eq_dist <- paste0(
  "4) ln(flat_count) = ",
  round(const_dist, 3),
  " + ",
  round(coefs["distance_to_moscow_km"], 3),
  " * distance_to_moscow_km"
)


cat(eq_salary, "\n")
cat(eq_pop, "\n")
cat(eq_lat, "\n")
cat(eq_dist, "\n")




pred <- predict(lm_model_district, interval = "confidence")

df_all_districts$fit <- exp(pred[, "fit"])
df_all_districts$lwr <- exp(pred[, "lwr"])
df_all_districts$upr <- exp(pred[, "upr"])


ggplot(df_all_districts, aes(x = fit, y = flat_count)) +
  geom_point(alpha = 0.6, color = "blue") +
  
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.2, fill = "blue") +
  
  labs(title = "Модель LM с доверительным интервалом",
       subtitle = "Предсказанные vs фактические значения",
       x = "Предсказанные flat_count",
       y = "Фактические flat_count") +
  
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  
  theme_minimal() +
  
  annotate("text", 
           x = min(df_all_districts$fit, na.rm = TRUE) * 1.05,
           y = max(df_all_districts$flat_count, na.rm = TRUE) * 0.95,
           label = paste("R² =", round(summary(lm_model_district)$r.squared, 3)), 
           hjust = 0, size = 5)




# ===================================================================
# МОДЕЛЬ 2: FE модель (plm) по округам (within)
# ===================================================================

model2_fe <- plm(log(flat_count) ~ log(mean_salary) + log(Population) + latitude + distance_to_moscow_km,
                 data = df_all_districts,
                 index = c("Регион", "Год"),   # здесь "Регион" = название ФО
                 model = "within")

summary(model2_fe)
coef(model2_fe)

# Стандартизированные коэффициенты для FE модели
df_scaled_fe <- df_all_districts %>%
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
df_all_districts$predicted2_fe <- exp(predict(model2_fe))
ggplot(df_all_districts, aes(x = predicted2_fe, y = flat_count)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Модель FE: фиксированные эффекты по ФО",
       subtitle = "Предсказанные vs фактические значения (в уровнях)",
       x = "Предсказанные flat_count",
       y = "Фактические flat_count") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  annotate("text", 
           x = min(df_all_districts$predicted2_fe, na.rm = TRUE) * 1.05,
           y = max(df_all_districts$flat_count, na.rm = TRUE) * 0.95,
           label = "R² (within) = 0.815", 
           hjust = 0, size = 5)

# График: предсказанные vs фактические значения (в логарифмах) для FE модели
df_all_districts$predicted_log_fe <- predict(model2_fe)
ggplot(df_all_districts, aes(x = predicted_log_fe, y = log(flat_count))) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Модель FE: фиксированные эффекты по ФО",
       subtitle = "Предсказанные vs фактические значения (в логарифмах)",
       x = "Предсказанные log(flat_count)",
       y = "Фактические log(flat_count)") +
  theme_minimal() +
  annotate("text", 
           x = min(df_all_districts$predicted_log_fe, na.rm = TRUE) * 1.05,
           y = max(log(df_all_districts$flat_count), na.rm = TRUE) * 0.95,
           label = "R² (within) = 0.815", 
           hjust = 0, size = 5)

# График остатков для FE модели
res_fe <- residuals(model2_fe)
plot_data_fe <- data.frame(
  predicted = as.numeric(predict(model2_fe)),
  residuals = as.numeric(res_fe)
)

ggplot(plot_data_fe, aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.9, size = 1.9, color = "darkgreen") +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  labs(
    title = "Остатки модели FE",
    subtitle = paste("Среднее остатков:", round(mean(res_fe), 6)),
    x = "Предсказанные значения log(flat_count)",
    y = "Остатки"
  ) +
  theme_minimal()

# Анализ фиксированных эффектов для FE модели
fixef_df <- data.frame(
  Округ = names(fixef(model2_fe)),
  fixef_log = as.numeric(fixef(model2_fe)),
  fixef_real = exp(as.numeric(fixef(model2_fe)))  
)

fixef_df <- fixef_df %>% 
  arrange(desc(fixef_real)) %>%
  mutate(Округ = factor(Округ, levels = Округ))   

cat("\n=== БАЗОВЫЕ УРОВНИ ОКРУГОВ (exp(fixef)) ===\n")
print(fixef_df %>% 
        mutate(`Множитель` = round(fixef_real, 0),
               `log-уровень` = round(fixef_log, 3)) %>%
        select(Округ, `Множитель`, `log-уровень`), 
      row.names = FALSE)

# График фиксированных эффектов
ggplot(fixef_df, aes(x = Округ, y = fixef_real, fill = fixef_real)) +
  geom_col(width = 0.75, color = "white", alpha = 0.9) +
  geom_text(aes(label = round(fixef_real, 0)), 
            vjust = -0.4, size = 4.5, fontface = "bold") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Множитель") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Базовый уровень строительства по ФО",
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
# СРАВНЕНИЕ LM vs PLM (Окружная)
# СЗФО, 2010
# =========================================================

test_obs <- df_all_districts %>%
  filter(Регион == "Северо-Западный ф...", Год == 2010)

pred_lm_log <- predict(lm_model_district, newdata = test_obs)
pred_lm <- exp(pred_lm_log)

b <- coef(model2_fe)
fe_region <- fixef(model2_fe)[as.character(test_obs$Регион)]
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
  labs(title = "Сравнение LM и PLM (СЗФО, 2010)",
       y = "Количество квартир",
       x = "") +
  theme_minimal() +
  theme(legend.position = "none")



# Сравнение MAPE для конкретного округа
years <- 2000:2012
lm_errors <- numeric(length(years))
plm_errors <- numeric(length(years))
lm_mape <- numeric(length(years))
plm_mape <- numeric(length(years))

results_list <- list()

for(i in seq_along(years)) {
  year <- years[i]
  
  test_obs <- df_all_districts %>%
    filter(Регион == "Дальневосточный ф...", Год == year)
  
  if(nrow(test_obs) == 0) {
    lm_errors[i] <- NA
    plm_errors[i] <- NA
    lm_mape[i] <- NA
    plm_mape[i] <- NA
    next
  }
  
  pred_lm_log <- predict(lm_model_district, newdata = test_obs)
  pred_lm <- exp(pred_lm_log)
  
  b <- coef(model2_fe)
  fe_region <- fixef(model2_fe)[as.character(test_obs$Регион)]
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
  labs(title = "Сравнение MAPE: LM vs PLM модели",
       y = "MAPE (%)",
       x = "Год",
       color = "Модель") +
  theme_minimal() +
  scale_color_manual(values = c("LM" = "blue", "PLM" = "red")) 

cat("\n\nMAPE (%):\n")
print(data.frame(
  Год = years,
  LM_MAPE = round(lm_mape, 2),
  PLM_MAPE = round(plm_mape, 2)
))


# Графики сравнение MAPE общих окружных моделей
calculate_mape_for_region <- function(region_name) {
  lm_mape_region <- numeric(length(years))
  plm_mape_region <- numeric(length(years))
  
  for(i in seq_along(years)) {
    year <- years[i]
    
    test_obs <- df_all_districts %>%
      filter(Регион == region_name, Год == year)
    
    if(nrow(test_obs) == 0) {
      lm_mape_region[i] <- NA
      plm_mape_region[i] <- NA
      next
    }
    
    pred_lm_log <- predict(lm_model_district, newdata = test_obs)
    pred_lm <- exp(pred_lm_log)
    
    b <- coef(model2_fe)
    fe_region <- fixef(model2_fe)[as.character(test_obs$Регион)]
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

all_regions_data <- do.call(rbind, lapply(district_names, calculate_mape_for_region))

all_regions_long <- all_regions_data %>%
  tidyr::pivot_longer(
    cols = c(LM_MAPE, PLM_MAPE),
    names_to = "Модель",
    values_to = "MAPE"
  ) %>%
  mutate(Модель = ifelse(Модель == "LM_MAPE", "LM", "PLM"))

ggplot(all_regions_long, aes(x = Год, y = MAPE, color = Модель)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Регион, scales = "free_y", ncol = 2) +
  labs(title = "Сравнение MAPE: LM vs PLM по регионам (2000-2012)",
       y = "MAPE (%)",
       x = "Год") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(size = 8)
  ) +
  scale_color_manual(values = c("LM" = "blue", "PLM" = "red")) +
  scale_x_continuous(breaks = seq(2000, 2012, by = 2))


