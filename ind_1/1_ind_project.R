install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("forecast")
install.packages("plm")
install.packages("modelsummary")
install.packages("rstudioapi")

library(readxl)
library(dplyr)
library(tidyr)
library(forecast)
library(plm)
library(modelsummary)
library(lmtest)
library(sandwich)

# Этап 1: Подгрузка данных и сведение табличек
# Данные представляют собой панельные данные (регионы РФ по годам). 
mean_salary_df <- read_excel("Среднедушевые денежные доходы.xlsx", sheet = 1, skip = 1)
mean_salary_df <- mean_salary_df[-1, ]  

flat_count_df <- read_excel("Количество построенных квартир.xlsx", sheet = 1, skip = 1)
flat_count_df <- flat_count_df[-1, ]  

# Находим общие колонки и фильтруем
common_cols <- intersect(colnames(mean_salary_df), colnames(flat_count_df))
mean_salary_df <- mean_salary_df[, colnames(mean_salary_df) %in% common_cols]
flat_count_df <- flat_count_df[, colnames(flat_count_df) %in% common_cols]

# Переименовываем первую колонку
colnames(mean_salary_df)[1] <- "Год"
colnames(flat_count_df)[1] <- "Год"

# Преобразование в длинный формат
mean_long <- mean_salary_df %>%
  pivot_longer(cols = -Год, names_to = "Регион", values_to = "mean_salary")

flat_long <- flat_count_df %>%
  pivot_longer(cols = -Год, names_to = "Регион", values_to = "flat_count")

# Объединение и очистка от нулей
final_df <- left_join(mean_long, flat_long, by = c("Год", "Регион")) %>%
  filter(mean_salary > 0 & flat_count > 0) # не попадают хотя бы с одним нулем

write.csv(final_df, "final_data.csv", row.names = FALSE, fileEncoding = "UTF-8")


print(final_df)
print(paste("Число наблюдений:", nrow(final_df)))
print(paste("Уникальных регионов:", n_distinct(final_df$Регион)))
print(paste("Уникальных лет:", n_distinct(final_df$Год)))


# Этап 2: Проверка наличия линейной связи
final_df_sorted <- final_df %>% arrange(mean_salary)

round(cor(final_df_sorted$mean_salary, final_df_sorted$flat_count), 4) # 0.1831756 
cor.test(final_df_sorted$mean_salary, final_df_sorted$flat_count, conf.level = 0.9)
# Корреляция слабая, но значимая (p-value = 0.00000007162). Линейная связь не сильна — рассмотрим нелинейные модели.


plot(final_df_sorted$mean_salary, final_df_sorted$flat_count,
     main = "Корреляционное поле",
     xlab = "Доходы", ylab = "Квартиры", pch = 18, col = "blue")

plot(log(final_df_sorted$mean_salary), log(final_df_sorted$flat_count),
     main = "Корреляционное поле",
     xlab = "Доходы", ylab = "Квартиры", pch = 18, col = "blue")


par(mar = c(5, 4, 4, 5) + 0.1)  

plot(salary_by_year$Год, salary_by_year$mean_salary_year,
     type = "b", pch = 19, col = "blue", lwd = 2,
     main = "Динамика доходов и строительства квартир по годам",
     xlab = "Год", ylab = "Средняя зарплата (руб.)",
     xaxt = "n")
axis(1, at = salary_by_year$Год, labels = salary_by_year$Год)

par(new = TRUE)
plot(flat_by_year$Год, flat_by_year$mean_flat_year,
     type = "b", pch = 17, col = "red", lwd = 2,
     axes = FALSE, xlab = "", ylab = "")
axis(4, at = pretty(range(flat_by_year$mean_flat_year)))  
mtext("Среднее количество квартир", side = 4, line = 3)  

legend("topleft", 
       legend = c("Средняя зарплата", "Среднее количество квартир"),
       col = c("blue", "red"), 
       lwd = 2, 
       pch = c(19, 17),
       bty = "n")  


# Корреляционное поле с выделением 10 регионов
#set.seed(123)  # для воспроизводимости
selected_regions <- sample(unique(final_df$Регион), 10)

colors <- rainbow(10)

plot(final_df$mean_salary, final_df$flat_count,
     main = "Корреляционное поле: Доходы и построенные квартиры",
     xlab = "Среднедушевые доходы (руб.)", 
     ylab = "Количество построенных квартир",
     pch = 16, col = "lightgray", cex = 0.7)

for (i in 1:10) {
  region <- selected_regions[i]
  region_data <- final_df %>% 
    filter(Регион == region) %>%
    arrange(Год)  # сортируем по годам для правильного соединения линий
  
  lines(region_data$mean_salary, region_data$flat_count, 
        col = colors[i], lwd = 2, type = "b", pch = 16)
}

legend("topright", legend = selected_regions, 
       col = colors, lwd = 2, pch = 16, cex = 0.7, ncol = 2)


# Этап 3: Построение и сравнение моделей
# Сравнить линейную, лог-лог, лог-лин и лин-лог формы для выбора лучшей.
reg_lin <- lm(flat_count ~ mean_salary, data = final_df_sorted)  # Lin-lin
reg_log <- lm(log(flat_count) ~ log(mean_salary), data = final_df_sorted)  # Log-log
reg_log_lin <- lm(log(flat_count) ~ mean_salary, data = final_df_sorted)  # Log-lin
reg_lin_log <- lm(flat_count ~ log(mean_salary), data = final_df_sorted)  # Lin-log

models <- list(lin_lin = reg_lin, log_log = reg_log, log_lin = reg_log_lin, lin_log = reg_lin_log)

comparison <- data.frame(
  model = names(models),
  R2 = sapply(models, function(x) summary(x)$r.squared),
  Adj_R2 = sapply(models, function(x) summary(x)$adj.r.squared),
  AIC = sapply(models, AIC)
)
print(comparison)

# График для выбранной log-log (pooled)
plot(log(final_df_sorted$mean_salary), log(final_df_sorted$flat_count),
     main = "Лог-лог модель",
     xlab = "log(Доходы)", ylab = "log(Квартиры)", pch = 18, col = "blue")
abline(reg_log, col = "red", lwd = 2)

coef(reg_log)

# Вывод: Log-log показывает лучший баланс R² и AIC. Но R² низкий — учет панельной структуры (FE) может улучшить.


# Этап 4: Панельные модели на базе log-log
# Pooled OLS игнорирует региональные/временные фиксированные различия. FE по регионам должны сильно улучшить модель.

#?plm

pdata <- pdata.frame(final_df, index = c("Регион", "Год"))

m1 <- plm(log(flat_count) ~ log(mean_salary), data = pdata, model = "pooling")  # Pooled
m2 <- plm(log(flat_count) ~ log(mean_salary), data = pdata, model = "within", effect = "individual")  # FE регионы
m3 <- plm(log(flat_count) ~ log(mean_salary), data = pdata, model = "within", effect = "time")  # FE годы

modelsummary(list("Pooled" = m1, "FE регионы" = m2, "FE годы" = m3),
             stars = TRUE, title = "Сравнение панельных спецификаций", coef_omit = "Intercept")
coef(m2)
summary(m2) # p-value: < 0.000000000000000222 => Модель статистически значима на уровне значимости 0.1
# Вывод: FE по регионам (m2) — лучшая (within R² ~0.519). Выбираем m2 как основную.
# fixef(m2) # Получить все фиксированные эффекты регионов



regional_effects <- fixef(m2)

regional_df <- data.frame(
  Регион = names(regional_effects),
  alpha_i = as.numeric(regional_effects)
)

beta_value <- coef(m2)[1]  # 0.4064458

regional_df <- regional_df %>%
  mutate(
    alpha_rounded = round(alpha_i, 4),
    formula_math = paste0("log(flat_count) = ", alpha_rounded, 
                          " + ", beta_rounded, " × log(mean_salary)"),
  )

write.csv(regional_df, "regional_fixed_effects.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")



y_within <- pmodel.response(m2)  # log(flat_count_it) - среднее(log(flat_count_i)) — отклонения логарифма квартир в регионе i в год t от среднего значения по этому же региону за все годы
X_within <- model.matrix(m2)[,1] # log(mean_salary_it) - среднее(log(mean_salary_i)) — отклонения логарифма доходов от средних по региону

plot(X_within,
     y_within,
     pch=19,
     col=rgb(0,0,0,0.3),
     xlab="log(Доход)",
     ylab="log(Квартиры)",
     main="FE модель по регионам")

abline(a=0, b=coef(m2), col="blue", lwd=2)



# Этап 5: Верификация основной модели (m2)

# Остатки
res_m2 <- residuals(m2)
print(paste("Среднее остатков:", mean(res_m2)))  # ~0

plot(x = as.numeric(predict(m2)),
     y = as.numeric(m2$residuals),
     main = "Остатки ",
     xlab = "Предсказанные значения log(flat_count)",
     ylab = "Остатки",
     pch = 19, col = rgb(0, 0, 0, 0.4), cex = 0.8)

abline(h = 0, col = "red", lwd = 2)


karelia_res <- residuals(m2)[index(pdata)$Регион == "Республика Карелия"]
plot(karelia_res,
     main = "Остатки модели m2 — Республика Карелия",
     xlab = "Наблюдение (годы)",
     ylab = "Остатки", type = "b", pch = 19, col = "darkgreen")
abline(h = 0, col = "red")

# Q-Q 
qqnorm(res_m2, main = "Q-Q plot остатков")
qqline(res_m2, col = "red")


# MAPE
pred_log <- predict(m2)
pred_flats <- exp(pred_log)
actual_flats <- pdata$flat_count

mape <- mean(abs((pred_flats - actual_flats) / actual_flats)) * 100 # 21.64328
mape
# Вывод: Модель адекватна. 

# Этап 6: Прогноз для Республики Карелия 
year <- "2010"
karelia_data <- final_df %>% filter(Регион == "Республика Карелия", Год == year)
p_karelia <- pdata.frame(karelia_data, index = c("Регион", "Год"))

pred_log_m2 <- predict(m2, newdata = p_karelia)
pred_flats_m2 <- exp(pred_log_m2)

actual_salary <- karelia_data$mean_salary
actual_flats <- karelia_data$flat_count

cat("Фактический доход:", round(actual_salary, 0), "\n")
cat("Фактические квартиры:", actual_flats, "\n")
cat("Прогноз m2:", round(pred_flats_m2, 0), "\n")
cat("Абсолютная ошибка:", round(pred_flats_m2 - actual_flats, 0), "\n")
cat("Относительная ошибка:", round((pred_flats_m2 - actual_flats)/actual_flats * 100, 2), "%\n") # Прогноз близок к факту, ошибка разумна.
