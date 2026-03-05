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
  filter(mean_salary > 0 & flat_count > 0) 

write.csv(final_df, "final_data.csv", row.names = FALSE, fileEncoding = "UTF-8")

#print(final_df)
#print(paste("Число наблюдений:", nrow(final_df)))
#print(paste("Уникальных регионов:", n_distinct(final_df$Регион)))
#print(paste("Уникальных лет:", n_distinct(final_df$Год)))


# Этап 2: Проверка наличия линейной связи
final_df_sorted <- final_df %>% arrange(mean_salary)

round(cor(final_df_sorted$mean_salary, final_df_sorted$flat_count), 4) # 0.1571
cor.test(final_df_sorted$mean_salary, final_df_sorted$flat_count, conf.level = 0.9)
# Корреляция слабая, но значимая (p-value = 0.00000007162). Линейная связь не сильна — рассмотрим нелинейные модели.

plot(final_df_sorted$mean_salary, final_df_sorted$flat_count,
     main = "Корреляционное поле",
     xlab = "Доходы", ylab = "Квартиры", pch = 18, col = "blue")
abline(reg_log, col = "red", lwd = 2)

plot(log(final_df_sorted$mean_salary), log(final_df_sorted$flat_count),
     main = "Корреляционное поле (log)",
     xlab = "Доходы", ylab = "Квартиры", pch = 18, col = "blue")
abline(reg_log, col = "red", lwd = 2)


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

# Вывод: Log-log показывает лучший баланс R² и AIC. Эластичность ~0.43. Но R² низкий — учет панельной структуры (FE) может улучшить.


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
# Вывод: FE по регионам (m2) — лучшая (within R² ~0.36, коэффициент ~0.44). Выбираем m2 как основную.


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


plot(as.numeric(pdata$Год), residuals(m2),
     main = "Остатки модели",
     xlab = "Год",
     ylab = "Остатки",
     pch = 19, col = rgb(0,0,0,0.3))
abline(h = 0, col = "red", lwd = 2)
lines(tapply(residuals(m2), pdata$Год, mean),  # сглаживание по годам
      type = "l", col = "blue", lwd = 2)

karelia_res <- residuals(m2)[index(pdata)$Регион == "Республика Карелия"]
plot(karelia_res,
     main = "Остатки модели m2 — Республика Карелия",
     xlab = "Наблюдение (годы)",
     ylab = "Остатки", type = "b", pch = 19, col = "darkgreen")
abline(h = 0, col = "red")

# Q-Q 
qqnorm(res_m2, main = "Q-Q plot остатков m2")
qqline(res_m2, col = "red")


# MAPE
pred_log <- predict(m2)
pred_flats <- exp(pred_log)
actual_flats <- pdata$flat_count

mape <- mean(abs((pred_flats - actual_flats) / actual_flats)) * 100 # 27.64328

# Вывод: Модель адекватна. 

# Этап 6: Прогноз для Республики Карелия с m2
year <- "2008"
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


y_within <- pmodel.response(m2)  # отклонения от среднего значения по каждому региону
X_within <- model.matrix(m2)[,1] # log(mean_salary) в within-форме

plot(X_within,
     y_within,
     pch=19,
     col=rgb(0,0,0,0.3),
     xlab="log(Доход)",
     ylab="log(Квартиры)",
     main="FE модель")

abline(a=0, b=coef(m2), col="blue", lwd=2)
