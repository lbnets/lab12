data <- read.csv("C:\\Users\\Alina\\Desktop\\mentalhealth\\mentalUkraine.csv", sep = ",")
View(data)
library(ggplot2)
# Побудова графіка  в декартовій системі координат
ggplot(data, aes(x = Year, y = Depressive.disorders..share.of.population.)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(
    title = "Динаміка депресивних розладів (Україна)",
    x = "Рік",
    y = "Частка населення (%)"
  ) +
  theme_minimal()

# Побудова графіка у полярній системі координат
ggplot(data, aes(x = Year, y = Depressive.disorders..share.of.population.)) +
  geom_line(color = "black") +
  coord_polar() +
  labs(
    title = "Динаміка депресивних розладів в Україні",
    x = "Рік",
    y = "Частка населення (%)"
  ) +
  theme_minimal()

# Описова статистика
library(tidyr)
library(e1071) # Для обчислення асиметрії та ексцесу
mean_value <- mean(data$Depressive.disorders..share.of.population., na.rm = TRUE)  # Середнє арифметичне
std_error <- sd(data$Depressive.disorders..share.of.population., na.rm = TRUE) / sqrt(length(na.omit(ukraine_data_long$Value)))  # Стандартна помилка
median_value <- median(data$Depressive.disorders..share.of.population., na.rm = TRUE)  # Медіана
mode_value <- as.numeric(names(sort(table(data$Depressive.disorders..share.of.population.), decreasing = TRUE)[1]))  # Мода
sd_value <- sd(data$Depressive.disorders..share.of.population., na.rm = TRUE)  # Стандартне відхилення
variance_value <- var(data$Depressive.disorders..share.of.population., na.rm = TRUE)  # Дисперсія
kurtosis_value <- kurtosis(data$Depressive.disorders..share.of.population., na.rm = TRUE)  # Ексцес
skewness_value <- skewness(data$Depressive.disorders..share.of.population., na.rm = TRUE)  # Асиметрія
range_value <- max(data$Depressive.disorders..share.of.population., na.rm = TRUE) - min(data$Depressive.disorders..share.of.population., na.rm = TRUE)  # Розмах
min_value <- min(data$Depressive.disorders..share.of.population., na.rm = TRUE)  # Мінімум
max_value <- max(data$Depressive.disorders..share.of.population., na.rm = TRUE)  # Максимум
sum_value <- sum(data$Depressive.disorders..share.of.population., na.rm = TRUE)  # Сума значень
count <- sum(!is.na(data$Depressive.disorders..share.of.population.))  # Кількість значень

# Виведення результатів
cat("Середнє арифметичне:", mean_value, "\n")
cat("Стандартна помилка:", std_error, "\n")
cat("Медіана:", median_value, "\n")
cat("Мода:", mode_value, "\n")
cat("Стандартне відхилення:", sd_value, "\n")
cat("Дисперсія:", variance_value, "\n")
cat("Ексцес:", kurtosis_value, "\n")
cat("Асиметрія:", skewness_value, "\n")
cat("Розмах:", range_value, "\n")
cat("Мінімум:", min_value, "\n")
cat("Максимум:", max_value, "\n")
cat("Сума значень:", sum_value, "\n")
cat("Кількість значень:", count, "\n")

# Побудова гістограми
hist(data$Depressive.disorders..share.of.population.,
     main = "Гістограма показників депресивних розладів в Україні", # Заголовок
     xlab = "Частка населення що страждає на депресію (%)", # Підпис осі X
     ylab = "Частота" # Підпис осі Y
)

# Встановлення кількості інтервалів відповідно до n - 1
n <- length(values)
num_bins <- n - 1

# Побудова гістограми та обчислення кумуляти
hist_data <- hist(values, breaks = num_bins, plot = FALSE)  # Створюємо гістограму без візуалізації
cum_freq <- cumsum(hist_data$counts) / sum(hist_data$counts)  # Кумулятивна частота

# Графік кумуляти за даними гістограми
plot(hist_data$mids, cum_freq, type = "s", col = "blue", lwd = 2,
     xlab = "Частка населення (%)", ylab = "Частість (імовірність)",
     main = "Кумулята за даними гістограми")

# Сортування даних та обчислення кумулятивної суми
sorted_values <- sort(values)
cum_percentage <- cumsum(sorted_values) / sum(sorted_values)

# Графік кумуляти за інтегральним процентом
plot(1:length(cum_percentage), cum_percentage, type = "s", col = "red", lwd = 2,
     xlab = "Інтервали (кишені)", ylab = "Частість (імовірність)",
     main = "Кумулята за інтегральним процентом")

# Метод ковзної середньої
require(smooth)# Завантаження бібліотеки для  для аналізу та згладжування часових рядів

# Використання даних про частку населення з депресивними розладами
values <- data$Depressive.disorders..share.of.population.  # Дані про депресивні розлади

# Обчислення ковзного середнього для вказаних періодів h = w
sma(values, h = 3, silent = FALSE)  # Ковзне середнє для 3
sma(values, h = 5, silent = FALSE)  # Ковзне середнє для 5 
sma(values, h = 7, silent = FALSE)  # Ковзне середнє для 7
sma(values, h = 9, silent = FALSE) 
sma(values, h = 11, silent = FALSE) 
sma(values, h = 13, silent = FALSE) 
sma(values, h = 15, silent = FALSE) 

# Завантаження бібліотеки
library(TTR)  # Для функції SMA щоб порахувати кількість поворотних точок та коефіцієнти кореляції між оригінальними значеннями та згладженими

# Визначення розмірів вікон
window_sizes <- c(3, 5, 7, 9, 11, 13, 15)

# Ініціалізація для збереження результатів
smoothing_results <- list()
correlations <- numeric()
turning_points <- numeric()

# Цикл для обчислення згладжування для кожного розміру вікна
for (w in window_sizes) {
  # Згладжування методом ковзного середнього
  smoothed <- SMA(values, n = w)
  smoothing_results[[as.character(w)]] <- smoothed
  
  # Підрахунок поворотних точок
  turning_point_count <- sum(diff(sign(diff(smoothed, na.pad = TRUE)), na.pad = TRUE) != 0, na.rm = TRUE)
  turning_points <- c(turning_points, turning_point_count)
  
  # Кореляція між вихідними і згладженими даними
  corr <- cor(values, smoothed, use = "complete.obs")
  correlations <- c(correlations, corr)
}

# Формування результатів у таблицю
results <- data.frame(
  Window_Size = window_sizes,
  Turning_Points = turning_points,
  Correlation = correlations
)

# Вивід результатів
print(results)

#ПОБУДОВА узагальненого графіку результатів згладжування для однієї реалізації даних, лише один ряд);
plot(
  values, type = "l", col = "black", lwd = 2, 
  xlab = "Час", ylab = "Частка населення (%)", 
  main = "Узагальнений графік результатів згладжування"
)

# Додавання згладжених рядів для кожного вікна
colors <- rainbow(length(window_sizes))  # Кольори для різних вікон
for (i in seq_along(window_sizes)) {
  lines(smoothing_results[[as.character(window_sizes[i])]], 
        col = colors[i], lwd = 2)
}

# Додавання легенди
legend("topright", legend = paste("h =", window_sizes), 
       col = colors, lty = 1, lwd = 2, cex = 0.8)

#ПОБУДОВА кореляційної таблиці для всіх інтервалів згладжування
correlation_matrix <- matrix(nrow = length(smoothing_results), ncol = length(smoothing_results))
rownames(correlation_matrix) <- names(smoothing_results)
colnames(correlation_matrix) <- names(smoothing_results)

# Обчислення кореляції
for (i in names(smoothing_results)) {
  for (j in names(smoothing_results)) {
    correlation_matrix[i, j] <- cor(smoothing_results[[i]], smoothing_results[[j]], use = "complete.obs")
  }
}

print(round(correlation_matrix, 2))  # Вивід кореляційної таблиці(Округлення для зручності)


# ПОБУДОВА діаграми поворотних точок
plot(
  window_sizes, turning_points, 
  type = "b",                # Тип графіку: "b" - точки з лініями
  col = "blue",              # Колір лінії
  pch = 19,                  # Тип точок (заповнені кружечки)
  lwd = 2,                   # Товщина лінії
  xlab = "Розмір вікна згладжування (h)", 
  ylab = "Кількість поворотних точок",
  main = "Діаграма поворотних точок"
)
grid()# Додавання сітки

#ЕКСПОНЦІАЛЬНЕ ЗГЛАДЖУВАННЯ
library(TTR)
# Набір параметрів згладжування
alpha_values <- c(0.1, 0.15, 0.2, 0.25, 0.3)

# Функція для підрахунку поворотних точок
count_turning_points <- function(series) {
  sum(diff(sign(diff(series))) != 0)
}

# Зберігатимемо результати у списки
turning_points <- list()
correlations <- list()

# Згладжування ряду та обчислення поворотних точок і коефіцієнтів кореляції
for (alpha in alpha_values) {
  # Експоненціальне згладжування
  smoothed_series <- EMA(data$Depressive.disorders..share.of.population., n = 1 / alpha)
  
  # Видалення NA для порівняння
  valid_index <- !is.na(smoothed_series)
  original_series <- data$Depressive.disorders..share.of.population.[valid_index]
  smoothed_series <- smoothed_series[valid_index]
  
  # Кількість поворотних точок
  turning_points[[as.character(alpha)]] <- count_turning_points(smoothed_series)
  
  # Коефіцієнт кореляції
  correlations[[as.character(alpha)]] <- cor(original_series, smoothed_series)
}

# Виведення результатів
results <- data.frame(
  Alpha = alpha_values,
  TurningPoints = unlist(turning_points),
  Correlation = unlist(correlations)
)
print(results)

#ВИВЕДЕННЯ ГРАФІКУ
# Збільшуємо поля графіку
par(mar = c(5, 5, 4, 2) + 0.1)
library(ggplot2)

# Очищення активних графічних пристроїв
while (!is.null(dev.list()))  dev.off()

# Цикл для виведення графіків
for (alpha in alpha_values) {
  # Відкриття нового графічного вікна
  x11(width = 10, height = 6)
  
  # Експоненціальне згладжування
  smoothed_series <- EMA(data$Depressive.disorders..share.of.population., n = 1 / alpha)
  
  # Побудова графіка
  plot(smoothed_series, type = "o", col = "blue", pch = 16,
       main = paste("Alpha =", alpha), xlab = "Час", ylab = "Значення")
  lines(smoothed_series, col = "blue")
  
  # Пауза для перегляду кожного графіка
  readline(prompt="Press [Enter] to see the next plot")
}

# Завантаження матриці близькості з Excel
file_path <- "C:\\Users\\Lenovo\\Documents\\mentalUkraine.xlsx"  # Замініть на ваш шлях до файлу
proximity_matrix <- read_excel(file_path, range = "p28:t32", col_names = FALSE)

# Перетворення даних у матрицю
proximity_matrix <- as.matrix(proximity_matrix)

# Перевірка завантаженої матриці
print("Матриця близькості:")
print(proximity_matrix)

# Перевірка наявності NA
if (any(is.na(proximity_matrix))) {
  warning("Матриця містить NA. Замінюємо NA на 0.")
  proximity_matrix[is.na(proximity_matrix)] <- 0
}

# Перевірка, чи матриця квадратна
if (nrow(proximity_matrix) != ncol(proximity_matrix)) {
  stop("Матриця не квадратна! Перевірте дані.")
}

# Обчислення матриці відстаней
Mdist <- as.dist(proximity_matrix)

# Виконання ієрархічного кластерного аналізу
hc <- hclust(Mdist, method = "single")

# Побудова дендрограми
plot(hc, cex = 0.6, main = "Дендрограма матриці близькості")

# Побудова дендрограм для різних кількостей кластерів
# Для 2 кластерів
plot(hc, cex = 0.6, main = "2 кластери")
rect.hclust(hc, k = 2, border = "red")

# Для 4 кластерів
plot(hc, cex = 0.6, main = "4 кластери")
rect.hclust(hc, k = 4, border = "blue")

# Для 5 кластерів
plot(hc, cex = 0.6, main = "5 кластерів")
rect.hclust(hc, k = 5, border = "green")
