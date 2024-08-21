# Load necessary libraries
library(ggplot2)
library(reshape2)

# Load the preprocessed data
data <- read_csv("C:/Users/Ahmad Afzal/Desktop/Refined_Scotland_WRFdata.csv")

# Ensure the column names are correct
colnames(data) <- c("TSK", "PSFC", "U10", "V10", "Q2", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS", "DATETIME", "WIND_SPEED", "Hour", "Day_Night", "Date")

# Convert columns to appropriate types
data$DATETIME <- as.POSIXct(data$DATETIME, format = "%d.%m.%Y.%H.%M")
data$Day_Night <- as.factor(data$Day_Night)
data$Hour <- as.numeric(data$Hour)
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

# 1. Temperature at Skin Level (TSK) Distribution
p1 <- ggplot(data, aes(x = TSK)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Temperature at Skin Level (TSK)", x = "TSK", y = "Frequency") +
  theme_minimal()

# Plot 1
print(p1)

# 2. Surface Pressure (PSFC) Distribution
p2 <- ggplot(data, aes(x = PSFC)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Surface Pressure (PSFC)", x = "PSFC", y = "Frequency") +
  theme_minimal()

# Plot 2
print(p2)

# 3. Wind Speed Distribution
p3 <- ggplot(data, aes(x = WIND_SPEED)) +
  geom_histogram(binwidth = 0.5, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Wind Speed", x = "Wind Speed", y = "Frequency") +
  theme_minimal()

# Plot 3
print(p3)

# 4. Relationship between TSK and PSFC
p4 <- ggplot(data, aes(x = PSFC, y = TSK)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between Surface Pressure (PSFC) and TSK", x = "PSFC", y = "TSK") +
  theme_minimal()

# Plot 4
print(p4)

# 5. Wind Components (U10 and V10) Distribution
p5 <- ggplot(data, aes(x = U10, y = V10)) +
  geom_point(color = "orange", alpha = 0.6) +
  labs(title = "Distribution of Wind Components (U10 vs V10)", x = "U10", y = "V10") +
  theme_minimal()

# Plot 5
print(p5)

# 6. Correlation Heatmap
numeric_data <- data[, c("TSK", "PSFC", "U10", "V10", "Q2", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS", "WIND_SPEED")]
cor_matrix <- cor(numeric_data, use = "complete.obs")
melted_cor_matrix <- melt(cor_matrix)

p6 <- ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 6
print(p6)

# 7. Boxplots of TSK by Day/Night
p7 <- ggplot(data, aes(x = Day_Night, y = TSK, fill = Day_Night)) +
  geom_boxplot() +
  labs(title = "Boxplot of TSK by Day/Night", x = "Day/Night", y = "TSK") +
  theme_minimal()

# Plot 7
print(p7)

# 8. Rainfall (RAINC) over Time
p8 <- ggplot(data, aes(x = DATETIME, y = RAINC)) +
  geom_line(color = "darkblue", alpha = 0.7) +
  labs(title = "Rainfall (RAINC) Over Time", x = "DateTime", y = "RAINC") +
  theme_minimal()

# Plot 8
print(p8)

# 9. Scatter Plot Matrix
pairs(data[, c("TSK", "PSFC", "U10", "V10", "Q2", "WIND_SPEED")], main = "Scatterplot Matrix")

# 10. Monthly Average TSK
data$Month <- format(data$Date, "%Y-%m")
monthly_avg_tsk <- aggregate(TSK ~ Month, data, mean)

p9 <- ggplot(monthly_avg_tsk, aes(x = Month, y = TSK)) +
  geom_line(group = 1, color = "darkred") +
  labs(title = "Monthly Average TSK", x = "Month", y = "Average TSK") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 9
print(p9)
