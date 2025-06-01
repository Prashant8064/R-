# 📦 Load required package
library(ggplot2)

# 📊 Step 1: Create mock data
year <- 2000:2025
crude_oil_price <- c(22, 24, 25, 28, 32, 40, 50, 60, 70, 
                     110, 105, 96, 60, 45, 50, 65, 70, 75, 
                     80, 85, 90, 95, 100, 105, 110, 115)
inflation <- c(3.2, 3.5, 3.8, 4.1, 4.3, 5.2, 6.5, 6.8, 7.5, 
               10.1, 9.8, 8.5, 5.0, 4.0, 4.5, 5.0, 5.2, 5.5,
               6.0, 6.5, 6.8, 7.0, 7.2, 7.4, 7.6, 7.8)

# 🧱 Step 2: Create data frame
df <- data.frame(year, crude_oil_price, inflation)

# 👁 Step 3: Show first few rows
head(df)

# 📈 Step 4: Plot Trends (Crude Oil vs Inflation)
ggplot(df, aes(x = year)) +
  geom_line(aes(y = crude_oil_price, color = "Crude Oil ($/bbl)"), size = 1.5) +
  geom_line(aes(y = inflation * 10, color = "Inflation Rate (% x10)"), size = 1.5, linetype = "dashed") +
  scale_y_continuous(
    name = "Crude Oil Price ($/bbl)",
    sec.axis = sec_axis(~./10, name = "Inflation Rate (%)")
  ) +
  labs(title = "📈 Crude Oil Prices vs Inflation (India 2000–2025)",
       subtitle = "Inflation line scaled x10 for comparison",
       x = "Year", color = "Legend") +
  scale_color_manual(values = c("Crude Oil ($/bbl)" = "darkblue", "Inflation Rate (% x10)" = "firebrick")) +
  theme_minimal(base_size = 15)

# 📉 Step 5: Build Linear Regression Model
model <- lm(inflation ~ crude_oil_price, data = df)

# 📊 Step 6: Show Regression Summary
cat("📊 Regression Model Summary:\n")
summary(model)

# 🔍 Step 7: Regression Scatter Plot
ggplot(df, aes(x = crude_oil_price, y = inflation)) +
  geom_point(color = "#007f5f", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "#ff6f00", size = 1.2) +
  labs(title = "🔍 Regression: Crude Oil Price vs Inflation",
       x = "Crude Oil Price ($/bbl)", 
       y = "Inflation Rate (%)") +
  theme_minimal(base_size = 15)
