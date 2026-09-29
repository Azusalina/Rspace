library(ggplot2)
library(dplyr)
library(tidyr)


df <- read.csv("ict.csv")
colnames(df) <- c("year", "lv5", "lv5s", "lv5ss")

df_long <- df %>%
  pivot_longer(
    cols = c("lv5", "lv5s", "lv5ss"),
    names_to = "level",
    values_to = "percentage"
  )
df_long$level <- factor(df_long$level,
                        levels = c("lv5", "lv5s", "lv5ss"),
                        labels = c("Level 5", "Level 5*", "Level 5**") # 这里改成带星号的
)

predict_data <- df_long %>%
  group_by(level) %>%
  do({
    model <- lm(percentage ~ year, data = .)
    new_data <- data.frame(year = 2027)
    pred <- predict(model, newdata = new_data, interval = "confidence", level = 0.95)
    data.frame(
      year = 2027,
      fit = pred[1],   
      lwr = pred[2],   
      upr = pred[3]   
    )
  }) %>%
  ungroup()

print("result:")
print(predict_data)

####


my_colors <- c("Level 5" = "#2c7bb6", "Level 5*" = "#d7191c", "Level 5**" = "#1a9641")

ggplot() +
  geom_point(data = df_long, aes(x = year, y = percentage, color = level), size = 2) +
  geom_line(data = df_long, aes(x = year, y = percentage, color = level), linewidth = 0.8) +
  geom_smooth(data = df_long, aes(x = year, y = percentage, color = level), 
              method = "lm", se = TRUE, linetype = "dashed", alpha = 0.15) +
  geom_point(data = predict_data, aes(x = year, y = fit, color = level), 
             shape = 4, size = 4, stroke = 1.5) +
  geom_errorbar(data = predict_data, aes(x = year, ymin = lwr, ymax = upr, color = level),
                width = 0.3, linewidth = 0.8) +
  scale_x_continuous(breaks = seq(2013, 2027, 2)) + 
  scale_color_manual(values = my_colors) +
  labs(
    title = "ICT trend",
    x = "Year",
    y = "Percentage (%)",
    color = "Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave("ict_prediction_2027.svg", width = 10, height = 6)