library(ggplot2)
library(ggthemes)


year <- seq(
  from = 2006, 
  to = 2018)

totalLeavingPercent <- c(
  35, 33.99, 32.91, 31.63, 34.62, 37.35, 32.08, 31.23, 31.82, 36.51, 36.01, 35.5, 38)

ictLeavingPercent <- c(
  28, 26.13, 29.36, 32.11, 28.18, 31.45, 33.54, 32.35, 33.45, 34.06, 37.45, 38.93, 40.03) + 2

df <- data.frame(
  year = year,
  totalLeavingPercent = totalLeavingPercent,
  ictLeavingPercent = ictLeavingPercent,
  stringsAsFactors = FALSE)

ggplot(data = df, aes(x = year)) + 
  geom_point(aes(y = totalLeavingPercent, colour = "a")) + 
  geom_line(aes(y = totalLeavingPercent, colour = "a")) + 
  geom_point(aes(y = ictLeavingPercent, colour = "b")) + 
  geom_line(aes(y = ictLeavingPercent, colour = "b")) + 
  scale_y_continuous(limits = c(20, 50)) +
  xlab("Year") +
  ylab("Students leaving NI (%)") +
  scale_colour_discrete(
    name = " ",
    labels = c("Average", "Computer Science")) +
  theme_minimal() + 
  theme(legend.position = "bottom")


ggsave(
  filename = getwd() %>% 
    paste0("/analysis/images/student-percentages-leaving-ni.png"), 
  device = "png")
