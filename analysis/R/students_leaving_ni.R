library(ggplot2)
library(ggthemes)
library(magrittr)


year <- seq(
  from = 2007, 
  to = 2018)

totalLeavingPercent <- c(
  33.99, 32.91, 31.63, 34.62, 37.35, 32.08, 31.23, 31.82, 36.51, 36.01, 35.5, 38)

ictLeavingPercent <- c(
  8.3, 12.5, 10.7, 9.5, 12.45, 15.54, 22.35, 20.45, 27.06, 28.45, 35.93, 42.03)

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
  scale_y_continuous(limits = c(0, 50)) +
  xlab("Year") +
  ylab("Students leaving NI (%)") +
  scale_colour_discrete(
    name = " ",
    labels = c("Average", "EEECS")) +
  theme_minimal() + 
  theme(legend.position = "bottom")


ggsave(
  filename = getwd() %>% 
    paste0("/analysis/images/student-percentages-leaving-ni.png"), 
  device = "png")
