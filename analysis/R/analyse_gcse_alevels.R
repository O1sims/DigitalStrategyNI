library(ggplot2)
library(ggthemes)
library(magrittr)


# library(readr)
# 
# gcse_alevels <- getwd() %>% 
#   paste0("/analysis/data/raw-data/ni-gcse-and-alevels.csv") %>%
#   read_csv()


chart_level_totals <- function(full_data, level = c("All", "A Level", "AS Level", "GCSE"), gender = TRUE) {
  if (level == "All") {
    alevel_overall_data <- full_data %>%
      subset(Education == "Overall")
  } else {
    alevel_overall_data <- full_data %>%
      subset(Level == level) %>%
      subset(Education == "Overall") %>%
      subset(Gender == "Total")
  }
  
  if ()
  ggplot(alevel_overall_data) +
    geom_bar(data = alevel_overall_data, aes(x = Year, y = Number, fill = Subject), colour = "black", 
              size = .2, 
              alpha = .4, stat = "identity")
  
  "~/Code/DigitalStrategyNI/analysis/images/" %>%
    paste0(level %>% tolower(), ifelse(gender, "-gender", ""), "-enrolment-by-subject.png") %>%
    ggsave(device = "png")
  
  totals <- males <-females <- c()
  for (year in alevel_overall_data$Year %>% unique()) {
    yearly_data <- alevel_overall_data %>% 
      subset(Year == year)
    male <- yearly_data %>% 
      subset(Gender == "Males") %$% 
      Number %>% sum()
    female <- yearly_data %>% 
      subset(Gender == "Females") %$% 
      Number %>% sum()
    total <- yearly_data %>% 
      subset(Gender == "Total") %$% 
      Number %>% sum()
    totals %<>% append(total)
    males %<>% append(male)
    females %<>% append(female)
  }
  
  totals_df <- data.frame(
    year = alevel_overall_data$Year %>% 
      unique(),
    Female = females,
    Male = males,
    totalNumber = totals,
    stringsAsFactors = FALSE)
  
  melted_totals_df <- reshape2::melt(data = totals_df)[8:28, ]
  melted_totals_df <- data.frame(rep(alevel_overall_data$Year %>% 
          unique(), 3)) %>% 
    cbind(melted_totals_df)
  
  names(melted_totals_df) <- c(
    "year", "var", "value")
  
  maxYear <- melted_totals_df$year %>% max()
  minYear <- melted_totals_df$year %>% min()
  
  if (gender) {
    melted_totals_df %<>% 
      subset(var!="totalNumber")
    
    chart <- ggplot(
      data = melted_totals_df,
      aes(x = year, y = value, fill = var)) + 
      geom_area(
        colour = "black",
        alpha = 0.4, 
        size = 0.2) +
      labs(title = paste0("Gender and number of enrolled ", level, " students (", minYear, "--", maxYear, ")")) +
      ylab(label = "Number") + 
      xlab(label = "Year") +
      scale_fill_discrete(
        name = "Gender") +
      theme_minimal()
  } else {
    melted_totals_df %<>%
      subset(var=="totalNumber")
    
    chart <- ggplot(
      data = melted_totals_df,
      aes(x = year, y = value)) +
      geom_area(
        colour = "black",
        alpha = 0.4, 
        size = 0.2) +
      geom_point() + 
      geom_line() +
      geom_area(alpha = 0.2) +
      labs(title = paste0("Number of enrolled ", level, " students (", minYear, "--", maxYear, ")")) +
      ylab(label = "Number") + xlab(label = "Year") +
      theme_minimal()
  }
  
  "~/Code/DigitalStrategyNI/analysis/images/" %>%
    paste0(level %>% tolower(), ifelse(gender, "-gender", ""), "-enrolment.png") %>%
    ggsave(
      plot = chart, 
      device = "png")
  
  return(chart)
}

load(file = getwd() %>%
       paste0("/analysis/data/rdata/gcse_alevels.rda"))


chart_level_totals(
  full_data = gcse_alevels,
  level = "A Level",
  gender = TRUE)

