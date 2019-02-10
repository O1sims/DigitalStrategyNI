library(ggplot2)
library(ggthemes)
library(magrittr)


# library(readr)
# 
# gcse_alevels <- getwd() %>% 
#   paste0("/analysis/data/raw-data/ni-gcse-and-alevels.csv") %>%
#   read_csv()


chart_level_totals <- function(full_data, level = c("A Level", "AS Level", "GCSE"), gender = TRUE) {
  alevel_overall_data <- full_data %>%
    subset(Level==level) %>%
    subset(Education=="Overall")
  
  totals <- males <-females <- c()
  for (year in alevel_overall_data$Year %>% unique()) {
    yearly_data <- alevel_overall_data %>% 
      subset(Year==year)
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
      aes(x = year, y = value)) + 
      geom_area(position = 'stack') +
      labs(title = paste0("Number of ", level, " students (", minYear, "-", maxYear, ")")) +
      ylab(label = "Number") + xlab(label = "Year") +
      theme_minimal()
  } else {
    melted_totals_df %<>%
      subset(var=="totalNumber")
    chart <- ggplot(
      data = melted_totals_df,
      aes(x = year, y = value)) + 
      geom_point() + 
      geom_line() +
      labs(title = paste0("Number of ", level, " students (", minYear, "-", maxYear, ")")) +
      ylab(label = "Number") + xlab(label = "Year") +
      theme_minimal()
  }
  
  return(chart)
}

load(file = getwd() %>%
       paste0("/analysis/data/rdata/gcse_alevels.rda"))


chart_level_totals(
  full_data = gcse_alevels, 
  level = "AS Level", 
  gender = FALSE)

