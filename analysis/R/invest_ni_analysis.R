library(readr)
library(ggplot2)
library(ggthemes)
library(magrittr)


generate_ict_proportion_projects <- function(INIData, SICSector) {
  ictPercentage <- c()
  uniqueYears <- INIData$`Financial Year Offer Made` %>% 
    unique()
  
  for (year in uniqueYears) {
    sicSectors <- INIData %>% 
      subset(`Financial Year Offer Made` == year) %$%
      `SIC Sector`
    ictPercentage %<>% 
      append(values = 100 * 
               (sum(sicSectors == SICSector) / length(sicSectors)))
  }
  
  percentageChart <- data.frame(
    year = uniqueYears,
    percentage = ictPercentage,
    stringsAsFactors = FALSE) %>%
    ggplot(aes(
      x = year, 
      y = percentage)) +
    geom_point() +
    ylab(label = "Percentage of projects (%)") +
    xlab(label = "Financial year") +
    theme_minimal()
  
  return(percentageChart)
}


generate_job_creation_bar <- function(INIData, SICSector = NULL) {
  if (!is.null(SICSector)) {
    INIData %<>% 
      subset(`SIC Sector` == SICSector)
  }
  
  INIDataChart <- INIData %>%
    ggplot() + 
    geom_bar(
      aes(
        x = `Financial Year Offer Made`, 
        y = `Jobs to be Created (Assisted)`,
        fill = `Ownership when the offer was made`), 
      stat = "identity") +
    ylab(label = "Jobs created") +
    xlab(label = "Financial year") +
    labs(title = "Invest Northern Ireland Activity") +
    theme_minimal() +
    theme(legend.position = "none")
  
  return(INIDataChart)
}


violin_chart_total_investment <- function(INIData, SICSectors = c()) {
  INIData %>% 
    subset(
      `SIC Sector` %in% 
        SICSectors) %>%
    ggplot() +
    geom_violin(
      aes(
        x = `SIC Sector`, 
        y = (`Total Investment (Includes Invest NI Assistance)`)/1000000,
        fill = `SIC Sector`)) +
    xlab(label = "SIC sector") +
    scale_y_continuous(
      name = "Total investment (£ million)", 
      labels = scales::comma) +
    theme_minimal() +
    theme(legend.position = "none")
}


total_investment_over_time <- function(INIData, SICSector) {
  INIData %<>% 
    subset(`SIC Sector` == SICSector) 
  
  totalInvestment <- c()
  allFinancialYears <- INIData$`Financial Year Offer Made` %>% 
    unique()
  for (financialYear in allFinancialYears) {
    total <- INIData %>% 
      subset(`Financial Year Offer Made` == financialYear) %$% 
      `Total Investment (Includes Invest NI Assistance)` %>% 
      sum()
    
    totalInvestment %<>% 
      append(values = total)
  }
  
  INIData %>%
    ggplot() +
    geom_bar(
      aes(
        x = `Financial Year Offer Made`, 
        y = `Total Investment (Includes Invest NI Assistance)`,
        fill = `Ownership when the offer was made`), 
      stat = "identity") +
    ylab(label = "Total investment (£)") +
    xlab(label = "Financial year") + 
    scale_fill_discrete(breaks = c("External","Local")) +
    theme_minimal() +
    theme(
      legend.title = element_blank())
}


# Proportion of investment provided by Invest NI
average_amount_ini <- function(sector = NA) {
  financed.INI.projects <- INIData %>% 
    subset(`Total Assistance Offered by Invest NI` > 0 & 
             `Total Investment (Includes Invest NI Assistance)` > 0)
  
  if (!is.na(sector)) {
    financed.INI.projects %<>% 
      subset(`SIC Sector` == sector)
  }
  
  (financed.INI.projects$`Total Assistance Offered by Invest NI` / 
      financed.INI.projects$`Total Investment (Includes Invest NI Assistance)`) %>% 
    mean() %>%
    return()
}


# Load up INI data
INIData <- getwd() %>%
  paste0("/analysis/data/raw-data/ini-open-data-upload-09jan19.csv") %>%
  read_csv(progress = TRUE)

# Clean up some of the data
INIData$`SIC Sector` %<>% gsub(
  pattern = "&", 
  replacement = "And")
INIData$`SIC Sector` %<>% gsub(
  pattern = "Wholesale And Retail Trade; Repair Of Motor Vehicles And Motorcycles", 
  replacement = "Wholesale And Retail Trade And Repair of Vehicles")

# Define the SIC sector to analyse
SICSector <- "Information And Communication"

# Sector of interest only
ICData <- INIData %>% 
  subset(`SIC Sector` == SICSector)

# Generate job creation bar chart
generate_job_creation_bar(
  INIData = INIData,
  SICSector = SICSector)

# Generate chart to map the proportion of ICT projects
generate_ict_proportion_projects(
  INIData = INIData, 
  SICSector = SICSector)

# Generate violin chart 
violin_chart_total_investment(
  INIData = INIData,
  SICSectors = c(SICSector))

# Generate stacked bar chart for total investment
total_investment_over_time(
  INIData = INIData, 
  SICSector = SICSector)

# Total number of jobs created in the `Information and Communication` sector
ICData$`Jobs to be Created (Assisted)` %>%
  sum()

# Total number of jobs created from external firms in the `Information and Communication` sector
ICData %>% 
  subset(`Ownership when the offer was made` == "External") %$%
  `Jobs to be Created (Assisted)` %>% 
  sum()
