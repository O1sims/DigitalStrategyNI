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
    ggplot(
      mapping = aes(
        x = year, 
        y = percentage,
        group = NA)) +
    geom_line() +
    geom_point() +
    ylab("Percentage of supported projects in ICT (%)") +
    xlab("Financial year") +
    theme_minimal()
  
  getwd() %>%
    paste0("/analysis/images/ini-percentage-projects-ict.png") %>% 
    ggsave(
      plot = percentageChart, 
      device = "png")
  
  return(percentageChart)
}


generate_job_creation_bar <- function(INIData, SICSector = NULL) {
  if (!is.null(SICSector)) {
    INIData %<>% 
      subset(`SIC Sector` == SICSector)
    s <- SICSector %>% 
      tolower() %<>%
      gsub(" ", "-", .)
  } else {
    s <- ""
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
    scale_fill_discrete(name = "Ownership") +
    theme_minimal() +
    theme()
  
  getwd() %>%
    paste0("/analysis/images/ini-job-creation-", s, ".png") %>%
    ggsave(
      plot = INIDataChart,
      device = "png")
  
  return(INIDataChart)
}


violin_chart_total_investment <- function(INIData, SICSectors = c()) {
  violinChart <- INIData %>% 
    subset(
      `SIC Sector` %in% 
        SICSectors) %>%
    ggplot(
      mapping = aes(
        x = `SIC Sector`, 
        y = (`Total Investment (Includes Invest NI Assistance)`)/1000000,
        group = `Ownership when the offer was made`,
        colour = `Ownership when the offer was made`,
        fill = `Ownership when the offer was made`)) +
    geom_violin() +
    xlab("") +
    scale_y_continuous(
      name = "Total investment (£ million)", 
      labels = scales::comma) +
    scale_fill_discrete(name = "Ownership") +
    scale_color_discrete(guide = 'none') +
    theme_minimal() +
    theme()
  
  getwd() %>%
    paste0("/analysis/images/ini-distribution-of-ict-investments.png") %>%
    ggsave(
      plot = violinChart,
      device = "png")
  
  return(violinChart)
}


total_investment_over_time <- function(INIData, SICSector = NA) {
  if (!is.na(SICSector)) {
    INIData %<>% 
      subset(`SIC Sector` == SICSector)
    s <- SICSector %>% 
      tolower() %<>%
      gsub(" ", "-", .)
  } else {
    s <- ""
  }
  
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
  
  investmentOverTime <- INIData %>%
    ggplot() +
    geom_bar(
      aes(
        x = `Financial Year Offer Made`, 
        y = `Total Assistance Offered by Invest NI` / 1000000,
        fill = `Ownership when the offer was made`), 
      stat = "identity") +
    xlab(label = "") + 
    scale_y_continuous(
      name = "Total investment (£ million)", 
      labels = scales::comma) +
    scale_fill_discrete(
      name = "Ownership") +
    theme_minimal()
  
  getwd() %>%
    paste0("/analysis/images/ini-investment-over-time", s, ".png") %>%
    ggsave(
      plot = investmentOverTime,
      device = "png")
  
  return(investmentOverTime)
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




# Percentage of jobs created in ICT
for (year in INIData$`Financial Year Offer Made` %>% unique()) {
  yearINIData <- subset(
    x = INIData, 
    subset = `Financial Year Offer Made` == year)
  
  yearICData <- subset(
    x = yearINIData, 
    subset = `SIC Sector` == SICSector)
  
  s <- sum(yearICData$`Jobs to be Created (Assisted)`)
  
  n <- sum(yearINIData$`Jobs to be Created (Assisted)`)
  
  print(s/n)
}




ictJobPercentage <- c()

uniqueYears <- INIData$`Financial Year Offer Made` %>% 
  unique()

for (year in uniqueYears) {
  yearINIData <- subset(
    x = INIData, 
    subset = `Financial Year Offer Made` == year)
  
  yearICData <- subset(
    x = yearINIData, 
    subset = `SIC Sector` == SICSector)
  
  s <- sum(yearICData$`Jobs to be Created (Assisted)`)
  
  n <- sum(yearINIData$`Jobs to be Created (Assisted)`)
  
  ictJobPercentage %<>% append(s/n)
}

proportionJobsICT <- data.frame(
  year = uniqueYears,
  jobPercentage = ictJobPercentage,
  stringsAsFactors = FALSE) %>%
  ggplot(
    mapping = aes(
      x = year, 
      y = jobPercentage,
      group = NA)) +
  geom_line() +
  geom_point() +
  ylab("Percentage of jobs created in ICT relative to all sectors (%)") +
  xlab("") +
  theme_minimal()

getwd() %>%
  paste0("/analysis/images/ini-proportion-jobs-created-ict.png") %>%
  ggsave(
    plot = proportionJobsICT,
    device = "png")
