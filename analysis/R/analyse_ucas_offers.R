library(ggplot2)
library(ggthemes)
library(magrittr)


# library(readr)
#
# ucas_hosts <- read_csv(file = paste0(getwd(), "/data/raw-data/ucas-acceptances-by-country-subject-host.csv"))
# ucas_applicants <- read_csv(file = paste0(getwd(), "/data/raw-data/ucas-acceptances-by-country-subject-applicant.csv"))
#
# save(ucas_hosts,
#      file = paste0(getwd(), "/data/R/RData/ucas_hosts.rda"))
# save(ucas_applicants,
#      file = paste0(getwd(), "/data/R/RData/ucas_applicants.rda"))


get_ucas_subjects <- function(ucasApplicationDoata) {
  subjects <- c()
  for (subject in ucasApplicationDoata$`Subject Group (Detailed Level)` %>% unique()) {
    lowerSubject <- subject %>%
      tolower()
    if ((grepl("compu", lowerSubject) || grepl("cyber", lowerSubject) || 
         grepl("engineer", lowerSubject) || grepl("software", lowerSubject)) && 
        (!grepl("comb", lowerSubject) & !grepl("other", lowerSubject) & 
         !grepl("any", lowerSubject) & !grepl("general", lowerSubject) & 
         !grepl("audio", lowerSubject) & !grepl("aerospace", lowerSubject) & 
         !grepl("civil", lowerSubject) & !grepl("mechanical", lowerSubject) & 
         !grepl("manufact", lowerSubject) & !grepl("chemical", lowerSubject))) {
      subjects %<>% 
        append(subject)
    }
  }
  return(subjects)
}


ucas_applications_acceptances <- function(ucasApplicationData, ucasHostData, domicile, applicationRoute) {
  filters <- list(
    subjects = ucasApplicationData %>% get_ucas_subjects(),
    applicationRoute = applicationRoute,
    domicile = domicile)
  
  ucas_data <- ucasApplicationData %>%
    subset(`Subject Group (Detailed Level)` %in% filters$subject) %>%
    subset(`Acceptance Route` %in% filters$applicationRoute) %>%
    subset(`Applicant Domicile (High Level)` == filters$domicile)
  
  hostEntrants <- ucasHostData %>%
    subset(`Subject Group (Detailed Level)` %in% filters$subject) %>%
    subset(`Acceptance Route` %in% filters$applicationRoute) %>%
    subset(`Provider Country` == filters$domicile)
  
  totalYears <- totalHostYears <- c()
  
  allYears <- ucas_data$`Cycle Year` %>% 
    unique()
  
  for (year in allYears) {
    sub <- subset(ucas_data, `Cycle Year` == year)
    subHosts <- subset(hostEntrants, `Cycle Year` == year)
    totalYears %<>% append(sum(sub$`Number of Acceptances`))
    totalHostYears %<>% append(sum(subHosts$`Number of Acceptances`))
  }
  
  ucasLine <- data.frame(
    years = allYears,
    totalApplicationYears = totalYears,
    totalHostYears = totalHostYears,
    stringsAsFactors = FALSE) %>%
    ggplot() + 
    geom_line(
      mapping = aes(
        x = years, 
        y = totalApplicationYears, 
        colour = "Applications")) + 
    geom_point(
      mapping = aes(
        x = years, 
        y = totalApplicationYears, 
        colour = "Applications")) +
    geom_line(
      mapping = aes(
        x = years, 
        y = totalHostYears, 
        colour = "Acceptances")) +
    geom_point(
      mapping = aes(
        x = years, 
        y = totalHostYears, 
        colour = "Acceptances")) +
    scale_colour_discrete(
      name = "",
      labels = c("UCAS Acceptances", "UCAS Applications")) +
    ylab("Number") + xlab("Years") +
    labs(title = "Number of UCAS applications from NI students to UK firms &\n number of UCAS acceptancs by NI universities in EEECS subjects") +
    theme_minimal()
  
  getwd() %>% 
    paste0("/analysis/images/ucas-applications-", applicationRoute %>% tolower(), ".png") %>% 
    ggsave(
      plot = ucasLine, 
      device = "png")
  
  return(ucasLine)
}


ucas_application_acceptance_diff <- function(ucasApplicationData, ucasHostData, applicationRoute, domicile) {
  filters <- list(
    subjects = ucasApplicationData %>% get_ucas_subjects(),
    applicationRoute = applicationRoute,
    domicile = domicile)
  
  ucas_data <- ucasApplicationData %>%
    subset(`Subject Group (Detailed Level)` %in% filters$subject) %>%
    subset(`Acceptance Route` %in% filters$applicationRoute) %>%
    subset(`Applicant Domicile (High Level)` == filters$domicile)
  
  hostEntrants <- ucasHostData %>%
    subset(`Subject Group (Detailed Level)` %in% filters$subject) %>%
    subset(`Acceptance Route` %in% filters$applicationRoute) %>%
    subset(`Provider Country` == filters$domicile)
  
  ucas_data$demandSupplyDiff <- ucas_data$`Number of Acceptances` - 
    hostEntrants$`Number of Acceptances`
  
  ucas_gap_chart <- ggplot(
    data = ucas_data,
    aes(x = `Cycle Year`)) + 
    geom_point(aes(y = demandSupplyDiff,
                   colour = `Subject Group (Detailed Level)`)) + 
    geom_line(aes(y = demandSupplyDiff, 
                  colour = `Subject Group (Detailed Level)`)) +
    labs(title = paste0("Difference between number of University applications and number of acceptances (2007--2018)")) +
    ylab("Demand / supply difference") + xlab("Year") +
    scale_colour_discrete(
      name = "Subject",
      labels = c("Electronic & Electrical Engineering", "Computer Science", "Software Engineering")) +
    theme_minimal()
  
  getwd() %>% 
    paste0("/analysis/images/ucas-demand-suplus.png") %>% 
    ggsave(
      plot = ucas_gap_chart, 
      device = "png")
  
  return(ucas_gap_chart)
}


generate_host_acceptance_area <- function(ucasApplicationData, ucasHostData, applicationRoute, domicile) {
  filters <- list(
    subjects = ucasApplicationData %>% 
      get_ucas_subjects(),
    applicationRoute = applicationRoute,
    domicile = domicile)
  
  hostEntrants <- ucasHostData %>%
    subset(`Subject Group (Detailed Level)` %in% filters$subject) %>%
    subset(`Acceptance Route` %in% filters$applicationRoute) %>%
    subset(`Provider Country` == filters$domicile)
  
  ucasAcceptanceChart <- ggplot(
    data = hostEntrants,
    aes(x = `Cycle Year`,
        y = `Number of Acceptances`,
        fill = `Subject Group (Detailed Level)`)) +
    geom_area(
      colour = "black", 
      size = .2, 
      alpha = .4) +
    labs(title = paste0("All NI University acceptances by all ", filters$applicationRoute," applicants (2007--2018)")) +
    ylab("Number of acceptances") + 
    xlab("Year") +
    scale_fill_discrete(
      name = "Subject",
      labels = c("Electronic & Electrical Engineering", "Computer Science", "Software Engineering")) +
    theme_minimal()
  
  getwd() %>% 
    paste0("/analysis/images/ucas-acceptance.png") %>%
    ggsave(
      plot = ucasAcceptanceChart, 
      device = "png")
  
  return(ucasAcceptanceChart)
}


load(file = getwd() %>% paste0("/analysis/data/rdata/ucas_hosts.rda"))
load(file = getwd() %>% paste0("/analysis/data/rdata/ucas_applicants.rda"))

generate_host_acceptance_area(
  ucasApplicationData = ucas_applicants,
  ucasHostData = ucas_hosts,
  applicationRoute = "'Firm choice'", 
  domicile = "'Northern Ireland'")

ucas_applications_acceptances(
  ucasApplicationData = ucas_applicants,
  ucasHostData = ucas_hosts,
  applicationRoute = "'Firm choice'", 
  domicile = "'Northern Ireland'")

ucas_application_acceptance_diff(
  ucasApplicationData = ucas_applicants,
  ucasHostData = ucas_hosts,
  applicationRoute = "'Firm choice'", 
  domicile = "'Northern Ireland'")



clean_subject <- function(subject) {
  if (grepl(" - ", subject)) {
    subject %<>% 
      strsplit(split = " - ")
    subject <- subject[[1]][2] %>% 
      gsub(pattern = "'", replacement = "")
  } else {
    subject <- NA
  }
  return(subject)
}


where_students_go <- function(ucasApplicationData, fromYear, domicile) {
  ucasApplicationData %<>% 
    subset(`Applicant Domicile (High Level)` == domicile &
             `Cycle Year` >= fromYear)
  for (i in 1:(ucasApplicationData %>% nrow())) {
    ucasApplicationData$cleanSubject[i] <- ucasApplicationData$`Subject Group (Detailed Level)`[i] %>% 
      clean_subject()
  }
  subjectero <- subjectYear <- totalNumber <- c()
  cleanSubjects <- ucasApplicationData$cleanSubject %>% 
    unique()
  for (subject in cleanSubjects) {
    if (!(subject %>% is.na())) {
      subjectSubset <- ucasApplicationData %>% 
        subset(cleanSubject == subject) 
      for (year in subjectSubset$`Cycle Year` %>% unique()) {
        subjectero %<>% 
          append(subject)
        subjectYear %<>% 
          append(year)
        totalNumber %<>% append(subjectSubset %>%
          subset(`Cycle Year` == year) %$%
          `Number of Acceptances` %>% 
          sum())
      }
    }
  }
  df <- data.frame(
    subject = subjectero,
    year = subjectYear,
    number = totalNumber,
    stringsAsFactors = FALSE)
  completeSubjects <- variance <- totalChange <- c()
  for (s in cleanSubjects) {
    subject.df <- df %>% 
      subset(subject == s)
    if (fromYear %in% subject.df$year & (subjectYear %>% max()) %in% subject.df$year) {
      variance %<>% append(var(df %>% subset(subject == s) %$% number))
      t1 <- df %>% subset(subject == s & year == fromYear) %$% number
      t2 <- df %>% subset(subject == s & year == subjectYear %>% max()) %$% number
      totalChange %<>% append(t2 - t1)
      completeSubjects %<>% append(s)
    }
  }
  
  df2 <- data.frame(
    subject = completeSubjects, 
    variance = variance, 
    totalChange = totalChange)
  
  df3 <- rbind(
    df2[order(totalChange), ][1:10, ],
    df2[order(-totalChange), ][1:10, ]) %>%
    as.data.frame() %<>% 
    .[order(-.$totalChange), ]
  
  df3$subject %<>% 
    gsub(" and ", " & ", .) %<>% 
    gsub(" by Area", "", .) %<>%
    gsub("Combinations within ", "", .) %<>%
    gsub(" by Period", "", .)  %<>%
    gsub("Building", "Building & Planning", .)  %<>%
    gsub("Others in Biological Sciences", "Biological Sciences", .)
  
  df3$subject %<>% factor(
    levels = df3[order(-df3$totalChange), ]$subject)
  
  topBottom10Bar <- df3 %>%
    ggplot() +
    geom_bar(
      mapping = aes(
        x = subject, 
        y = totalChange,
        fill = totalChange > 0), 
      stat = "identity",
      position = "identity") +
    geom_hline(yintercept = 0) +
    coord_flip() +
    ylab("") +
    xlab("") +
    theme_minimal() +
    theme(
      legend.position = "none")
  
  getwd() %>%
    paste0("/analysis/images/change-in-acceptances-", fromYear, "-", 
           (subjectYear %>% max()),".png") %>%
    ggsave(
      plot = topBottom10Bar, 
      device = "png")
  
  return(topBottom10Bar)
}


where_students_go(
  ucasApplicationData = ucas_applicants,
  fromYear = 2013,
  domicile = "'Northern Ireland'")
