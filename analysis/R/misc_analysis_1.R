library(readr)
library(ggplot2)
library(ggthemes)
library(magrittr)


ucas_hosts_applications <- getwd() %>% 
  paste0("/analysis/data/raw-data/ucas-host-applications.csv") %>% 
  read_csv()
ucas_hosts_acceptances <- getwd() %>% 
  paste0("/analysis/data/raw-data/ucas-host-acceptances.csv") %>% 
  read_csv()

# ucas_applicants <- read_csv(file = paste0(getwd(), "/data/raw-data/ucas-acceptances-by-country-subject-applicant.csv"))
#
# save(ucas_hosts,
#      file = paste0(getwd(), "/data/R/RData/ucas_hosts.rda"))
# save(ucas_applicants,
#      file = paste0(getwd(), "/data/R/RData/ucas_applicants.rda"))

getwd() %>% paste0("/analysis/data/rdata/ucas_hosts.rda") %>% load()
getwd() %>% paste0("/analysis/data/rdata/ucas_applicants.rda") %>% load()


computer_subjects <- c()
for (subject in ucas_hosts_acceptances$`Subject Group (Summary Level)` %>% unique()) {
  lowerSubject <- subject %>%
    tolower()
  if (grepl("compu", lowerSubject) || grepl("cyber", lowerSubject) || grepl("software", lowerSubject)) {
    computer_subjects %<>% 
      append(subject)
  }
}

filters <- list(
  subjects = computer_subjects,
  host = "queen's university")

ucas_hosts_data <- ucas_hosts_acceptances %>%
  subset(grepl(pattern = filters$host, x = ucas_hosts_acceptances$`Provider Name`, ignore.case = TRUE)) %>%
  subset(`Subject Group (Summary Level)` %in% filters$subject)

totalYears <- totalHostYears <- c()
allYears <- ucas_hosts_data$`Cycle Year` %>% unique()
for (year in allYears) {
  sub <- subset(ucas_hosts_data, `Cycle Year` == year)
  totalYears %<>% append(sum(sub$`Number of Acceptances`))
}

df <- data.frame(
  years = allYears,
  totalApplicationYears = totalYears,
  stringsAsFactors = FALSE)

ggplot(data = df) + 
  geom_line(aes(x = years, y = totalApplicationYears, colour = "QUB")) + 
  geom_point(aes(x = years, y = totalApplicationYears, colour = "QUB")) +
  geom_line(aes(x = years, y = uu, colour = "UU")) + 
  geom_point(aes(x = years, y = uu, colour = "UU")) +
  ylab("Number of students") + xlab("Years") +
  labs(title = "Total number of UCAS acceptances for 'Computer Sciences' for NI Universities") +
  scale_colour_discrete(
    name = "University",
    labels = c("Queen's", "Ulster")) +
  theme_minimal()

ggsave(
  filename = getwd() %>% paste0("/analysis/images/ucas-ni-university-acceptances.png"), 
  device = "png")

ucas_data$demandSupplyDiff <- ucas_data$`Number of Acceptances` - 
  hostEntrants$`Number of Acceptances`

ucas_data$vacanciesRecorded <- c(
  540, 505, 530, 560, 610, 635, 700, 775, 760, 715, 785, 745)

ucas_application_chart <- ggplot(
  data = ucas_data,
  aes(x = `Cycle Year`,
      y = `Number of Acceptances`)) + 
  geom_line() +
  labs(title = paste0('UK university applications by all ', filters$applicationRoute, ' NI applicants (2007-2018)')) +
  ylab("Number of acceptances") + xlab("Year") +
  scale_fill_discrete(
    name = "Subject",
    labels = c("Electronic & Electrical Engineering", "Computer Science", "Software Engineering")) +
  theme_minimal()

ggsave(
  filename = getwd() %>% paste0("/analysis/images/ucas-applications-insurance.png"), 
  plot = ucas_application_chart, 
  device = "png")

ucas_acceptance_chart <- ggplot(
  data = hostEntrants,
  aes(x = `Cycle Year`,
      y = `Number of Acceptances`,
      fill = `Subject Group (Detailed Level)`)) +
  geom_area(
    colour = "black", 
    size = .2, 
    alpha = .4) +
  labs(title = paste0("All NI University acceptances by all ", filters$applicationRoute," applicants (2007--2018)")) +
  ylab("Number of acceptances") + xlab("Year") +
  scale_fill_discrete(
    name = "Subject",
    labels = c("Electronic & Electrical Engineering", "Computer Science", "Software Engineering")) +
  theme_minimal()

ggsave(
  filename = getwd() %>% paste0("/analysis/images/ucas-acceptance.png"), 
  plot = ucas_acceptance_chart, 
  device = "png")

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

ggsave(
  filename = getwd() %>% paste0("/analysis/images/ucas-demand-suplus.png"), 
  plot = ucas_gap_chart, 
  device = "png")
