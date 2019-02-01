library(ggplot2)
library(magrittr)

# library(readr)
# ucas_hosts <- read_csv(file = paste0(getwd(), "/data/raw-data/ucas-acceptances-by-country-subject-host.csv"))
# ucas_applicants <- read_csv(file = paste0(getwd(), "/data/raw-data/ucas-acceptances-by-country-subject-applicant.csv"))
# save(ucas_hosts,
#      file = paste0(getwd(), "/data/R/RData/ucas_hosts.rda"))
# save(ucas_applicants,
#      file = paste0(getwd(), "/data/R/RData/ucas_applicants.rda"))

load(file = getwd() %>% paste0("/analysis/data/rdata/ucas_hosts.rda"))
load(file = getwd() %>% paste0("/analysis/data/rdata/ucas_applicants.rda"))

computer_subjects <- c()
for (subject in ucas_applicants$`Subject Group (Detailed Level)` %>% unique()) {
  lowerSubject <- subject %>%
    tolower()
  if (grepl("compu", lowerSubject) || grepl("cyber", lowerSubject)) {
    computer_subjects %<>% append(subject)
  }
}

filters <- list(
  subject = "'I1 - Computer Science'",
  applicationRoute = "'Firm choice'",
  domicile = "'Northern Ireland'")

ucas_data <- ucas_applicants %>%
  subset(`Subject Group (Detailed Level)` == filters$subject) %>%
  subset(`Acceptance Route` == filters$applicationRoute) %>%
  subset(`Applicant Domicile (High Level)` == filters$domicile)

ucas_data$hostEntrants <- ucas_hosts %>%
  subset(`Subject Group (Detailed Level)` == filters$subject) %>%
  subset(`Acceptance Route` == filters$applicationRoute) %>%
  subset(`Provider Country` == filters$domicile) %$% 
  `Number of Acceptances`

ucas_data$diff <- ucas_data$`Number of Acceptances` - 
  ucas_hosts_computer$`Number of Acceptances`

ucas_data$vacanciesRecorded <- c(540, 505, 530, 560, 610, 635, 700, 775, 760, 715, 785, 745)

ucas_application_chart <- ggplot(
    data = ucas_data,
    aes(x = `Cycle Year`)) + 
  geom_point(aes(y = `Number of Acceptances`,
                 colour = "NI Applications")) +
  geom_line(aes(y = `Number of Acceptances`,
                colour = "NI Applications")) + 
  geom_point(aes(y = hostEntrants,
                 colour = "NI Entrants")) +
  geom_line(aes(y = hostEntrants, 
                colour = "NI Entrants")) +
  theme_minimal()

ucas_gap_chart <- ggplot(
  data = ucas_data,
  aes(x = `Cycle Year`)) + 
  geom_point(aes(y = diff, 
                 colour = "Applicant / entrant gap")) + 
  geom_line(aes(y = diff, 
                colour = "Applicant / entrant gap")) +
  theme_minimal()
