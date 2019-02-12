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

load(file = getwd() %>% paste0("/analysis/data/rdata/ucas_hosts.rda"))
load(file = getwd() %>% paste0("/analysis/data/rdata/ucas_applicants.rda"))


computer_subjects <- c()
for (subject in ucas_applicants$`Subject Group (Detailed Level)` %>% unique()) {
  lowerSubject <- subject %>%
    tolower()
  if ((grepl("compu", lowerSubject) || grepl("cyber", lowerSubject) || 
      grepl("engineer", lowerSubject) || grepl("software", lowerSubject)) && 
      (!grepl("comb", lowerSubject) & !grepl("other", lowerSubject) & 
       !grepl("any", lowerSubject) & !grepl("general", lowerSubject) & 
       !grepl("audio", lowerSubject) & !grepl("aerospace", lowerSubject) & 
       !grepl("civil", lowerSubject) & !grepl("mechanical", lowerSubject) & 
       !grepl("manufact", lowerSubject) & !grepl("chemical", lowerSubject))) {
    computer_subjects %<>% 
      append(subject)
  }
}

filters <- list(
  subjects = computer_subjects,
  applicationRoute = "'Firm choice'",
  domicile = "'Northern Ireland'")

ucas_data <- ucas_applicants %>%
  subset(`Subject Group (Detailed Level)` %in% filters$subject) %>%
  subset(`Acceptance Route` == filters$applicationRoute) %>%
  subset(`Applicant Domicile (High Level)` == filters$domicile)

hostEntrants <- ucas_hosts %>%
  subset(`Subject Group (Detailed Level)` %in% filters$subject) %>%
  subset(`Acceptance Route` == filters$applicationRoute) %>%
  subset(`Provider Country` == filters$domicile)

ucas_data$demandSupplyDiff <- ucas_data$`Number of Acceptances` - 
  hostEntrants$`Number of Acceptances`

ucas_data$vacanciesRecorded <- c(
  540, 505, 530, 560, 610, 635, 700, 775, 760, 715, 785, 745)

ucas_application_chart <- ggplot(
    data = ucas_data,
    aes(x = `Cycle Year`,
        y = `Number of Acceptances`,
        fill = `Subject Group (Detailed Level)`)) + 
  geom_area(
    colour = "black", 
    size = .2, 
    alpha = .4) +
  labs(title = paste0('UK university acceptances by all ', filters$applicationRoute, ' NI applicants (2007-2018)')) +
  ylab("Number of acceptances") + xlab("Year") +
  scale_fill_discrete(
    name = "Subject",
    labels = c("Electronic & Electrical Engineering", "Computer Science", "Software Engineering")) +
  theme_minimal()

ggsave(
  filename = getwd() %>% paste0("/analysis/images/ucas-applications.png"), 
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
  geom_point(aes(y = demandSupplyDiff * 2,
                 colour = `Subject Group (Detailed Level)`)) + 
  geom_line(aes(y = demandSupplyDiff * 2, 
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
