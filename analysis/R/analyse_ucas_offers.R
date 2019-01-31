library(readr)
library(ggplot2)


ROOT_DIRECTORY <- "~/Code/DigitalStrategyNI/"


# ucas_hosts <- read_csv(file = paste0(ROOT_DIRECTORY, "data/raw-data/ucas-acceptances-by-country-subject-host.csv"))
# ucas_applicants <- read_csv(file = paste0(ROOT_DIRECTORY, "data/raw-data/ucas-acceptances-by-country-subject-applicant.csv"))
# save(ucas_hosts,
#      file = paste0(ROOT_DIRECTORY, "data/R/RData/ucas_hosts.rda"))
# save(ucas_applicants,
#      file = paste0(ROOT_DIRECTORY, "data/R/RData/ucas_applicants.rda"))

load(file = paste0(ROOT_DIRECTORY, "analysis/data/rdata/ucas_hosts.rda"))
load(file = paste0(ROOT_DIRECTORY, "analysis/data/rdata/ucas_applicants.rda"))

computer_subjects <- c()
for (subject in ucas_applicants$`Subject Group (Detailed Level)` %>% unique()) {
  if (grepl("compu", tolower(subject))) {
    computer_subjects %<>% append(subject)
  }
}

ucas_applicants_computer <- ucas_applicants %>%
  subset(`Subject Group (Detailed Level)` == "'I1 - Computer Science'") %>%
  subset(`Acceptance Route` == "'Firm choice'")
