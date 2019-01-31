library(readr)
library(magrittr)


ROOT_DIRECTORY <- "~/Code/DigitalStrategyNI/"

# he_student_enrolments_by_subject_area_all <- read_csv(
#   paste0(ROOT_DIRECTORY, "analysis/data/rdata/he-student-enrolments-by-subject-area-all.csv"))

load(
  file = paste0(
    ROOT_DIRECTORY, 
    "analysis/data/rdata/he_student_enrolments_by_subject_area_all.rda"))

he_student_enrolments_by_subject_area_all$`Subject of study` <- sapply(
  X = strsplit(
    x = he_student_enrolments_by_subject_area_all$`Subject of study`, 
    split = ") "), 
  FUN = `[`, 2)

sub_enrolment_data <- get_enrolments(
  data = he_student_enrolments_by_subject_area_all,
  subject = "Computer science",
  level = "All undergraduate",
  mode = "All",
  region = "Northern Ireland")

get_enrolments <- function(data, subject, level, mode, region) {
  enrolment_data <- data %>% 
    subset(`Level of study`==level) %>%
    subset(`Subject of study`==subject) %>%
    subset(`Mode of study`==mode) %>%
    subset(`Domicile marker`==region)
  
  enrolment_data$`Academic Year` <- sapply(
    X = strsplit(
      x = enrolment_data$`Academic Year`, 
      split = "/"),
    FUN = `[`, 1)
  
  return(enrolment_data)
}
