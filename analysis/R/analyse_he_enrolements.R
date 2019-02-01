library(readr)
library(magrittr)


get_enrolments <- function(data, subject, level, mode, region, marker) {
  enrolment_data <- data %>% 
    subset(`Level of study` == level) %>%
    subset(`Subject of study` == subject) %>%
    subset(`Mode of study` == mode) %>%
    subset(`Domicile marker` == region) %>%
    subset(`First year marker` == marker)
  
  enrolment_data$`Academic Year` <- sapply(
    X = strsplit(
      x = enrolment_data$`Academic Year`, 
      split = "/"),
    FUN = `[`, 1)
  
  return(enrolment_data)
}

# he_student_enrolments_by_subject_area_all <- read_csv(
#   paste0(getwd(), "analysis/data/rdata/he-student-enrolments-by-subject-area-all.csv"))

load(file = getwd() %>% 
       paste0("/analysis/data/rdata/he_student_enrolments_by_subject_area_all.rda"))

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
  marker = "First year",
  region = "Northern Ireland")

he_application_chart <- ggplot(
  data = sub_enrolment_data,
  aes(
    x = `Academic Year`, 
    y = Number, 
    group = `Subject of study marker`,
    colour = `Subject of study marker`)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "bottom")

he_application_chart
