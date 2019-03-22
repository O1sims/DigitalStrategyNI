library(readr)
library(ggplot2)
library(ggthemes)
library(magrittr)


get_enrolments <- function(data, subjects, level, mode, region, marker, subjectAreas) {
  enrolment_data <- data %>% 
    subset(`Level of study` == level) %>%
    subset(`Subject of study` %in% subjects) %>%
    subset(`Mode of study` == mode) %>%
    subset(`Domicile marker` == region) %>%
    subset(`First year marker` == marker) %>%
    subset(`Subject of study marker` %in% subjectAreas)
  
  enrolment_data$`Academic Year` <- sapply(
    X = strsplit(
      x = enrolment_data$`Academic Year`, 
      split = "/"),
    FUN = `[`, 1)
  
  return(enrolment_data)
}


# he_student_enrolments_by_subject_area_all <- read_csv(
#   paste0(getwd(), "analysis/data/rdata/he-student-enrolments-by-subject-area-all.csv"))

getwd() %>% 
  paste0("/analysis/data/rdata/he_student_enrolments_by_subject_area_all.rda") %>%
  load()

he_student_enrolments_by_subject_area_all$`Subject of study` <- sapply(
  X = strsplit(
    x = he_student_enrolments_by_subject_area_all$`Subject of study`, 
    split = ") "), 
  FUN = `[`, 2) %>% 
  tolower()

sub_enrolment_data <- get_enrolments(
  data = he_student_enrolments_by_subject_area_all,
  subjects = c(
    "computer science", 
    "engineering & technology", 
    "software engineering"),
  level = "All postgraduate",
  mode = "All",
  marker = "First year",
  region = "Northern Ireland",
  subjectAreas = c(
    "Subject area", 
    "Principal subject", 
    "4-digit JACS subject"))


years <- seq(
  from = sub_enrolment_data$`Academic Year` %>% min(),
  to = sub_enrolment_data$`Academic Year` %>% max())

subject <- year <- number <- c()

for (y in years) {
  number %<>% append(
    sum(sub_enrolment_data %>%
          subset(`Academic Year` == y) %$% 
          Number))
  subject %<>% append("all")
  year %<>% append(y)
}

for (s in c("computer science", 
            "engineering & technology", 
            "software engineering")) {
  for (y in years) {
    number %<>% append(
      sum(sub_enrolment_data %>%
            subset(`Subject of study` == s) %>%
            subset(`Academic Year` == y) %$% 
            Number))
    subject %<>% append(s)
    year %<>% append(y)
  }
}

data.frame(
  number = number,
  subject = subject,
  year = year) %>%
  ggplot() +
  geom_point(aes(
    x = year, 
    y = number, 
    colour = subject)) +
  geom_line(aes(
    x = year, 
    y = number, 
    colour = subject)) +
  ylab("Number of Students") + 
  xlab("Academic Year") +
  labs(title = "Number of students attending posgraduate courses in technical subjects in NI") + 
  scale_colour_discrete(
    name = "Subject") +
  theme_minimal() +
  theme(legend.position = "right")
