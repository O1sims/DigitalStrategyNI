library(plyr)
library(ggplot2)
library(ggthemes)
library(jsonlite)
library(magrittr)



LONDON_WAGES <- c(
  42155, 60000, 50000, 24000, 24000, 80000, 45000, 35000, 35000, 110000, 
  40000, 40000, 42155, 56526, 60000, 75000, 50000, 34000, 65000, 65000, 
  45000, 70000, 26000, 26000, 75000, 43000, 95000, 28000, 45000, 120000, 
  60000, 42000, 71074, 70000, 56526, 75000, 42155, 35000, 65000, 110000, 
  65000, 70000, 50000, 95000, 60000, 34189, 50000, 25000, 55000, 71074, 
  56266, 80000, 40000, 70000, 35000, 35000, 44000, 19500, 50000, 120000, 
  55000, 75000, 28500, 33945, 40000, 40000, 65000, 75000, 70000, 85000, 
  35000, 67000, 60000) # Collected from HAYS London

vanrath.data <- "/home/owen/Code/DigitalStrategyNI/analysis/data/jobSpecs.json" %>%
  jsonlite::read_json()

minWages <- maxWages <- 
  languages <- technologies <- c()
for (role in vanrath.data) {
  if (length(role$languages) > 0) {
    languages %<>% append(role$languages)
    technologies %<>% append(role$technologies)
  }
  if (length(role$wage) > 1 && 
      role$contractType == "Permanent") {
    minWages %<>% append(role$wage[[1]])
    maxWages %<>% append(role$wage[[2]])
  } else if (length(role$wage) > 0 && 
             role$contractType == "Permanent") {
    maxWages %<>% append(role$wage[[1]])
  }
}

minRange <- "Minimum" %>% 
  rep(length(minWages))

maxRange <- "Maximum" %>% 
  rep(length(c(maxWages, LONDON_WAGES)))

data.frame(
  wage = c(maxWages, LONDON_WAGES),
  type = maxRange, 
  location = c(
    "Belfast" %>%
      rep(length(maxWages)),
    "London" %>%
      rep(length(LONDON_WAGES))),
  stringsAsFactors = FALSE) %>% 
  subset(wage > 1000) %>%
  ggplot() + 
  geom_density(
    mapping = aes(
      x = wage, 
      fill = location), 
    alpha = 0.5, 
    size = 0.3) +
  xlab("Salary (£)") + 
  ylab("Density") + 
  labs(
    title = "Salary distribution for permanent IT professionals, April 2019",
    subtitle = "Salaries scraped from Belfast & London-based recruiter websites") +
  theme_minimal() + 
  ggthemes::scale_fill_ptol("") +
  theme(
    legend.position = "bottom")

languages %<>% 
  purrr::flatten_chr()

technologies %<>% 
  purrr::flatten_chr()

languageTable <- languages %>% 
  table() %>% 
  as.data.frame() %>%
  subset(Freq > 10)

technologiesTable <- technologies %>% 
  table() %>% 
  as.data.frame()

languageTable %>%
  ggplot() + 
  geom_bar(
    mapping = aes(
      y = Freq,
      x = .), 
    stat = "identity",
    fill = "red",
    colour = "black",
    size = 0.3,
    alpha = 0.5) + 
  xlab("") +
  ylab("Count") + 
  labs(
    title = "Number of IT vacancies by programming language, April 2019") +
  theme_minimal() + 
  ggthemes::scale_fill_ptol() +
  theme(
    legend.position = "none")

technologiesTable %>%
  ggplot() + 
  geom_bar(
    mapping = aes(
      y = Freq,
      x = .), 
    stat = "identity",
    fill = "blue",
    colour = "black",
    size = 0.3,
    alpha = 0.5) + 
  xlab("") +
  ylab("Count") + 
  labs(
    title = "Number of IT vacancies by technology, April 2019") +
  theme_minimal() + 
  ggthemes::scale_fill_ptol() +
  theme(
    legend.position = "none")


uniqueLanguages <- languageTable$.

langMax <- language <- c()
for (lang in uniqueLanguages) {
  for (role in vanrath.data) {
    if (lang %in% role$languages && 
        length(role$wage) > 1 && 
        role$contractType == "Permanent") {
      langMax %<>% append(role$wage[[2]])
      language %<>% append(lang)
    } else if (lang %in% role$languages && 
               length(role$wage) > 0 && 
               role$contractType == "Permanent") {
      langMax %<>% append(role$wage[[1]])
      language %<>% append(lang)
    }
  }
}


language.df <- data.frame(
  langMax, language, 
  stringsAsFactors = FALSE) %>% 
  subset(langMax > 1000) 

stripped.language.df <- language.df[0, ]
for (t in language.df$language %>% unique()) {
  language.data <- language.df %>% 
    subset(language == t)
  language.data$mean <- faveMean <- as.integer(language.data$langMax %>% 
         mean())
  stripped.language.df %<>% 
    rbind(language.data)
}

mean <- stripped.language.df$mean
data <- stripped.language.df[order(-mean), ]
stripped.language.df$language %<>% factor(
  levels = data$language %>% unique())

stripped.language.df %>%
  ggplot() + 
  geom_violin(
    mapping = aes(
      x = language,
      y = langMax, 
      fill = language), 
    alpha = 0.5, 
    size = 0.3) + 
  xlab("") +
  ylab("Salary (£)") + 
  labs(
    title = "NI salary distribution for programming languages & technologies, April 2019") +
  theme_minimal() + 
  ggthemes::scale_fill_ptol() +
  theme(
    legend.position = "none")


# Network graph of languages

source <- target <- c()
for (role in vanrath.data) {
  if (length(role$languages) > 1) {
    combinations <- role$languages %>% 
      combn(2)
    source %<>% 
      append(combinations[1, ])
    target %<>% 
      append(combinations[2, ])
  }
}

network.df <- data.frame(
  source = source %>% 
    purrr::flatten_chr(),
  target = target %>% 
    purrr::flatten_chr(),
  stringsAsFactors = FALSE)

network.df %<>%
  plyr::ddply(.(source, target), nrow)

G <- igraph::graph_from_data_frame(
  d = network.df, 
  vertices = network.df[, 1:2] %>% 
    purrr::flatten_chr() %>% 
    unique(), 
  directed = FALSE)

layout <- G %>% 
  layout.reingold.tilford(
    circular = TRUE)

G %>% 
  plot(
    layout = layout,
    edge.width = network.df$V1 %>% 
      log())




# Check is degrees are needed

degrees <- 0
for (role in vanrath.data) {
  if (agrepl("years experience", role$details$description, ignore.case = TRUE)) {
    degrees <- degrees + 1
  }
}
