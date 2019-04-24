library(plyr)
library(ggplot2)
library(ggthemes)
library(jsonlite)
library(magrittr)



vanrath.data <- "/home/owen/Code/DigitalStrategyNI/analysis/data/jobSpecs.json" %>%
  jsonlite::read_json()

minWages <- maxWages <- languages <- c()
for (role in vanrath.data) {
  if (length(role$languages) > 0) {
    languages %<>% append(role$languages)
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
  rep(length(maxWages))

data.frame(
  wage = maxWages  ,
  type = maxRange, 
  stringsAsFactors = FALSE) %>% 
  subset(wage > 1000) %>%
  ggplot() + 
  geom_density(
    mapping = aes(
      x = wage, 
      fill = type), 
    alpha = 0.5, 
    size = 0.3) + 
  xlab("Salary (£)") + 
  ylab("Density") + 
  labs(
    title = "Salary distribution for permanent IT professionals, April 2019",
    subtitle = "Wage analysis of 521 jobs") +
  theme_minimal() + 
  ggthemes::scale_fill_ptol() +
  theme(
    legend.position = "none")


languages %<>% 
  purrr::flatten_chr()

languageTable <- languages %>% 
  table() %>% 
  as.data.frame() %>%
  subset(Freq > 30)

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
    title = "Number of IT vacancies by programming language & technology, April 2019") +
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

