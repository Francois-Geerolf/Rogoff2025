rm(list = ls())
setwd(here::here())
library(tidyverse)
library(readxl)
library(eurostat)
library(scales)
library(ggpubr)
library(eurostat)

load("colors.RData")
load("geo.RData")

country_list <- c("Australia", "Canada", "UnitedStates", "France", 
                  "Italy", "Germany", "EuroArea", "Spain", "UnitedKingdom")


hzskal <- tibble(country = country_list) %>%
  mutate(data = map(seq(8, 184, 22), ~ read_excel("hzskal.xlsx", skip = ., n_max = 16))) %>%
  unnest %>%
  rename(date = ...1) %>%
  mutate(date = as.Date(date)) %>%
  mutate(date = zoo::as.yearqtr(paste(year(date), quarter(date)), format = "%Y %q"))


for (countryname in country_list){
  graphique1_line <- hzskal %>%
    filter(country == countryname) %>%
    select(-country) %>%
    gather(Line, values, -date) %>%
    filter(Line == "GDP deflator, in %")
  
  plot <- hzskal %>%
    filter(country == countryname) %>%
    select(-country) %>%
    gather(variable, values, -date) %>%
    filter(variable != "GDP deflator, in %") %>%
    ggplot(., aes(x = date, y = values/100)) +
    geom_col(aes(fill = variable), alpha = 1) +
    geom_line(data = graphique1_line, aes(linetype = Line), size = 1.2) +
    theme_minimal() + xlab("") + ylab("%") +
    # c("orange", "red", "blue", "darkgreen")
    scale_fill_manual(values = viridis(3)[3:1]) +
    zoo::scale_x_yearqtr(labels = date_format("%Y Q%q"),
                         breaks = expand.grid(2019:2022, c(1,2, 3, 4)) %>%
                           mutate(breaks = zoo::as.yearqtr(paste0(Var1, "Q", Var2))) %>%
                           pull(breaks)) +
    scale_y_continuous(breaks = 0.01*seq(-10, 30, 1),
                       labels = percent_format(accuracy = 1),
                       limits = 0.01*c(-6, 8)) +
    theme(legend.position = c(0.15, 0.8),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.key.size = unit(0.4, "cm"))
  
  assign(paste0("plot_", countryname), plot)
}



ggarrange(
  plot_France, plot_Germany, plot_Italy, plot_Spain, labels = c("France", "Germany", "Italy", "Spain"),
  common.legend = TRUE, legend = "bottom"
)

ggsave("pdf/figure4.pdf", width = 10, height = 6, device = cairo_pdf)
ggsave("png/figure4.png", width = 10, height = 6, bg = "white")



