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

## Load Eurostat datasets if not already loaded ------

datasets_eurostat <- c("nrg_pc_204", "nrg_pc_202")

if (!all(datasets_eurostat %in% ls())){
  for (dataset in datasets_eurostat){
    assign(dataset, 
           get_eurostat(dataset, stringsAsFactors = F, cache = F, time_format = "raw") |>
             rename(time = TIME_PERIOD)
    )
  }
}

plot3 <- nrg_pc_202 %>%
  filter(geo %in% c("FR", "DE", "EA", "ES", "IT"),
         nrg_cons == "GJ_LT20",
         currency == "EUR",
         unit == "KWH",
         as.numeric(substr(time, 1, 4)) >= 2021,
         as.numeric(substr(time, 1, 4)) <= 2024) %>%
  select_if(~n_distinct(.) > 1) %>%
  left_join(geo, by = "geo") %>%
  mutate(Geo = ifelse(geo == "EA", "Euro area", Geo)) %>%
  mutate(values = values*1000) %>%
  spread(tax, values) %>%
  transmute(time, Geo,
            ` Taxes except VAT` = X_VAT-X_TAX,
            `  VAT` = I_TAX-X_VAT,
            `Excluding taxes` = X_TAX) %>%
  gather(Tax, values, - time, -Geo) %>%
  ggplot(., aes(x = time, y = values, fill = Tax)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Geo) + theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_fill_manual(values = viridis(3)[1:3]) +
  scale_y_continuous(breaks = seq(0, 1000, 50),
                     labels = dollar_format(a = 1, pre = "", su = "€/MWh"),
                     limits = c(-30, 250))  +
  xlab("Semester") + ylab("") +
  ggtitle("Natural gas price per MWh\nLow Consumption (< 20 GJ, < 5.5 MWh)")

plot3

## High -----

plot4 <- nrg_pc_202 %>%
  filter(geo %in% c("FR", "DE", "EA", "ES", "IT"),
         nrg_cons == "GJ_GE200",
         currency == "EUR",
         unit == "KWH",
         as.numeric(substr(time, 1, 4)) >= 2021,
         as.numeric(substr(time, 1, 4)) <= 2024) %>%
  select_if(~n_distinct(.) > 1) %>%
  left_join(geo, by = "geo") %>%
  mutate(Geo = ifelse(geo == "EA", "Europe", Geo)) %>%
  mutate(values = values*1000) %>%
  spread(tax, values) %>%
  transmute(time, Geo,
            ` Taxes except VAT` = X_VAT-X_TAX,
            `  VAT` = I_TAX-X_VAT,
            `Excluding taxes` = X_TAX) %>%
  gather(Tax, values, - time, -Geo) %>%
  ggplot(., aes(x = time, y = values, fill = Tax)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Geo) + theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_fill_manual(values = viridis(3)[1:3]) +
  scale_y_continuous(breaks = seq(0, 1000, 50),
                     labels = dollar_format(a = 1, pre = "", su = "€/MWh"),
                     limits = c(-30, 250))  +
  xlab("Semester") + ylab("") +
  ggtitle("\nHigh Consumption  (> 200 GJ, > 55.5 MWh)")

plot4

ggarrange(
  plot3, plot4, labels = c("", ""),
  common.legend = TRUE, legend = "bottom"
)

ggsave("pdf/figure3.pdf", width = 10, height = 6, device = cairo_pdf)
ggsave("png/figure3.png", width = 10, height = 6, bg = "white")
