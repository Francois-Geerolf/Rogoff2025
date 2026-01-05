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

datasets_eurostat <- c("nrg_pc_204")

if (!all(datasets_eurostat %in% ls())){
  for (dataset in datasets_eurostat){
    assign(dataset, 
           get_eurostat(dataset, stringsAsFactors = F, cache = F, time_format = "raw") |>
             rename(time = TIME_PERIOD)
    )
  }
}

## Low -----

plot1 <- nrg_pc_204 %>%
  filter(geo %in% c("FR", "DE", "EA", "ES", "IT"),
         nrg_cons == "KWH_LT1000",
         currency == "EUR",
         as.numeric(substr(time, 1, 4)) >= 2021,
         as.numeric(substr(time, 1, 4)) <= 2024) %>%
  select_if(~n_distinct(.) > 1) %>%
  left_join(geo, by = "geo") %>%
  mutate(Geo = ifelse(geo == "EA", "Euro Area", Geo)) %>%
  mutate(values = 1000*values) %>%
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
  theme(legend.position = c(0.2, 0.9),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_fill_manual(values = viridis(3)[1:3]) +
  scale_y_continuous(breaks = seq(0, 1000, 100),
                     labels = dollar_format(a = 1, pre = "", su = "€/MWh"),
                     limits = c(-30, 650))  +
  xlab("Semester") + ylab("") +
  ggtitle("Electricity price per MWh\nLow Consumption (< 1 MWh)")

## High -----

plot2 <- nrg_pc_204 %>%
  filter(geo %in% c("FR", "DE", "EA", "ES", "IT"),
         nrg_cons == "KWH_GE15000",
         currency == "EUR",
         as.numeric(substr(time, 1, 4)) >= 2021,
         as.numeric(substr(time, 1, 4)) <= 2024) %>%
  select_if(~n_distinct(.) > 1) %>%
  left_join(geo, by = "geo") %>%
  mutate(Geo = ifelse(geo == "EA", "Euro Area", Geo)) %>%
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
  scale_y_continuous(breaks = seq(0, 1000, 100),
                     labels = dollar_format(a = 1, pre = "", su = "€/MWh"),
                     limits = c(-30, 650))  +
  xlab("Semester") + ylab("") +
  ggtitle("\nHigh Consumption (> 15 MWh)")

ggarrange(
  plot1, plot2, labels = c("", ""),
  common.legend = TRUE, legend = "bottom"
)


ggsave("pdf/figure2.pdf", width = 10, height = 6, device = cairo_pdf)
ggsave("png/figure2.png", width = 10, height = 6, bg = "white")



