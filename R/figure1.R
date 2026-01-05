setwd(here::here())
library(dplyr)
library(eurostat)
library(ggplot2)
library(scales)
library(viridis)


load("colors.RData")
load("geo.RData")


# fig-DE-FR-ES-EA19 ------------

datasets_eurostat <- c("prc_hicp_manr", "prc_hicp_inw")

if (!all(datasets_eurostat %in% ls())){
  for (dataset in datasets_eurostat){
    assign(dataset, 
           get_eurostat(dataset, stringsAsFactors = F, cache = F) |>
             rename(date = TIME_PERIOD)
    )
  }
}

ctry <- c("DE", "EA", "FR", "ES", "IT", "NL")

start_date <- as.Date("2021-01-01")
end_date <- as.Date("2024-01-01")

# graphique3 ------------

# Load packages
# Weights
weights <- prc_hicp_inw %>%
  rename(time = date) %>%
  filter(geo %in% ctry,
         coicop %in% c("FOOD", "NRG", "IGD_NNRG", "SERV")) %>%
  left_join(geo, by = "geo") %>%
  # Generate year column for merge with monthly index-growth data
  mutate(year = substring(time, 1, 4)) %>%
  select(-time) %>%
  rename(weight = values)


index <- prc_hicp_manr %>%
  rename(time = date) %>%
  filter(geo %in% ctry,
         coicop %in% c("CP00", "TOT_X_NRG_FOOD", "FOOD", "NRG", "IGD_NNRG", "SERV")) %>%
  filter(time >= start_date,
         time <= end_date,
         !is.na(values)) %>%
  # Prepare percentage data for ggplot
  mutate(values = values / 100) %>%
  left_join(geo, by = "geo") %>%
  mutate(Geo = ifelse(geo == "EA", "Euro area", Geo))


comp <- index %>%
  # Drop series, which are not used for decomposition
  filter(!coicop %in% c("CP00", "TOT_X_NRG_FOOD")) %>%
  # Gernate year column for merge with weights
  mutate(year = substring(time, 1, 4)) %>%
  # Merge with weights
  left_join(weights, by = c("year", "geo", "coicop")) %>%
  # Obtain values for composition
  group_by(time, geo) %>%
  mutate(weight = weight / sum(weight),
         values = values * weight) %>%
  ungroup() %>%
  # Last formatting for graph
  mutate(var_en = factor(coicop, levels = c("SERV", "IGD_NNRG", "FOOD", "NRG"),
                         labels = c("Services", "Manufactured Goods",
                                    "Food", "Energy"))) %>%
  left_join(geo, by = "geo") %>%
  mutate(Geo = ifelse(geo == "EA", "Euro area", Geo))

line <- index %>%
  # Filter for series of the line plots
  filter(coicop %in% c("CP00", "TOT_X_NRG_FOOD"),
         !is.na(values)) %>%
  # Last formatting for graph
  mutate(line_en = factor(coicop, levels = c("CP00", "TOT_X_NRG_FOOD"),
                          labels = c("HICP Inflation", "Core Inflation")))

ggplot(comp, aes(x = time, y = values)) +
  # Add bars to plot the decomposition
  geom_col(aes(fill = var_en), alpha = 1) +
  # Add leine for headline and core inflation
  geom_line(data = line, aes(linetype = line_en), size = 1.2) +
  # Finetune x-axis design
  scale_x_date(breaks = seq.Date(as.Date("2020-01-01"), Sys.Date(), "3 months"),
               labels = date_format("%b %Y")) +
  # Finetune y-axis design
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  # Facetting, since we look at more than one country
  facet_wrap(~ Geo, ncol = 3) +
  #facet_wrap(~ Geo, ncol = 3, scales = "free") +
  # Finetune legend design
  guides(fill = guide_legend(ncol = 4),
         linetype = guide_legend(ncol = 2, keywidth = 1.5)) +
  # Labels
  labs(title = "", 
       subtitle = "",
       caption = "",
       x = "", y = "") + 
  scale_fill_manual(values = viridis(4)[1:4]) +
  theme_minimal() +
  # Last finetuning
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 



ggsave("pdf/figure1.pdf", width = 10, height = 6, device = cairo_pdf)
ggsave("png/figure1.png", width = 10, height = 6, bg = "white")




