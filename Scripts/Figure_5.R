library(ggplot2)
library(tidyverse)
library(patchwork)

rm(list = ls())
gc()

data_path <- "../Data"

race_colors <- c(
  "Asian"   = "#1F78B4",   
  "Black"   = "#33A02C",  
  "Hispanic"= "#E31A23",   
  "White"   = "#FF7F00",   
  "Other/Unknown" = "#6A3D9A"  
)

# load data
data <- readRDS(paste0(data_path, "/data_dispirity.rds"))

# plot individual figure
p0 <- ggplot(data %>% filter(location == "Average"), 
             aes(x = income_decile*10, y = concentration, col = race_ethnicity)) +
  facet_wrap(~ type, ncol = 1, scales = "free") +
  geom_line() +
  labs(x = "Income (%)", y = "Exposure") +            
  theme_classic() +
  scale_color_manual(values = race_colors) +
  theme(plot.title = element_blank(),
        strip.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

p1 <- ggplot(data %>% filter(location == "Western US"), 
             aes(x = income_decile*10, y = concentration, col = race_ethnicity)) +
  facet_wrap(~ type, ncol = 1, scales = "free") +
  geom_line() +
  labs(x = "Income (%)", y = "Exposure") +
  theme_classic() +
  scale_color_manual(values = race_colors) +
  theme(plot.title = element_blank(),
        strip.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

p2 <- ggplot(data %>% filter(location == "Eastern US"), 
             aes(x = income_decile*10, y = concentration, col = race_ethnicity)) +
  facet_wrap(~ type, ncol = 1, scales = "free") +
  geom_line() +
  labs(x = "Income (%)", y = "Exposure") +
  theme_classic() +
  scale_color_manual(values = race_colors) +
  theme(plot.title = element_blank(),
        strip.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

# bind all figure
ppp <- (p0 + p1 + p2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

ppp

