library(tidyverse)
library(data.table)
library(ggplot2)
library(geofacet)
library(slider)

rm(list = ls())
gc()

data_path <- "../Data"

# load data
data <- readRDS(sprintf("%s/data_EPA_4thMax_series_byState.rds", data_path)) 

# process data
data_long_diff <- data %>%
  pivot_longer(
    cols = c("total_o3", "non_smoke_o3"),
    names_to = "type",
    values_to = "o3_value"
  ) %>%
  pivot_wider(
    id_cols = c(state, year),
    names_from = type,
    values_from = o3_value
  ) %>%
  mutate(
    ymin = non_smoke_o3,
    ymax = total_o3
  )

# plot
p1 <- ggplot(data_long_diff %>% filter(state != "Contiguous US"), aes(x = year)) +
  
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "grey", alpha = 0.7) +
  
  geom_line(aes(y = total_o3, color = "Total"), linewidth = 0.5) +
  geom_line(aes(y = non_smoke_o3, color = "Non-smoke"), linewidth = 0.5) +
  
  facet_geo(~ state, grid = "us_state_grid1", scales = "free_y") +
  
  labs(
    x = "Year",
    y = "Annual 4th Maximum MDA8 O3 (ppb)",
    color = "",
    fill = "Smoke contribution"
  ) +
  
  scale_color_manual(
    values = c(
      "Total" = "#E41A1C",
      "Non-smoke" = "black"
    )
  ) +
  
  scale_x_continuous(
    limits = c(2006, 2023),
    breaks = seq(2006, 2023, 17),
    expand = c(0, 0)
  ) +
  
  theme_bw() +
  theme(
    text = element_text(color = "black", size = 12),  
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    strip.text = element_text(color = "black", margin = margin(b = 2)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none",
    legend.key = element_blank(),               
    legend.spacing.x = unit(0.3, "cm"),         
    panel.spacing = unit(1, "lines")            
  )

p2 <- ggplot(data_long_diff %>% filter(state == "Contiguous US"), aes(x = year)) +
  
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "grey", alpha = 0.7) +
  
  geom_line(aes(y = total_o3, color = "Total"), linewidth = 0.5) +
  geom_line(aes(y = non_smoke_o3, color = "Non-smoke"), linewidth = 0.5) +
  
  labs(
    x = "Year",
    y = "Annual 4th Maximum MDA8 O3 (ppb)",
    color = "",
    fill = "Smoke contribution"
  ) +
  
  scale_color_manual(
    values = c(
      "Total" = "#E41A1C",
      "Non-smoke" = "black"
    )
  ) +
  
  scale_x_continuous(
    limits = c(2006, 2023),
    breaks = seq(2006, 2023, 17),
    expand = c(0, 0)
  ) +
  
  theme_bw() +
  theme(
    text = element_text(color = "black", size = 12),  
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    strip.text = element_text(color = "black", margin = margin(b = 2)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none",
    legend.key = element_blank(),               
    legend.spacing.x = unit(0.3, "cm"),         
    panel.spacing = unit(1, "lines")            
  )
