library(tidyverse)
library(data.table)
library(ggplot2)
library(patchwork)

rm(list = ls())
gc()

data_path <- "../Data"

data <- readRDS(paste0(data_path, "/data_NY_2023_case_study.rds"))

# prepare data
data_decompose <- data %>%
  mutate(
    smoke_o3 = o3_total - o3_counterfactual,
    mete_o3 = o3_counterfactual - o3_base
    )

data_b <- data_decompose %>%
  select(date, o3_total, o3_counterfactual) %>%
  pivot_longer(cols = c(o3_total, o3_counterfactual),
               names_to = "component",
               values_to = "value") %>%
  mutate(component = factor(component, 
                            levels = c("o3_total", "o3_counterfactual")))

data_c <- data_decompose %>%
  select(date, mete_o3, smoke_o3) %>%
  pivot_longer(cols = c(mete_o3, smoke_o3),
               names_to = "component",
               values_to = "value") %>%
  mutate(component = factor(component, 
                            levels = c("mete_o3", "smoke_o3")))

# plot
p1 <- ggplot(data_b, aes(x = date, y = value, color = component, linetype = component)) +  
  geom_line(linewidth = 0.8, alpha = 1) +
  scale_color_manual(values = c(
    "o3_counterfactual" = "#A6A6A6",    
    "o3_total" = "black"      
  ), labels = c(
    "o3_counterfactual" = expression("Smoke met"),
    "o3_total" = expression("Non-smoke counterfactual")
  )) +
  scale_linetype_manual(values = c(      
    "o3_counterfactual" = "dashed",
    "o3_total" = "solid"
  )) +
  guides(linetype = "none") +
  labs(x = "", 
       y = expression(O[3] ~ (ppb)),
       color = "") +
  theme_classic() +
  theme(
    axis.title = element_text(color = "black", size = 10),
    axis.text = element_text(color = "black", size = 10),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.text = element_text(color = "black", size = 10),
    legend.position = "top",
    axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", size = 10)
  ) +
  scale_x_date(date_labels = "%m-%d", date_breaks = "2 weeks", expand = c(0,0))


p2 <- ggplot(data_c, aes(x = date, y = value, color = component)) +
  geom_hline(size = 0.7, yintercept = 0, col="grey", linetype = "dashed") + 
  geom_line(linewidth = 0.8, alpha = 1) +
  scale_color_manual(values = c(
    "smoke_o3" = "#E386B9",    
    "mete_o3" = "#8BC24F"  
  ), labels = c(
    "smoke_o3" = expression("Smoke-driven"),
    "mete_o3" = expression("Meteorology-driven")
  )) +
  labs(x = "", 
       y = expression(O[3] ~ (ppb)),
       color = "") +
  theme_classic() +
  theme(
    axis.title = element_text(color = "black", size = 10),
    axis.text = element_text(color = "black", size = 10),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.text = element_text(color = "black", size = 10),
    legend.position = "top",
    axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", size = 10)
  ) +
  scale_x_date(date_labels = "%m-%d", date_breaks = "2 weeks", expand = c(0,0))

pp <- p1/p2


