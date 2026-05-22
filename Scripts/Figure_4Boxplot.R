library(tidyverse)
library(data.table)
library(ggplot2)
library(geofacet)

rm(list = ls())
gc()

data_path <- "../Data"

# load data
data <- readRDS(paste0(data_path, "/data_so_spm_relation_byRegion.rds"))

max_lines <- data %>%
  filter(count > 100) %>%
  group_by(region) %>%
  summarise(
    fit_loess = {
      mod <- loess(middle ~ as.numeric(bin), data = cur_data(), span = 0.75)
      x_seq <- seq(min(as.numeric(bin)), max(as.numeric(bin)), length.out = 200)
      pred <- predict(mod, newdata = data.frame(bin = x_seq), type = "response")
      x_seq[which.max(pred)]
    },
    .groups = "drop"
  )

region_colors <- c(
  
  "West"                        = "#d73027", 
  "Southwest"                   = "#f46d43", 
  "Northwest"                   = "#fdae61", 
  "Northern Rockies" = "#fee090", 
  "South"                       = "#e6ab02",
  
  "Northeast"                   = "#313695",
  "Southeast"                   = "#1b9e77",
  "Upper Midwest"               = "#74add1",
  "Ohio Valley"                 = "#bebada",
  
  "Others"                      = "#d9d9d9"
)

# plot
p <- ggplot() +
  facet_wrap(~ region, scales = "free", ncol = 3) +  
  geom_boxplot(data = data %>% filter(count > 100),
               aes(x = bin, 
                   lower = lower, 
                   middle = middle, 
                   upper = upper,
                   ymin = ymin, 
                   ymax = ymax,
                   fill =region), 
               stat = "identity", 
               color = "black", width = 0.55) +
  scale_fill_manual(values = region_colors, name = "") +
  geom_smooth(data = data %>% filter(count > 100), 
              aes(x = as.numeric(bin), y = middle),
              method = "loess", se = FALSE, color = "red", size = 1.2) +
  geom_vline(data = max_lines, aes(xintercept = fit_loess),
             color = "blue", linetype = "dashed", linewidth = 1) +
  labs(x = expression("Smoke PM2.5 (" * mu * "g/m"^3 * ")"), 
       y = "Smoke O3 (ppb)") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(color = "black", size = 10),
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



