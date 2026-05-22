library(data.table)
library(ggplot2)
library(tidyverse)
library(maps)
library(sf)

rm(list = ls())
gc()

col_smoke<- c("#CCFFFF","#6EB6F1", "#699E1D", "#fff179","#EA8A39","#A3340D","#470F27")
usa_map <- map_data("state")

data_path <- "../Data"

grid_id <- read_sf(paste0(data_path, "/10km_grid_wgs84/10km_grid_wgs84.shp")) 

data <- readRDS(paste0(data_path, "/data_annual_smoke_o3.rds")) %>%
  left_join(grid_id)
  
quantile(data$annual_smoke_o3, seq(0, 1, 0.01))
data$annual_smoke_o3[data$annual_smoke_o3 < 0] <- 0

# plot 
p <- ggplot() +
  geom_sf(data = data %>% filter(LC_water < 50), 
          aes(geometry = geometry, fill = annual_smoke_o3),
          color = NA,  
          size = 0) +
  facet_wrap(~ year, ncol = 5) + 
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 8),
    strip.text = element_blank()
  ) + 
  scale_fill_gradientn(
    colors = col_smoke, 
    na.value = NA,
    oob = scales::squish,
    limits = c(0, 5),
    trans = "sqrt",
    name = "Annual average O3 (ppb)",
    guide = guide_colorbar(
      direction = "horizontal",
      nrow = 1,
      title.position = "top",
      barwidth = 10,
      barheight = 0.75,
      ticks.colour = "white",
      ticks.linewidth = 0.2
    )
  )


