library(ggplot2)
library(sf)
library(tidyverse)
library(sf)
library(data.table)

rm(list = ls())
gc()

data_path <- "../Data"

# load data
map <- read_sf(paste0(data_path, "/tl_2021_us_state/tl_2021_us_state.shp"))

# classify regions
map <- map %>%
  mutate(region_9 = case_when(
    # 1. Northwest
    STUSPS %in% c("WA", "OR", "ID") ~ "Northwest",
    
    # 2. West
    STUSPS %in% c("CA", "NV") ~ "West",
    
    # 3. Southwest
    STUSPS %in% c("AZ", "CO", "NM", "UT") ~ "Southwest",
    
    # 4. Northern Rockies
    STUSPS %in% c("MT", "ND", "SD", "NE", "WY") ~ "Northern Rockies",
    
    # 5. Upper Midwest
    STUSPS %in% c("IA", "MI", "MN", "WI") ~ "Upper Midwest",
    
    # 6. South
    STUSPS %in% c("AR", "KS", "LA", "MS", "OK", "TX") ~ "South",
    
    # 7. Ohio Valley
    STUSPS %in% c("IL", "IN", "KY", "MO", "OH", "TN", "WV") ~ "Ohio Valley",
    
    # 8. Southeast
    STUSPS %in% c("AL", "FL", "GA", "NC", "SC", "VA") ~ "Southeast",
    
    # 9. Northeast
    STUSPS %in% c("CT", "DE", "ME", "MD", "MA", "NH", "NJ", "NY", "PA", "RI", "VT") ~ "Northeast",
    
    # others
    TRUE ~ "Others"
  ))

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


p <- ggplot(map %>% filter(region_9 != "Others")) +

  geom_sf(aes(fill = region_9), color = "white", size = 0.1) +

  scale_fill_manual(values = region_colors, name = "") +

  theme_void() +

  theme(
    legend.position = "bottom",        
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 10))
  ) 


