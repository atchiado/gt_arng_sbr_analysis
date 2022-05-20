library(tidyverse)
library(ggdist)
library(gghalves)
library(ggnewscale)
library(maps)
library(gridExtra)


# Load Data ------------------
sbr <- read.csv("~/Desktop/work/data/r/gt_arng_sbr_analysis/arng_sb_data.csv")
long_lat <- map_data("state")


# Merge Data --------------------
merged_data <- inner_join(sbr, long_lat, by = c("State" = "region"))


# Data Viz --------------------
# Create a density plot
ggplot(sbr, aes(SB_Recruiter, Total, fill = factor(SB_Recruiter), color = factor(SB_Recruiter))) + 
  stat_halfeye(adjust = .5, width = .6, .width = 0, justification = -.3, point_color = NA) + 
  geom_point(size = 1.3, alpha = .5, position = position_jitter(seed = 1, width = .1)) +
  geom_boxplot(width = .25, outlier.shape = NA, alpha = 0.1) +
  labs(title = "ARNG 56A Accessions, FY17-FY21",
       subtitle = "Distribution of accessions for states with and without Specialty Branch Recruiters.", 
       x = "Specialty Branch Recruiter", y = "Accessions") +
  ylim(0, 50) +
  coord_cartesian(xlim = c(1.2, NA), clip = "off") +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  coord_flip() +
  theme(plot.margin = margin(1,1.5,0.5,1, "cm"),
        plot.background = element_rect(fill = "grey98"))

# Create a choropleth map grouped by sb recruiter status
ggplot(merged_data, aes(x=long, y= lat, group = group)) +
  geom_polygon(aes_string(fill= "SB_Total"), size = 0.2) +
  scale_fill_gradient(low = "#fdfeea", high = "#ffe737", na.value = "white", limits = c(0, 50)) +
  new_scale_fill() + 
  geom_polygon(aes_string(fill= "No_SB_Total"), size = 0.2) +
  scale_fill_gradient(low = "#f5f5f5", high = "#070707", na.value = "transparent", limits = c(0, 50)) +
  theme_void() +
  theme(text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "grey98", color = NA),
        panel.background = element_rect(fill = "grey98", color = NA),
        legend.background = element_rect(fill = "grey98", color = NA))


# Significance Testing ------------------
sbr_t_test <- t.test(Total ~ SB_Recruiter, data = sbr, var.equal = FALSE)
sbr_t_test
