library(tidyverse)
library(ggdist)
library(gghalves)


# Load Data ------------------
sbr <- read.csv("~/Desktop/work/data/r/gt_arng_sbr_analysis/arng_sb_data.csv")


# Data Viz --------------------
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
  coord_flip()


# Significance Testing ------------------
sbr_t_test <- t.test(Total ~ SB_Recruiter, data = sbr, var.equal = FALSE)
sbr_t_test
