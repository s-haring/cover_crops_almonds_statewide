# Workbook for figures for first cover crop team meeting (May 2018)
# Second try
# SCH May 24, 2018

# Import data -------------------------------------------------------------

library(tidyverse)
WeedSurveys <- read_csv("data/20180518_CoverCropWeedSurveys.csv")
str(WeedSurveys)

# Stacked bar charts showing total weed cover, bare ground, and cover crop by species for each sampling timing --------





# Weed species diversity figure -------------------------------------------

# Revert tidy data into a "community matrix" for each plot
WeedCommunityMatrix <- WeedSurveys %>%
  filter(Zone != "Strip", TermTiming != "Early") %>% # only focused on alley weeds and the main termination plots
  spread(key = Species, value = Intercepts, fill = 0) %>% # put each species into its own column, fill cells with count of observations or 0
  select(-NONE, -SINAL, -VICVI, -LOLMU, -TRFRE, -RAPSR, -BRSNN) %>% # remove cover crops and bareground columns
  select(-TermTiming, -Zone, -Year, -Day) # remove unneeded/redundant columns

# Calculate Shannon Diversity for each plot
library(vegan)
shannon <- diversity(WeedCommunityMatrix, index = "shannon", )
?diversity
mutate( )
