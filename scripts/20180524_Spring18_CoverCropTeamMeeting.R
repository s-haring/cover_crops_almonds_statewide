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
ShannonMatrix <- WeedCommunityMatrix %>%
  select(-Month, -Location, -Rep, -Treatment) # create a new dataframe with just the species counts, as needed for diversity() 
shannon <- diversity(ShannonMatrix, index = "shannon") # calculate Shannon H into a new vector
WeedCommunityMatrix <- WeedCommunityMatrix %>%
  mutate(H = shannon) # add that vector back into original dataframe
rm(shannon, ShannonMatrix) # clean up the leftover objects

# Make the figure
library(ggthemes)
ggplot(WeedCommunityMatrix, aes(x = Treatment, y = H)) + # set simple axes (treatment by diversity)
  geom_boxplot(size = 0.8) + # boxplot with slightly thicker lines
  facet_wrap("Location") + #include each location in a separate panel
  labs(title = "Shannon Diversity of Weeds") + # figure title
  theme_few(base_size = 18) + # few theme
  theme(axis.text.x = element_text(angle =90, hjust = 1)) # rotate and align x axis titles
ggsave("output/20180524_WeedSpeciesRichness.wmf")
ggsave("output/20180524_WeedSpeciesRichness.svg")
ggsave("output/20180524_WeedSpeciesRichness.jpeg", dpi = 600)
