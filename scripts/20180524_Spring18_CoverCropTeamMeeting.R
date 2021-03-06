# Workbook for figures for first cover crop team meeting (May 2018)
# Second try
# SCH May 24, 2018

# Import data -------------------------------------------------------------

library(tidyverse)
WeedSurveys <- read_csv("data/20180518_CoverCropWeedSurveys.csv")
str(WeedSurveys)

# Stacked bar charts showing total weed cover, bare ground, and cover crop by species for each sampling timing --------

# Group data: % weed coverage, % bare ground, % each cover crop species
CoverTotals <- WeedSurveys %>%
  mutate(cover = Intercepts*2) %>% # add column for % cover
  mutate(PlantType = case_when( # add column for plant type
    Species == "NONE" ~ "Bare",
    Species == "SINAL" ~ "Mustard",
    Species == "VICVI" ~ "Vetch",
    Species == "LOLMU" ~"Ryegrass",
    Species == "TRFRE" ~ "Clover",
    Species == "RAPSR" ~"Radish",
    Species == "BRSNN" ~ "Canola",
    TRUE ~ "Weed"))

# Clean up data and combine weeds into one row that represents total weed coverage
CoverTotals <- CoverTotals %>%
  filter(Zone != "Strip", TermTiming != "Early", Month != 2) %>% # remove strip and early termination observations
  select(-Year, -Month, -Day, -Zone, -TermTiming) %>% # remove unneeded columns
  group_by(Location, Rep, Treatment, PlantType) %>% # group plots together
  summarize(Cover = sum(cover)) %>% # add up cover for each plant type; stop here if you need to find variance across reps
  group_by(Location, Treatment, PlantType) %>% # regroup for the next step
  summarize(Cover = sum(Cover)/4) # average across reps (ugh this is so clunky)

# Make the plot
library(ggthemes)
ggplot(CoverTotals, aes(x = Treatment, y = Cover, fill = PlantType)) + 
  geom_bar(stat = "identity") + # identity works because dataframe is averaged across reps
  scale_fill_manual(values=cbPalette) + # using accessible color palette definied below
  labs(title = "Ground Cover") +
  ylab("% Cover") +
  facet_wrap("Location") +
  theme_few(base_size = 18) + # this line and next line copied from diversity figure below in order to match style
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("output/20180528_GroundCover.wmf")
ggsave("output/20180528_GroundCover.svg")
ggsave("output/20180528_GroundCoverCover.jpeg", dpi = 600)

# A manual color palette (from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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
