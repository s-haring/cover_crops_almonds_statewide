# Workbook for figures for first cover crop team meeting (May 2018)
# SCH May 23, 2018

# Import data -------------------------------------------------------------

library(readr)
WeedSurveys <- read_csv("data/20180518_CoverCropWeedSurveys.csv")
str(WeedSurveys)

# Stacked bar charts showing total weed cover, bare ground, and cover crop by species for each sampling timing --------





# Weed species diversity figure -------------------------------------------

# spread each row (species counts) into a community matrix (column per species)
WeedCommunityMatrix <- PercentCover %>%
  select(-cover) %>%
  select(-TermTiming) %>%
  select(-Zone) %>%
  select(-PlantType) %>%
  select(-Year) %>%
  select(-Day) %>%
  filter(TermTiming != "Early") %>%
  filter(Zone != "Strip") %>%
  filter(PlantType != "Cover") %>%
  filter(PlantType != "Bare") %>%
  spread(key = Species, value = Intercepts, fill = 0)

?select

# finding Shannon H for each plot
library(vegan)
Shannon <- diversity()
data(BCI)
head(BCI)
    PercentCover %>%
  filter(PlantType != "Cover") %>%
  filter(PlantType != "Bare") %>%
  group_by(Location, Rep, Treatment) %>%
  summarise(H = n_distinct(Species))

# Old goals below ---------------------------------------------------------


# Ground cover ------------------------------------------------------------

# make a new column of percent ground cover
library(tidyverse)
PercentCover <- mutate(WeedSurveys, cover = Intercepts*2)
# group weed species and cover crop species together - every observation is of crop, weed, or bare ground
library(mosaic)
PercentCover <- mutate(PercentCover, PlantType = case_when(
  Species == "NONE" ~ "Bare",
  Species == "SINAL" | Species == "VICVI" | Species == "LOLMU" | Species == "TRFRE" | Species == "RAPSR" | Species == "BRSNN" ~ "Cover",
  TRUE ~ "Weed"))
# sum the cover from each species into totals for the category
CoverTotals <- PercentCover %>%
  group_by(Year, Month, Day, Location, Rep, Treatment, TermTiming, Zone, PlantType) %>%
  summarize(totalcover = sum(cover))

# make the figure
ggplot(data= CoverTotals, aes(x = PlantType, y = totalcover))

# Weed species diversity --------------------------------------------------

# count species richness in each plot
SpeciesCounts <- PercentCover %>%
  filter(Species != "NONE") %>%
  group_by(Location, Rep, Treatment, PlantType) %>%
  summarise(richness = n_distinct(Species))

#make the figure
ggplot(data = SpeciesCounts, aes(x = Treatment, y = richness)) +
  geom_boxplot() +
  facet_wrap(~ Location) +
  theme_bw()

# Cover crop establishment by species -------------------------------------

# use PercentCover dataframe for coverage of individual species