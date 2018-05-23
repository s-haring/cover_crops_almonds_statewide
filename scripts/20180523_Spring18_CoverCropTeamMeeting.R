# Workbook for figures for first cover crop team meeting (May 2018)
# SCH May 23, 2018

library(readr)
WeedSurveys <- read_csv("data/20180518_CoverCropWeedSurveys.csv")
str(WeedSurveys)

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

? geom_boxplot()

# Weed species diversity --------------------------------------------------

# count species richness in each plot
SpeciesCounts <- PercentCover %>%
  filter(Species != "NONE") %>%
  group_by(Location, Rep, Treatment, PlantType) %>%
  summarise(richness = n_distinct(Species))

#make the figure
ggplot(data = SpeciesCounts, aes(x = Treatment, y = richness)) +
  geom_() +
  facet_wrap(~ Location) +
  theme_bw()

?geom_boxplot
# Cover crop establishment by species -------------------------------------

# use PercentCover dataframe for coverage of individual species
