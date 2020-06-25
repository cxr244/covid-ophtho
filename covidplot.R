library(sf)
library(ggplot2)
library(albersusa)
library(maps)
library(dplyr)
library(readr)
# To install package, use devtools::install_github("hrbrmstr/albersusa")



#####SETUP#####
suppressPackageStartupMessages(library("readr"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("maps"))

####INPUT#####
# Below has to be changed based on the location of the appropriate file on host computer 
pcare <- read_csv("C:/Chandru/CWRU/Research/CCF/COVIDProject/aamc-state-data-Ophtho.csv")
# covid <- read_csv("C:/Chandru/CWRU/Research/CCF/COVIDProject/covid-confirmed.csv")

#####PROCESS#####
# pcare:
#   - add column for # of physicians over 60 per 100K population
#   - only select states that can be plotted (remove DC, PR)
pcare <- pcare %>%
  # mutate(Risk = as.double(Risk)) %>%
  filter(!(state %in% c("DC", "PR"))) %>%
  rename(long2 = Long_) %>%
  rename(lat2 = Lat) %>%
  mutate(LogCases = log10(`6/21/2020`)) %>% 
  filter(`6/21/2020` > 0) %>%
  filter(lat2 > 25 & lat2 < 50 & long2 > -130 & long2 < -65) 

# covid:
#   - only select date column that will actually be used
#   - rename Long_ to long (contains longitude)
#   - rename Lat to lat (contains latitude)
#   - remove rows w/ zero cases (won't be plotted anyway, won't cause logarithm issues)
#   - remove rows not in continental United States
#   - take logarithm to make bubbles more aesthetically pleasing
# covid <- covid %>%
#   select(Lat, Long_, `6/4/2020`) %>%
#   rename(long = Long_) %>%
#   rename(lat = Lat) %>%
#   filter(`6/4/2020` > 0) %>%
#   filter(lat > 25 & lat < 50 & long > -130 & long < -65) %>%
#   mutate(LogCases = log10(`6/4/2020`))

# prepare map data
states_pleth <- map_data("state")
state_data <- left_join(states_pleth, pcare, by = "region")

#####VISUALIZE & SAVE#####
## bubble map of 50 U.S. states
ggplot() +
  # map w/ states colored by # `physicians over 60` per 100K population
  geom_polygon(data = state_data,
               aes(x = long, y = lat, group = group, fill = PercentOver60),
               color = "black") +
  scale_fill_gradient("% Ophthalmologists > 60", low = "antiquewhite1", high = "darkblue", 
                      breaks = c(30,35,40,45), labels=c("30%","35%","40%", "45%"), na.value="black") +
  # scale_fill_gradient("Ophthalmologists > 60 years old (%)", low = "white", high = "red", na.value="white") +
  # bubbles w/ log(COVID cases)
  geom_point(data = state_data,
             aes(x = long2, y = lat2, size = `6/21/2020`),
             colour = "firebrick2", alpha = 0.9, shape = 20) +
  scale_size_continuous("COVID-19 Cases",
                        range = c(1, 3),
                        labels = c("100k", "200k", "300k")) +

  # ,
                   # labels = c("1-9", "10-99", "100-999",
                   #                 "1000-9999", "10000-99999",
                   #                 ">100000")
  # remove elements we don't need
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(fill = "white"))

# save bubble chloropleth
ggsave(filename = "BubbleMap.png",
       width = 8, height = 4)

