################################################################################
# Load libraries
################################################################################

library(tidyverse)
library(cmapplot)

################################################################################
# Load MDT data from separate repo
################################################################################

source("../mydailytravel/R/data_cleaning.R")
source("../mydailytravel/R/helper_fns.R")
setwd("../mdt_outputs")


################################################################################
# Generate base data
################################################################################

# Mileage bins
mileage_breaks <- c(-1,.25,.5,1,2.5,5,10,25,50,100)
mileage_labels <- c("<=0.25","0.25 to 0.5","0.5 to 1","1 to 2.5","2.5 to 5",
                    "5 to 10","10 to 25","25 to 50","50 to 100")
base_data <-
  mdt %>%                              # 125463 records
  # Exclude "beginning" trips
  filter(mode_c != "beginning") %>%   # 97374
  # Exclude trips with no travel distance. Note this is a different difference
  # calculation than that used in TT (great circle vs. actual travel distance).
  # We chose to do this since the published graphics do not involve any
  # comparison between TT and MDT. However, if we instead filter out those trips
  # that have a nonzero haversine distance from MDT, the results are similar.
  filter(distance_pg > 0) %>%         # 97316
  # Exclude trips with a "missing" mode
  filter(mode_c != "missing") %>%     # 97279
  # Exclude improbable walk trips
  filter(improbable_walk == 0) %>%    # 97239
  # Add mileage bins
  mutate(mileage_bin=cut(distance_pg,breaks=mileage_breaks,
                         labels=mileage_labels))

################################################################################
# Calculate percent of trips
################################################################################

trip_pcts <-
  pct_calculator(base_data,
                 breakdown_by = "mileage_bin",
                 weight = "weight") %>% 
  mutate(mode_c = "all")

trip_mode_pcts <-
  pct_calculator(base_data,
                 breakdown_by = "mileage_bin",
                 second_breakdown = "mode_c",
                 weight = "weight") %>% 
  rbind(trip_pcts)

write.csv(trip_pcts,"trip_pcts.csv")
write.csv(trip_mode_pcts,"trip_mode_pcts.csv")

################################################################################
# Calculate percent of trip miles
################################################################################

trip_distance_pcts <-
  base_data %>% 
  mutate(total_distance = sum(distance_pg)) %>% 
  group_by(mileage_bin) %>% 
  mutate(distance_share = distance_pg/total_distance) %>% 
  summarize(pct = round(sum(distance_share),6),
            n = n(),
            wt = sum(weight)) %>% 
  mutate(mode_c = "all")


trip_distance_mode_pcts <-
  base_data %>% 
  group_by(mode_c) %>% 
  mutate(total_distance = sum(distance_pg)) %>% 
  group_by(mileage_bin,mode_c) %>% 
  mutate(distance_share = distance_pg/total_distance) %>% 
  summarize(pct = round(sum(distance_share),6),
            n = n(),
            wt = sum(weight)) %>% 
  rbind(trip_distance_pcts)

write.csv(trip_distance_pcts,"trip_distance_pcts.csv")
write.csv(trip_distance_mode_pcts,"trip_distance_mode_pcts.csv")
