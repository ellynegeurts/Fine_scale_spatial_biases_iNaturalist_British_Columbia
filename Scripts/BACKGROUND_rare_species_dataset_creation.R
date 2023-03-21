#### Set_up ####

# Load libraries
library(here)
library(tidyverse)

#### Read in observations from all 22 parks ####

# These data were downloaded from iNaturalist on Jan, 30, 2023

csv_names <- list.files(here("Data", "all_park_obs_downloaded_Jan_30_2023"),
                        full.names = T)

csv_names2 <- data.frame(park = csv_names,
                         id = as.character(1:length(csv_names)))

park_dat <- csv_names %>% 
  lapply(read.csv) %>% 
  bind_rows(.id = "id") %>%
  left_join(csv_names2)

#### Read in data observed by iNaturalist team during transects ####

## All observations made during the transects collected by the BC iNaturalist team
obs_df <- read.csv(here("Data", "all_obs_from_transects.csv"))

# Note: a few columns were added during data prep (not in this script). 
# Newdate = just the day the observation took place (i.e. removed the time element from the original date column)
# Time_convert is the date and time in Pacific Standard Time (to match datasheets)

# Also, note this df contains observations from a transect that has missing data due to hard drive corruption, so 
# "2021-06-20 TM M ON" and "2021-06-20 ES M OFF" need to be removed in analysis (FYI already removed in trans_dat)

# Removing transects with corrupted data
obs_df <- obs_df[!(obs_df$Survey_ID == "2021-06-20 TM M ON" | obs_df$Survey_ID == "2021-06-20 ES M OFF"),]

#### Read in meta data of transects ####

## This file includes data from data sheets and some processed data that's been appended (see meta data)
trans_dat <- read.csv(here("Data", "transect_data.csv"))
# Please read meta data for more information regarding variables for transect data


#### Data processing - Part A ####

# Restricting observations to species-level observations
park_spp <-  park_dat[!(is.na(park_dat$taxon_species_name) | park_dat$taxon_species_name ==""), ] # 57,843 obs

# Then restrict to only verifiable observations
park_spp_veri <- subset(park_spp, quality_grade != "casual") #57,547 obs

# Counting number of observations for each species
park_spp_veri_count <- park_spp_veri %>% 
  count(taxon_species_name, sort = T)

# select single record species for now (ffor verifiable observations)
park_singles_veri <- park_spp_veri_count %>% 
  filter(n == 1)

# Number of each species for each park (for verifiable observations)
park_spp_veri_count_by_park <- park_spp_veri %>%
  group_by(park) %>% 
  count(taxon_species_name, sort = T) 

# Number of species observed by park (for verifiable observations)  
num_spp_park_VERI <- park_spp_veri_count_by_park %>% 
  count(park, name = "num_species")

# Select single record species BY PARK (for verifiable observations) 
singles_by_park_VERI <- park_spp_veri_count_by_park %>% 
  filter(n == 1)

# Number of SINGLE species by park (for verifiable observations)
num_single_spp_park_VERI <- singles_by_park_VERI %>% 
  count(park, name = "num_singles")


# Merge the number of species and singleton species for each park and calculate percent of species recorded are singletons (rare)
merge_species_counts_VERI <- merge(num_spp_park_VERI, num_single_spp_park_VERI) %>% 
  mutate(percent_single = (num_singles/num_species)*100)


#### Data processing - Part B ####

# Add info about transect meta data to transect observations
df_parks <- left_join(obs_df, trans_dat, by = "Survey_ID") # Now I can group obs by park

# The following sections are the same analyses repeated for each park. 
# The values generated were then entered into excel file - "on_off_single_obs_observed_during_transects_in_22_parks.csv"

# NOTE: if you try to repeat the analyses below, you will need to update the park names in the "singles_by_park_VERI" df first


#### Beatton ####

Beatton <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Beatton_observations-292103.csv")

df_Beatton <- df_parks %>% 
  filter(Park.name == "Beatton pp")

single_Beatton <- df_Beatton[df_Beatton$taxon_species_name %in% Beatton$taxon_species_name,] 

t_Beatton <- single_Beatton %>% 
  group_by(Trail.position) %>% 
  tally()

#### Birkenhead ####

Birkenhead <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Birkenhead_Lake_observations-292428.csv")

df_Birkenhead <- df_parks %>% 
  filter(Park.name == "Birkenhead Lake pp")

single_Birkenhead <- df_Birkenhead[df_Birkenhead$taxon_species_name %in% Birkenhead$taxon_species_name,] 

t_Birkenhead <- single_Birkenhead %>% 
  group_by(Trail.position) %>% 
  tally()


#### Boya ####

Boya <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Boya_observations-292463.csv")

df_Boya <- df_parks %>% 
  filter(Park.name == "Boya pp")

single_Boya <- df_Boya[df_Boya$taxon_species_name %in% Boya$taxon_species_name,] 

t_Boya <- single_Boya %>% 
  group_by(Trail.position) %>% 
  tally()

#### Charlie ####

Charlie <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Charlie_Lake_observations-292583.csv")

df_Charlie <- df_parks %>% 
  filter(Park.name == "Charlie Lake pp")

single_Charlie <- df_Charlie[df_Charlie$taxon_species_name %in% Charlie$taxon_species_name,] 

t_Charlie <- single_Charlie %>% 
  group_by(Trail.position) %>% 
  tally()

#### Crooked ####

Crooked <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Crooked_River_observations-292468.csv")

df_Crooked<- df_parks %>% 
  filter(Park.name == "Crooked River pp")

single_Crooked <- df_Crooked[df_Crooked$taxon_species_name %in% Crooked$taxon_species_name,] 

t_Crooked<- single_Crooked %>% 
  group_by(Trail.position) %>% 
  tally()

#### Elephant ####

Elephant <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Elephant_Hill_observations-292538.csv")

df_Elephant <- df_parks %>% 
  filter(Park.name == "Elephant Hill PP")

single_Elephant <- df_Elephant[df_Elephant$taxon_species_name %in% Elephant$taxon_species_name,] 

t_Elephant<- single_Elephant %>% 
  group_by(Trail.position) %>% 
  tally()

#### Ellison ####

Ellison <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Ellison_observations-292540.csv")

df_Ellison <- df_parks %>% 
  filter(Park.name == "Ellison")

single_Ellison <- df_Ellison[df_Ellison$taxon_species_name %in% Ellison$taxon_species_name,] 

t_Ellison <- single_Ellison %>% 
  group_by(Trail.position) %>% 
  tally()


#### Fintry ####

Fintry <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Fintry_observations-292543.csv")

df_Fintry <- df_parks %>% 
  filter(Park.name == "Fintry")

single_Fintry <- df_Fintry[df_Fintry$taxon_species_name %in% Fintry$taxon_species_name,] 

t_Fintry <- single_Fintry %>% 
  group_by(Trail.position) %>% 
  tally()

#### Kalamalka ####

Kalamalka <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Kalamalka_Lake_observations-292548.csv")

df_Kalamalka <- df_parks %>% 
  filter(Park.name == "Kalamalka Lake")

single_Kalamalka <- df_Kalamalka[df_Kalamalka$taxon_species_name %in% Kalamalka$taxon_species_name,] 

t_Kalamalka <- single_Kalamalka %>% 
  group_by(Trail.position) %>% 
  tally()

#### Kinaskan ####

Kinaskan <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Kinaskan_observations-292554.csv")

df_Kinaskan <- df_parks %>% 
  filter(Park.name == "Kinaskan pp")

single_Kinaskan <- df_Kinaskan[df_Kinaskan$taxon_species_name %in% Kinaskan$taxon_species_name,] 

t_Kinaskan <- single_Kinaskan %>% 
  group_by(Trail.position) %>% 
  tally()

#### Lakelse ####

Lakelse <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Lakelse_Lake_observations-292570.csv")

df_Lakelse <- df_parks %>% 
  filter(Park.name == "Lakelse Lake pp")

single_Lakelse <- df_Lakelse[df_Lakelse$taxon_species_name %in% Lakelse$taxon_species_name,] 

t_Lakelse <- single_Lakelse %>% 
  group_by(Trail.position) %>% 
  tally()

#### Marble Canyon ####

Marble <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Marble_Canyon_observations-292572.csv")

df_Marble <- df_parks %>% 
  filter(Park.name == "Marble Canyon")

single_Marble <- df_Marble[df_Marble$taxon_species_name %in% Marble$taxon_species_name,] 

t_Marble <- single_Marble %>% 
  group_by(Trail.position) %>% 
  tally()

#### Mehatl #### 

Mehatl <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Mehatl_creek_observations-292573.csv")

df_Mehatl <- df_parks %>% 
  filter(Park.name == "Mehatl Creek pp")

single_Mehatl <- df_Mehatl[df_Mehatl$taxon_species_name %in% Mehatl$taxon_species_name,] 

t_Mehatl <- single_Mehatl %>% 
  group_by(Trail.position) %>% 
  tally()

#### Nahatlatch ####

Nahatlatch <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Nahatlatch_observations-292575.csv")

df_Nahatlatch <- df_parks %>% 
  filter(Park.name == "Nahatlatch pp")

single_Nahatlatch <- df_Nahatlatch[df_Nahatlatch$taxon_species_name %in% Nahatlatch$taxon_species_name,] 

t_Nahatlatch <- single_Nahatlatch %>% 
  group_by(Trail.position) %>% 
  tally()

#### Nairns falls ####

Nairns <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Nairn_falls_observations-292576.csv")

df_Nairns <- df_parks %>% 
  filter(Park.name == "Nairn Falls ")

single_Nairns <- df_Nairns[df_Nairns$taxon_species_name %in% Nairns$taxon_species_name,] 

t_Nairns <- single_Nairns %>% 
  group_by(Trail.position) %>% 
  tally()

#### Oregon Jack ####

Oregon <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Oregon_Jack_observations-292578.csv")

df_Oregon <- df_parks %>% 
  filter(Park.name == "Oregon Jack PP")

single_Oregon <- df_Oregon[df_Oregon$taxon_species_name %in% Oregon$taxon_species_name,] 

t_Oregon <- single_Oregon %>% 
  group_by(Trail.position) %>% 
  tally()

#### Pine le Moray ####

Pine <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Pine_Le_Moray_observations-292581.csv")

df_Pine <- df_parks %>% 
  filter(Park.name == "Pine le Moray pp")

single_Pine <- df_Pine[df_Pine$taxon_species_name %in% Pine$taxon_species_name,] 

t_Pine <- single_Pine %>% 
  group_by(Trail.position) %>% 
  tally()

#### Skaha Bluffs ####

Skaha <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Skaha_Bluffs_observations-292585.csv")

df_Skaha <- df_parks %>% 
  filter(Park.name == "Skaha Bluffs")

single_Skaha <- df_Skaha[df_Skaha$taxon_species_name %in% Skaha$taxon_species_name,] 

t_Skaha <- single_Skaha %>% 
  group_by(Trail.position) %>% 
  tally()

#### Skihist ####

Skihist <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Skihist_observations-292587.csv")

df_Skihist <- df_parks %>% 
  filter(Park.name == "Skihist pp")

single_Skihist <- df_Skihist[df_Skihist$taxon_species_name %in% Skihist$taxon_species_name,] 

t_Skihist <- single_Skihist %>% 
  group_by(Trail.position) %>% 
  tally()

#### South Okanagan grasslands ####

South <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/South_Okanagan_Grasslands_observations-292589.csv")

df_South <- df_parks %>% 
  filter(Park.name == "South Okanagan Grasslands")

single_South <- df_South[df_South$taxon_species_name %in% South$taxon_species_name,] 

t_South <- single_South %>% 
  group_by(Trail.position) %>% 
  tally()

#### Steelhead #### 

Steelhead <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Steelhead_observations-292590.csv")

df_Steelhead <- df_parks %>% 
  filter(Park.name == "Steelhead pp")

single_Steelhead <- df_Steelhead[df_Steelhead$taxon_species_name %in% Steelhead$taxon_species_name,] 

t_Steelhead <- single_Steelhead %>% 
  group_by(Trail.position) %>% 
  tally()

#### Whiskers Point ####

Whiskers <- singles_by_park_VERI %>% 
  filter(park == "Z:/Ellyne/Research/NEW/RProjects/fine_scale_spatial_biases/Data/all_park_obs_downloaded_Jan_30_2023/Whiskers_Point_observations-292591.csv")

df_Whiskers <- df_parks %>% 
  filter(Park.name == "Whiskers Point pp")

single_Whiskers <- df_Whiskers[df_Whiskers$taxon_species_name %in% Whiskers$taxon_species_name,] 

t_Whiskers <- single_Whiskers %>% 
  group_by(Trail.position) %>% 
  tally()

