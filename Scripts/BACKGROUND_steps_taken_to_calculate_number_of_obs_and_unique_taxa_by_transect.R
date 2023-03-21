## All observations made during the transects collected by the BC iNaturalist team
obs_df <- read.csv(here("Data", "all_obs_from_transects.csv"))

# Note: a few columns were added during data prep (not in this script). 
# Newdate = just the day the observation took place (i.e. removed the time element from the original date column)
# Time_convert is the date and time in Pacific Standard Time (to match datasheets)

# Also, note this df contains observations from a transect that has missing data due to hard drive corruption, so 
# "2021-06-20 TM M ON" and "2021-06-20 ES M OFF" need to be removed in analysis (FYI already removed in trans_dat)

# Removing transects with corrupted data
obs_df <- obs_df[!(obs_df$Survey_ID == "2021-06-20 TM M ON" | obs_df$Survey_ID == "2021-06-20 ES M OFF"),]

obs_df_grouped <- obs_df %>% 
  arrange(newdate) %>% 
  group_by(Survey_ID)

obs <- obs_df_grouped %>% 
  tally() %>% 
  rename(obs = n)

taxa = obs_df_grouped %>% 
  summarise(count = n_distinct(taxon_id)) %>% 
  rename(taxa = count)


# Then the vectors "obs" and "taxa" was appended (using the merge function and matching  by Survey_ID) 
# to the meta datasheet "transect_data.csv" 