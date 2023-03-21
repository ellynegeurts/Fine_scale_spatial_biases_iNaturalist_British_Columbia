#### Set_up ####

# Load libraries
library(bbmle)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(here)
library(lme4)
library(sjPlot)
library(tidyverse)

#### Read in data ####

## This file includes data from data sheets and some processed data that's been appended (see meta data)
trans_dat <- read.csv(here("Data", "transect_data.csv"))
# Please read meta data for more information regarding variables for transect data


## Read in exotic data from BC Species Ecosystem Explorer downloaded in 2022
spp_22 <- read.csv(here("Data", "results_exported_for_exotic_species_from_BC_Species_Ecosystem_Explorer_May_17_2022.csv"))


## All observations made during the transects collected by the BC iNaturalist team
obs_df <- read.csv(here("Data", "all_obs_from_transects.csv"))
# Note: a few columns were added during data prep (not in this script). 
# Newdate = just the day the observation took place (i.e. removed time)
# Time_convert is the date and time in Pacific Standard Time (to match datasheets)

# Also, note this df contains observations from a transect that has missing data due to hard drive corruption, so 
# "2021-06-20 TM M ON" and "2021-06-20 ES M OFF" need to be removed in analysis (FYI already removed in trans_dat)


## Read in list of vulnerable species
vul_spp <- read.csv(here("Data", "list_of_BC_vulnerable_species_from_BC_rarities_iNaturalist_project.csv"))


## Read in csv file that contains the singly recorded species tallies by park and trail position
rare_spp <- read.csv(here("Data", "on_off_single_obs_observed_during_transects_in_22_parks.csv"))


#### Invasive species data prep ####

# Removing transects with corrupted data
obs_df <- obs_df[!(obs_df$Survey_ID == "2021-06-20 TM M ON" | obs_df$Survey_ID == "2021-06-20 ES M OFF"),]

# Remove rows that do not contain "exotic" in 'BC List"
spp_22_exotic <- spp_22 %>% 
  filter(BC.List == "Exotic")

# Replace the column name to match obs_df
names(spp_22_exotic)[names(spp_22_exotic) == "Scientific.Name"] <- "taxon_species_name"

# Isolating exotic obs 
exotic_obs <- obs_df[obs_df$taxon_species_name 
                 %in% spp_22_exotic$taxon_species_name,]

# Organizing data
# Trying to organize data
exotic_grouped <- exotic_obs %>%
  arrange(newdate) %>% # Arrange by day
  group_by(Survey_ID) #Group by survey ID

# Count the number of rows (observations) per transect ID
exotic_obs_counts <- exotic_grouped %>% 
  tally() %>% 
  rename(exotic_obs = n)

# Number of species by transect
exotic_taxa <- exotic_grouped %>% 
  summarise(count = n_distinct(taxon_id)) %>% 
  rename(exotic_taxa = count)


# Add the number of exotic observations per transect ID - doing a left join
transect_exotic_Ljoin <- merge(trans_dat, exotic_obs_counts, by = "Survey_ID", all.x = T)

# Add the number of unique exotic taxa observer per transect ID
transect_exotic_Ljoin <- merge(transect_exotic_Ljoin, exotic_taxa, by = "Survey_ID", all.x = T)

# Replace NAs with zeros for the two new columns
transect_exotic_Ljoin <- transect_exotic_Ljoin %>% 
  mutate(exotic_obs = coalesce(exotic_obs, 0),
         exotic_taxa = coalesce(exotic_taxa, 0))

# Create a ratio of exotic species by ALL taxa observation
transect_exotic_Ljoin$ratio <- transect_exotic_Ljoin$exotic_taxa/transect_exotic_Ljoin$taxa 


#### Create boxplot figure of the proportion of exotic species observed per transect across the three habitat types ####

par(mar = c(5, 5, 2, 2))

# Need to rename the habitat variables
transect_exotic_Ljoin$Main.habitat.type <-  gsub(transect_exotic_Ljoin$Main.habitat.type,
                                              pattern = "Forest",
                                              replacement = c("Closed canopy forest"),
                                              fixed = T)

transect_exotic_Ljoin$Main.habitat.type <-  gsub(transect_exotic_Ljoin$Main.habitat.type,
                                              pattern = "Woodland",
                                              replacement = c("Open canopy forest"),
                                              fixed = T)

transect_exotic_Ljoin$Main.habitat.type <- factor(transect_exotic_Ljoin$Main.habitat.type, 
                                               levels = c("Grassland", "Open canopy forest", "Closed canopy forest"))

# Plot
ggplot(transect_exotic_Ljoin, aes(x = Main.habitat.type, y = ratio, fill = Trail.position)) +
  geom_boxplot(outlier.shape = NA) + # remove outliers to allow for jitter
  geom_point(colour = "black",
             position = position_jitterdodge(0.1)) +
  guides(fill=guide_legend(title = "Trail position")) +
  xlab("") +
  ylab("Proportion of exotic species \n observed per transect") +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))

ggsave("PATH_YOU_WANT_THE_FIGURE_SAVED_TO.tif",
       device='tiff', 
       units = "cm",
       width = 20,
       height = 15,
       dpi = 600)


#### Invasive species analysis ####


# Summarize number of exotic species observed by park and trail position
exo_prep <- transect_exotic_Ljoin %>% 
  group_by(Park.name, Trail.position) %>% 
  summarise(total = sum(exotic_taxa)) %>% 
  arrange(Trail.position)

# Testing for difference between on and off trail by park (n = 22)
wilcox.test(total ~ Trail.position, data = exo_prep, paired = T) 



#### Testing random effects  - total taxonomic richness on vs off trail analysis ####

## Creating random-intercept model
full_int <- glmer(taxa ~ Trail.position + Main.habitat.type +
                    Est.dist.sc + (1|Park.name/Trail.name) + 
                    (1|Observer.name), family = poisson(link = "log"),
                  data = trans_dat)

summary(full_int)

# Calculate dispersion statistic
p <- length(coef(full_int)) 
n <- nrow(trans_dat)

# Dispersion statistic
sum(resid(full_int, "pearson")^2 / (n - p)) # 0.899

# Checking model assumptions
plot(full_int)
qqnorm(resid(full_int))
qqline(resid(full_int)) # Looks a bit wavy. Comes off a bit on right
class(full_int) # glmerMod

hist(resid(full_int)) # looks fairly normal


## Add random slope observer ID by trail position
full_int_slope_ob <- glmer(taxa ~ Trail.position + Main.habitat.type +
                             Est.dist.sc + (1|Park.name/Trail.name) + 
                             (1 + Trail.position|Observer.name), family = poisson(link = "log"),
                           data = trans_dat) # Boundary (singular) fit

## Add random slope for location by trail position
full_int_slope_loc <- glmer(taxa ~ Trail.position + Main.habitat.type +
                              Est.dist.sc + (1 + Trail.position|Park.name/Trail.name) + 
                              (1|Observer.name), family = poisson(link = "log"),
                            data = trans_dat) # Boundary (singular) fit


## Add random slope for location by habitat
hab_int_slope_loc <- glmer(taxa ~ Trail.position + Main.habitat.type +
                             Est.dist.sc + (1 + Main.habitat.type|Park.name/Trail.name) + 
                             (1|Observer.name), family = poisson(link = "log"),
                           data = trans_dat) # Boundary (singular) fit

## Add random slope for location by estimated distance (scaled)
dist_int_slope_loc <- glmer(taxa ~ Trail.position + Main.habitat.type +
                              Est.dist.sc + (1 + Est.dist.sc|Park.name/Trail.name) + 
                              (1|Observer.name), family = poisson(link = "log"),
                            data = trans_dat) # Boundary (singular) fit

## Add random slope for observer by estimated distance (scaled)
dist_int_slope_ob <- glmer(taxa ~ Trail.position + Main.habitat.type +
                             Est.dist.sc + (1|Park.name/Trail.name) + 
                             (1 + Est.dist.sc|Observer.name), family = poisson(link = "log"),
                           data = trans_dat) # Boundary (singular) fit

# Random slopes is not possible for this data set
# Random intercept model with observer ID and nested location appears to be the best model available


#### Testing fixed effects - total taxonomic richness on vs off trail analysis #### 

## Model includes all 3 fixed effects
m3 <- glmer(taxa ~ Trail.position + Main.habitat.type +
              Est.dist.sc + (1|Park.name/Trail.name) + 
              (1|Observer.name), family = poisson(link = "log"),
            data = trans_dat)

summary(m3)

# Calculate dispersion statistic
p <- length(coef(m3)) 
n <- nrow(trans_dat)

# Dispersion statistic
sum(resid(m3, "pearson")^2 / (n - p)) # 0.899

# Checking model assumptions
plot(m3) # Looks okay
qqnorm(resid(m3))
qqline(resid(m3)) # Looks a little wavy

hist(resid(m3)) # Looks normalish


## Only trail position (only one fixed variable)
m1 <- glmer(taxa ~ Trail.position + (1|Park.name/Trail.name) + (1|Observer.name), 
            family = poisson(link = "log"),
            data = trans_dat)

# Calculate dispersion statistic
p <- length(coef(m1)) 
n <- nrow(trans_dat)

# Dispersion statistic
sum(resid(m1, "pearson")^2 / (n - p)) # 0.874

# Checking model assumptions
plot(m1) # Looks okay
qqnorm(resid(m1))
qqline(resid(m1)) # Looks a little wavy

hist(resid(m1)) # Looks fairly normal


## Model with all three fixed effects with interaction btwn trail and distance
m4.1 <- glmer(taxa ~ Trail.position*Est.dist.sc + Main.habitat.type +
                (1|Park.name/Trail.name) + (1|Observer.name), 
              family = poisson(link = "log"),
              data = trans_dat)

summary(m4.1)

# Calculate dispersion statistic
p <- length(coef(m4.1)) 
n <- nrow(trans_dat)

# Dispersion statistic
sum(resid(m4.1, "pearson")^2 / (n - p)) # 0.896

# Checking model assumptions
plot(m4.1) # Looks okay
qqnorm(resid(m4.1))
qqline(resid(m4.1)) # hmm looks wavy

hist(resid(m4.1)) #looks normalish


## Null model - no fixed effects
null <- glmer(taxa ~ 1 + (1|Park.name/Trail.name) + 
                (1|Observer.name), 
              family = poisson(link = "log"),
              data = trans_dat)

summary(m4.1)

# Calculate dispersion statistic
p <- length(coef(m4.1)) 
n <- nrow(trans_dat)

# Dispersion statistic
sum(resid(null, "pearson")^2 / (n - p)) # 0.936

# Checking model assumptions
plot(null) # Looks okay
qqnorm(resid(null))
qqline(resid(null)) # A little wavy

hist(resid(null)) # Mostly normal


## Comparing the different fixed-effects models 

bbmle::AICctab(null, m3, m4.1, m1, weights = TRUE) 


#### Boxplot of taxonomic richness observed per transect across the three habitat types ####

# Replacing old names for closed and open canopy forests
trans_dat$Main.habitat.type <-  gsub(trans_dat$Main.habitat.type,
                                    pattern = "Forest",
                                    replacement = c("Closed canopy forest"),
                                    fixed = T)

trans_dat$Main.habitat.type <-  gsub(trans_dat$Main.habitat.type,
                                    pattern = "Woodland",
                                    replacement = c("Open canopy forest"),
                                    fixed = T)

# Reorder factor levels
trans_dat$Main.habitat.type <- factor(trans_dat$Main.habitat.type, 
                                     levels = c("Grassland", "Open canopy forest", "Closed canopy forest"))
# Set plot parameters
par(mfrow = c(1,1), mar = c(5, 5, 5, 5))

# Create plot
ggplot(trans_dat, aes(x = Main.habitat.type, y = taxa, fill = Trail.position)) +
  geom_boxplot() +
  geom_point(colour = "black",
             position = position_jitterdodge(0.1)) +
  guides(fill=guide_legend(title = "Trail position")) +
  xlab("") +
  ylab("Taxonomic richness \n observed per transect") +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))


ggsave("PATH_YOU_WANT_THE_FIGURE_TO_BE_SAVED_TO.tif",
       device='tiff', 
       units = "cm",
       width = 20,
       height = 15,
       dpi = 600)


#### Native taxonomic richness between on and off trail ####

# This analysis assumes you ran the code in "invasive species data prep"


# Removing the exotic species observation to leave native taxa observations
native_obs <- obs_df[!obs_df$taxon_species_name 
                    %in% spp_22_exotic$taxon_species_name,] 

# Organizing data
native_grouped <- native_obs %>%
  arrange(newdate) %>% # Arrange by day
  group_by(Survey_ID) #Group by survey ID

# Count the number of rows (observations) per transect ID
native_obs_counts <- native_grouped %>% 
  tally() %>% 
  rename(native_obs = n)

# Number of species by transect ID
native_taxa <- native_grouped %>% 
  summarise(count = n_distinct(taxon_id)) %>% 
  rename(native_taxa = count)


# Add the number of exotic observations per transect ID
transect_native <- merge(trans_dat, native_obs_counts, by = "Survey_ID")
transect_native <- merge(transect_native, native_taxa, by = "Survey_ID")


## Running models similar to total taxonomic richness analysis above
# Use the same random effects structure


# Using native species instead
m3.n <- glmer(native_taxa ~ Trail.position + Main.habitat.type +
                Est.dist.sc + (1|Park.name/Trail.name) + 
                (1|Observer.name), family = poisson(link = "log"),
              data = transect_native) 

summary(m3.n) # woodland barely significant!!!

# Calculate dispersion statistic
p <- length(coef(m3.n)) 
n <- nrow(transect_native)

# Dispersion statistic
sum(resid(m3.n, "pearson")^2 / (n - p)) # 0.912

# Checking some model assumptions
plot(m3.n) # Looks okay
qqnorm(resid(m3.n))
qqline(resid(m3.n)) # Looks a bit wavy

hist(resid(m3.n)) #looks normal


# Only trail position
m1.n <- glmer(native_taxa ~ Trail.position + (1|Park.name/Trail.name) + (1|Observer.name), 
              family = poisson(link = "log"),
              data = transect_native)
summary(m1.n)

# Calculate dispersion statistic
p <- length(coef(m1.n)) 
n <- nrow(transect_native)

# Dispersion statistic
sum(resid(m1.n, "pearson")^2 / (n - p)) # 0.885 

# Checking some model assumptions
plot(m1.n) # Looks okay
qqnorm(resid(m1.n))
qqline(resid(m1.n)) #Looks a little wavy

hist(resid(m1.n)) #looks normal


# Interaction between trail position and distance
m4.1.n <- glmer(native_taxa ~ Trail.position*Est.dist.sc + Main.habitat.type +
                  (1|Park.name/Trail.name) + (1|Observer.name), 
                family = poisson(link = "log"),
                data = transect_native)

summary(m4.1.n)

# Calculate dispersion statistic
p <- length(coef(m4.1.n)) 
n <- nrow(transect_native)

# Dispersion statistic
sum(resid(m4.1.n, "pearson")^2 / (n - p)) # 0.909

# Checking some model assumptions
plot(m4.1.n) # Looks okay
qqnorm(resid(m4.1.n))
qqline(resid(m4.1.n)) # Looks a little wavy with some tails coming off

hist(resid(m4.1.n)) #looks fairly normal


# Null model - no fixed effects
null.n <- glmer(native_taxa ~ 1 + (1|Park.name/Trail.name) + 
                  (1|Observer.name), 
                family = poisson(link = "log"),
                data = transect_native)

summary(null)

# Calculate dispersion statistic
p <- length(coef(null)) 
n <- nrow(transect_native)

# Dispersion statistic
sum(resid(null.n, "pearson")^2 / (n - p)) # 0.903

# Checking some model assumptions
plot(null) # Looks curved down
qqnorm(resid(null))
qqline(resid(null)) # hmm looks a bit wavy

hist(resid(null))

#Comparing the diferent models
bbmle::AICctab(null.n, m3.n, m4.1.n, m1.n, weights = TRUE)


#### Coefficient plot for total taxonomic richness versus native taxonomic richness ####

# Plot settings
set_theme(base = theme_classic(), #To remove the background color and the grids
          axis.textcolor = "black",
          axis.title.color = "black",
          axis.textsize.y = 1.1,
          axis.title.size = 1.1)  


# Creating plot for top model for taxonomic richness between on and off trail
all <- plot_model(m3, type = "est", show.values = T, value.offset = 0.2,
                  value.size = 4,
                  colors = c("steelblue4", "darkorange4"),
                  axis.title = "Coefficient estimates", transform = NULL, title = "", 
                  axis.labels = c("Distance travelled","Habitat \n [open canopy forest]",
                                  "Habitat \n [grassland]","Trail position \n [on]"))  # Forest-plot of estimates - Gives coefficient plots

# Now creating plot for top model for native taxonomic richness between on and off trail  
nat <- plot_model(m3.n, type = "est", show.values = T, value.offset = 0.2,
                  value.size = 4,
                  colors = c("steelblue4", "darkorange4"),
                  axis.title = "Coefficient estimates", transform = NULL, title = "", 
                  axis.labels = c("Distance travelled","Habitat \n [open canopy forest]",
                                  "Habitat \n [grassland]","Trail position \n [on]")) 

# Tif figure settings
tiff("PATH_YOU_WANT-THE_FIGURE_TOBE_SAVED_TO.tif",
     units = "cm",
     width = 20,
     height = 10,
     res = 600)

# Make plot
gridExtra::grid.arrange(all, nat, ncol = 2)
dev.off()


#### Vulnerable species analysis ####

# Replace a column name to match obs_df
names(vul_spp)[names(vul_spp) == "taxon_name"] <- "taxon_species_name"

# Isolate observations that contain vulnerable species
vul_obs <- obs_df[obs_df$taxon_species_name %in% vul_spp$taxon_species_name,]

# Organize data by transect ID
vul_grouped <- vul_obs %>% 
  arrange(newdate) %>% 
  group_by(Survey_ID)

# Count number of observations per transect
vul_obs_counts <- vul_grouped %>% 
  tally() %>% 
  rename(vul_obs = n)

# Count the number of unique taxa observed by transect
vul_taxa_counts <- vul_grouped %>% 
  summarise(count = n_distinct(taxon_id)) %>% 
  rename(vul_taxa = count)

# Add the number of vulnerable observations by transect ID
transect_vul <- merge(trans_dat, vul_obs_counts, by = "Survey_ID")

# Add the number of unique vulnerable taxa observed by transect ID
transect_vul <- merge(transect_vul, vul_taxa_counts, by = "Survey_ID")


# Get number of transects that encountered at least one vulnerable species organized by trail position
vul_by_trail_pos <- transect_vul %>% 
  group_by(Trail.position) %>% 
  tally()


#### Rare species analysis ####

# Preparation - format dataset for wilcoxon sign rank test
rare_spp$Trail.position <- as.factor(rare_spp$Trail.position)

off <- rare_spp %>% 
  filter(Trail.position == "off")
on <- rare_spp %>% 
  filter(Trail.position == "on")

rare_prepped <- rbind(on, off)

# Run test
wilcox.test(rare_prepped$Number.of.singles ~ rare_prepped$Trail.position, paired = TRUE) s# p = 0.3951
