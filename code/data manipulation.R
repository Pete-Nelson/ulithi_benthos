# Peter Nelson
# Institute for Marine Science
# University of California Santa Cruz
# created: 10 March 2023
# purpose: prep Ulithi benthic data for analyses
# requires: data from 'read data.R'

library(lubridate)
levels(temp$category)
levels(temp$morph_group)

# data wrangling ----

# all corals combined (area and year restricted example)
temp %>% 
  filter(region == "ulithi" &
           year(date) == "2016" &
           depth_zone == "shallow" & 
           category == "coral"
         ) %>% 
  select(c(2, 4:6, 10:11, 13:15, 18)) %>% 
  distinct() %>% 
  group_by(sitecode, 
           date,
           benthic_surveyor, 
           transect, quadrat, 
           morph_group, submorph_group) %>% 
  count() %>% 
  filter(n > 1)

# shows MANY quadrats with >1 observation of a morph+submorph combination!
# could be a good cluster to look at (ie separate islands by coral types)
# consider giving Montipora its own submorph_group (=cabbage?)

## total coral ----
total_coral <- temp %>% 
  filter(region == "ulithi" &
           year(date) == "2016" &
           depth_zone == "shallow" & 
           category == "coral"
  ) %>% 
  select(c(2, 4:6, 10:11, 13:15, 18)) %>% 
  group_by(island, sitecode, date, 
           benthic_surveyor, 
           transect, quadrat) %>% # summarises all coral morph_groups incl Montipora
  summarise(total_coral = sum(pct_cover)) # total coral cover (%)

## Montipora ----
montipora <- temp %>% 
  filter(region == "ulithi" &
           year(date) == "2016" &
           depth_zone == "shallow" &
           category == "coral" &
           morph_group == "montipora"
         ) %>% 
  select(c(2, 4:6, 10:11, 13:15, 18)) %>%
  group_by(island, sitecode, date,
           benthic_surveyor,
           transect, quadrat) %>% # summarises all Montipora
  summarise(montipora = sum(pct_cover)) # total Montipora cover (%)

## coral no Montipora ----
coral <- full_join(total_coral, montipora) %>% 
  mutate(coral = total_coral - montipora)

### coral morphs -----
coral_morphs <- temp %>% 
  filter(region == "ulithi" &
           year(date) == "2016" &
           depth_zone == "shallow" & 
           category == "coral"
  ) %>% 
  select(c(2, 4:6, 10:11, 13:15, 18)) %>% 
  group_by(island, sitecode, date, 
           benthic_surveyor, 
           transect, quadrat,
           morph_group) %>%
  summarise(cover = sum(pct_cover)) %>% 
  pivot_wider(names_from = morph_group,
              values_from = cover,
              values_fill = 0)
coral_morphs$benthic_surveyor <- 
  recode_factor(coral_morphs$benthic_surveyor, 
                "Michelle Paddack" = "MP")

## algae ----
algae <- temp %>% 
  filter(region == "ulithi" &
           year(date) == "2016" &
           depth_zone == "shallow" &
           category == "algae" &
           morph_group != "CCA"
  ) %>% 
  select(c(2, 4:6, 10:11, 13:15, 18)) %>%
  group_by(island, sitecode, date,
           benthic_surveyor,
           transect, quadrat) %>% # summarises all algae except CCA
  summarise(algae = sum(pct_cover)) # total algal cover (%)

## CCA ----
cca <- temp %>% 
  filter(region == "ulithi" &
           year(date) == "2016" &
           depth_zone == "shallow" &
           category == "algae" &
           morph_group == "cca"
  ) %>% 
  select(c(2, 4:6, 10:11, 13:15, 18)) %>%
  group_by(island, sitecode, date,
           benthic_surveyor,
           transect, quadrat) %>% # summarises all CCA
  summarise(cca = sum(pct_cover)) # total CCA cover (%)

## bare reef ----
abiotic <- temp %>% 
  filter(region == "ulithi" &
           year(date) == "2016" &
           depth_zone == "shallow" &
           category == "abiotic"
  ) %>% 
  select(c(2, 4:6, 10:11, 13:15, 18)) %>%
  group_by(island, sitecode, date,
           benthic_surveyor,
           transect, quadrat) %>% # summarises all abiotic reef
  summarise(abiotic = sum(pct_cover)) # total abiotic reef area (%)

(cover <- full_join(coral, algae) %>% 
    full_join(., cca) %>% 
    full_join(., abiotic) %>% 
    replace_na(list(total_coral = 0,
                    montipora = 0,
                    coral = 0,
                    algae = 0,
                    cca = 0,
                    abiotic = 0)))

plot(total_coral ~ island, data = coral, las = 1) 
# includes islands that were filtered out (eg Woleai), why? no data...

# FIX ##############################################################
# encrusting = Encrusting, cyanobacteria = Cyanobacteria, corallimorph = Corallimorph
# change "outbreak Montipora" to "Montipora"

# SCRATCH ##############################################################
df = data.frame(x = c(1.2, 0.4, NA, 0.6), y = c(NA, 0.3, 0.992, 0.5))
df %>% replace(. > 0.99 | is.na(.), 0)
df %>% mutate(x = replace(x, x > 0.99, "yay"))

df = data.frame(x = c(1.2, 0.4, "Montipora", 0.6), y = c("Montipora", 0.3, 0.992, 0.5))
df %>% mutate(x = replace(x, x == "Montipora", "coral"))

set.seed(123)
df <- temp %>% 
  filter(region == "ulithi") %>% 
  slice_sample(., n = 30) %>% 
  select(c(3, 5, 10, 11, 13:15, 18))
print(df, n=30)
levels(df$morph_group) # gives all the _potential_ levels for this factor!
df %>% group_by(morph_group) %>% summarise(n = n())

df$morph_group <- recode_factor(df$morph_group, 
                                "outbreak Montipora" = "Montipora",
                                "Encrusting" = "encrusting",
                                "Cyanobacteria" = "cyanobacteria",
                                "Corallimorph" = "corallimorph")
df %>% group_by(morph_group) %>% summarise(n = n())

vltava.spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-spe.txt', row.names = 1)

dis <- vegdist (sqrt (vltava.spe), method = 'bray') # percentage cover data are transformed by square root
cluster.single <- hclust (d = dis, method = 'single')
cluster.complete <- hclust (dis, 'complete')
cluster.average <- hclust (dis, 'average')

par (mfrow = c (1,3)) # will draw all dendrograms into one figure

plot (cluster.single, main = 'Single linkage')
plot (cluster.complete, main = 'Complete linkage')
plot (cluster.average, main = 'Average linkage')
