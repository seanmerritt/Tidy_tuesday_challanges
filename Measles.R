measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

dat <- measles
pacman::p_load(tidyverse, jmv, sf, maps, USAboundaries, USAboundariesData, ggrepel, rnaturalearth, geofacet, RColorBrewer,wesanderson, leaflet)

## I need to get rid of the NAs in the x files 

dat <- dat %>% 
  mutate(xrel = ifelse(is.na(xrel), 0, xrel),
         xper = ifelse(is.na(xper), 0, xper),
         xmed = ifelse(is.na(xmed), 0, xmed))

county_vax  <- dat %>% 
  group_by(state, county) %>% 
  summarize(mmr_county = weighted.mean(mmr, na.rm = T))


states <- st_as_sf(us_states())


us_counties <- st_as_sf(us_counties()) %>% 
  rename(state = "state_name",
         county = "name") %>% 
  left_join(county_vax) 

us_counties %>% 
  filter(!state_abbr %in% c("AK", "HI")) %>% 
  ggplot()+
  geom_sf(aes(fill = mmr_county))

dat %>% 
  pivot_longer(xrel:xper, names_to = "reason", values_to = "percentage_excused") %>% 
  group_by(reason) %>% 
  summarize(perc = weighted.mean(percentage_excused)) %>% 
  ggplot(aes(x = reason, y = perc))+
  geom_bar(stat = "identity")
