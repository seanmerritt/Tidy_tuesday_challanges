measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

dat <- measles
pacman::p_load(tidyverse, jmv, sf, maps, USAboundaries, USAboundariesData, ggrepel, rnaturalearth, geofacet, RColorBrewer,wesanderson, leaflet)

## I need to get rid of the NAs in the x files 
dat %>% 
  filter(mmr == -1) %>% 
  mutate(mmr = ifelse(mmr == -1, NA, mmr), )

dat <- dat %>% 
  select(state, county, mmr, overall, xrel, xmed, xper, enroll) %>% 
  mutate(xrel = ifelse(is.na(xrel), 0, xrel),
         xper = ifelse(is.na(xper), 0, xper),
         xmed = ifelse(is.na(xmed), 0, xmed), 
         mmr = ifelse(mmr == -1, NA, mmr),
         overall = ifelse(overall == -1, NA, overall)) 
  
  

dat %>% 
  select(mmr, xrel, xper, xmed) %>% 
  descriptives(  )

county_vax  <- dat %>% 
  group_by(state, county) %>% 
  summarize(mmr_county = weighted.mean(mmr, na.rm = T),
            no_vax = weighted.mean(xmed+xper +xrel, na.rm = T),
            overall_county = weighted.mean(overall, na.rm = T), 
            Medical = weighted.mean(xmed, na.rm = T), 
            Personal = weighted.mean(xper, na.rm = T), 
            Religion = weighted.mean(xrel, na.rm = T))


states <- st_as_sf(us_states())


us_counties <- st_as_sf(us_counties()) %>% 
  rename(state = "state_name",
         county = "name") %>% 
  left_join(county_vax) 

county_vax %>% 
  filter(state == "Idaho")

us_counties %>% 
  filter(!state_abbr %in% c("AK", "HI")) %>% 
  ggplot()+
  geom_sf(aes(fill = Medical))

us_counties %>% 
  filter(state_abbr == "CA") %>% 
  ggplot()+
  geom_sf(aes(fill = mmr_county))

us_counties %>% 
  filter(state_abbr == "CA") %>% 
  ggplot()+
  geom_sf(aes(fill =  Medical))

dat %>% 
# select(reason, percentage_excused, county, state) %>% 
  filter(xrel > 0 | xmed > 0 |  xper > 0) %>% 
  rename(Medical = "xmed", 
         Religion = "xrel",
         Personal = "xper") %>% 
pivot_longer(Religion:Personal, names_to = "reason", values_to = "percentage_excused") %>% 
  mutate(count = (percentage_excused/100)*enroll, 
         total = sum(count, na.rm = T)) %>% 
  group_by(reason) %>% 
  summarize(sum = sum(count, na.rm = T), percentage = sum(count, na.rm = T)/61287) %>% 
  mutate(coloring = ifelse(reason == "Personal", 1, 0)) %>% 
  ggplot(aes(x = reason, y = percentage, fill = factor(coloring)))+
  geom_bar(stat = "identity", color = "white")+
  geom_text(aes(label = round(sum, 0)), vjust = -.5)+
  ylim(0, .7)+
  theme_classic()+
  theme(legend.position = "none")+
  scale_fill_manual(values = c( "pink","red", "pink"))+
  labs(y = "Percentage of those excused", x = "Reason not Vaccinated", title = "Excuses for No Vaccine")

ggsave("Vaxinations.jpeg", width = 5, height = 5)


  
  
 
