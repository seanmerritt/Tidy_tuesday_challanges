pacman::p_load(tidyverse)

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

dat <- food_consumption

dat %>% 
  ggplot(aes(x = consumption, color = food_category))+
  geom_density()


p <- dat %>% 
  pivot_wider(names_from = food_category, 
              values_from = consumption) %>% 
  mutate(Cows = rowSums(.[names(.)[c(5,7)]], na.rm = TRUE),
         Chicken = rowSums(.[names(.)[c(4,8)]], na.rm = TRUE),
         Plants = rowSums(.[names(.)[c(10:13)]], na.rm = TRUE)) %>% 
  rename(Goat = "Lamb & Goat") %>% 
  pivot_longer(Pork:Plants, 
               names_to = "Food", 
               values_to = "Consumption") %>% 
  na.omit() %>% 
  filter(Food == "Cows" | Food ==  "Chicken" | Food == "Plants" | Food == "Pork" | Food == "Fish" | Food == "Goat") %>% 
  filter(Consumption != 0) %>% 
  group_by(Food)%>% 
mutate(co2_per_cons = sum(co2_emmission)/sum(Consumption),
       insight = case_when(Food == "Goat" ~ "Goat",
                           Food == "Cows" ~ "Cows",
                           Food != "Goat" | Food != "Cows" ~ "Else"))
p <- p %>% 
  group_by(Food, insight) %>% 
  summarise(co2 = sum(co2_emmission)/sum(Consumption) )

p %>% 
  ggplot(aes(x = Food, y = co2, fill = insight))+
  geom_bar(stat = "identity")+
  theme_classic()+
  labs(y = "kg Co2 emmited per 
 kg consumed")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c( "gold3","gray", "gold4"))

ggsave("Co2_Emmissions_and_Food.jpeg", width = 5, height = 5)



