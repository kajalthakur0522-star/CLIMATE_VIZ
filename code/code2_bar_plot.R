library(tidyverse)


t_data <- read_csv("data/climate_spiral.csv", na = "***") %>%
  select(year=Year, t_diff= `J-D`)%>%
  drop_na()
  
  annotation <- t_data %>%
  arrange(year)%>%
  slice(1,n())%>%
  mutate(t_diff=0)

t_data %>%
ggplot(aes(x= year, y= t_diff))+
geom_col()+ #geom_col() makes a bar chart where the bar height equals the actual value in your data
geom_text(data= annotation, aes(label=year))+
theme_void() # will delete x and y axis, will make just the bars


