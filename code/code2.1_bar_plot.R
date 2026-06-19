library(tidyverse)
library(glue)

t_data <- read_csv("data/climate_spiral.csv", na = "***") %>%
  select(year=Year, t_diff= `J-D`)%>%
  drop_na()

annotation <- t_data %>%
  arrange(year)%>%
  slice(1,n())%>%
  mutate(t_diff=0,
         x=year + c(-5,5))
max_t_diff <- format(round(max(t_data$t_diff),1), nsmall=1)


t_data %>%
  ggplot(aes(x= year, y= t_diff, fill= t_diff))+
  geom_col(show.legend=FALSE)+ #geom_col() makes a bar chart where the bar height equals the actual value in your data
  #geom_text(data= annotation, aes(x=x,label=year), color= "white")+
  scale_x_continuous(
    breaks= seq(1880,2026, by=20),
    limits= c(1880,2026),
    expand = expansion(mult= c(0.06,0))
  )+
  
  scale_y_continuous(
    breaks= seq(-0.9,1.5, by=0.3),
    limits =c(-0.9,1.5),
    label= label_number(accuracy = 0.1),
  )+
  
  
  
  geom_text(x= 1880, y=1.3, hjust=0, 
            label= glue("Global temperature has increased by over {max_t_diff}\u00B0C since {min(t_data$year)}"),
            color= "white")+
 
  scale_fill_stepsn(colors=c("darkblue","white","darkred"),
                    values= rescale(c(min(t_data$t_diff),0, max(t_data$t_diff))),
                    limits=c(min(t_data$t_diff), max(t_data$t_diff)),
                    n.breaks=20)+
  
  
  #theme_void()+ # will delete x and y axis, will make just the bars
  
  theme(
    plot.background = element_rect(fill='black',color=NA),
    panel.background = element_rect(fill="black", color=NA),
    
    panel.grid.major= element_blank(),
    panel.grid.minor= element_blank(),
    axis.line= element_line(color="white"),
    axis.text= element_text(color="white"),
    axis.ticks= element_line(color="white"),
    legend.text= element_text(color="white")
  )


ggsave("figures/temp_bar_plot2.png", width=7, height=4)



