libarary(tidyverse)

t_diff<- read_csv("data/climate_spiral.csv", na = "***")%>%
  select(year= Year, month.abb)%>%
  pivot_longer(-year, names_to="month", values_to='t_diff')%>%
  drop_na()

last_dec<- t_diff%>%
  filter(month=='Dec')%>%
  mutate(year= year+1,
         month= "last_Dec")

next_jan<- t_diff%>%
  filter(month=="Jan")%>%
  mutate(year=year-1,
         month= "next_Jan")


t_data<- bind_rows(last_dec, t_diff, next_jan)%>% 
  mutate(month= factor(month, levels= c("last_Dec", month.abb, "next_Jan")),
         month_number= as.numeric(month)-1,
         this_year=year==2026
  )# to make x axis jan to dec
annotation<- t_data%>%
  slice_max(year)%>%
  slice_max(month_number)

t_data%>%
  ggplot(aes(x=month_number, y=t_diff, group= year, color=year, size= this_year))+
  
  geom_hline(yintercept=0, color='white')+  
  geom_line()+
  geom_text(data = annotation,
            aes(x=month_number, y=t_diff, label=year, color=year),
            inherit.aes = FALSE,
            hjust = 0, size = 5, nudge_x = 0.15, fontface = "bold") +
  #geom_hline(yintercept=0, color='white')+ #if this line is written here, yaxis=0 line will be aage bali lines ke 
  scale_x_continuous(breaks=1:12,
                     labels= month.abb,
                     sec.axis= dup_axis(name=NULL, labels=NULL))+
  scale_y_continuous(breaks= seq(-2,2,0.2),
                     sec.axis= dup_axis(name=NULL, labels=NULL))+
  scale_size_manual(breaks= c(FALSE,TRUE),
                    values= c(0.25,1), guide=NULL)+ # GUIDE=NULL se iska legend hatt jayega 
  scale_color_viridis_c( breaks= seq(1880,2020,20),
                         guide=guide_colorbar(frame.colour='white',
                                              frame.linewidth=1))+
  coord_cartesian(xlim=c(1,12))+
  labs(x=NULL,
       y= "Temperature change since pre-industrial time [\u00B0C]",
       title= "Global temperature change since 1880")+
  theme(
    panel.background = element_rect(fill= "black", color='white', ),
    plot.background = element_rect(fill="#666666"),
    panel.grid= element_blank(),
    axis.text= element_text(color="white", size=12),
    axis.ticks= element_line(color='white'),
    axis.ticks.length = unit(-3, "pt"),
    axis.title= element_text(color='white', size=13),
    plot.title= element_text(color='white', hjust=0.5, size=15), # hjsut=0,.5,1 means left center and right justified 
    legend.title= element_blank(),
    legend.background= element_rect(fill=NA),
    legend.text= element_text(color= "white"),
    legend.key.height=unit(55,'pt')
    
  )

ggsave("figures/climate_spiral.png", width= 8, height=4.5)


