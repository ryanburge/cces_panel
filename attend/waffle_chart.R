
## Using born again status is a better approach 

cces %>% group_by(pew_bornagain_10) %>% count(pew_bornagain_14)
cces %>% filter(diff < 0) %>% group_by(pew_bornagain_10) %>% count(pew_bornagain_14)
##50

cces %>% filter(diff > 0) %>% group_by(pew_bornagain_10) %>% count(pew_bornagain_14)
##96

cces %>% filter(diff == 0) %>% group_by(pew_bornagain_10) %>% count(pew_bornagain_14)
##154

parts <- c(`Attending the Same`= 154 , `Attending More`= 96, `Attending Less`= 50)

waffle(parts, legend_pos = "bottom") + 
  bar_rb() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  labs(title = "Does Having a Born-Again Experience Change Attendance?", caption = "Data: CCES Panel (2010-2014)", subtitle = "One Square = One Person Who Became Born Again Between 2010 and 2014") +
  theme(plot.margin=unit(c(2,1,2,1),"cm")) + theme(plot.title = element_text(family = "Product Sans", size = 42, vjust =2, face = "bold"))

ggsave(file="D://cces_panel/attend/waffles.png", type = "cairo-png", width = 18, height = 12)


