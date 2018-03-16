att <- cces %>% 
  filter(pew_churatd_10 < 7) %>% 
  filter(pew_churatd_14 < 7) %>% 
  mutate(att10 = 7 - pew_churatd_10) %>% 
  mutate(att14 = 7 - pew_churatd_14) %>% 
  mutate(diff =att14 - att10) %>% 
  count(diff) %>% 
  mutate(pct = prop.table(n))

# att <- att %>% 
#   mutate(diff = as.factor(diff)) %>% 
#   mutate(diff = recode(diff, "0 = 'Attending the Same'; 5 = 'Attending Much More'; -5 = 'Attending Much Less' ")) 



bar_rb <- function(base_size = 25, base_family = "Product Sans") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "Product Sans", size = 54, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "Product Sans", size = 20, vjust =-1),
       plot.caption = element_text(family = "Product Sans", size =20),
       axis.title.x =  element_text(family = "Product Sans", size =32),
       axis.title.y =  element_text(family = "Product Sans", size =32), 
       axis.text.x = element_text(family = "Product Sans", size =24), 
       legend.text=element_text(size=36)
)
}

att <- att %>% 
  mutate(pct = round(pct, 3))

att %>% 
  ggplot(., aes(x=diff, y= pct, fill = pct)) + geom_col() + 
  scale_x_continuous(breaks = -5:5, labels = c("Attending Much Less", "", "", "", "", "Attending the Same","", "", "", "", "Attending Much More")) +
  bar_rb() +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  theme(legend.position="none") +
  labs(x = "Change in Attendance", y = "Percent of Population", title = "Do People Change How Much They Attend?", subtitle = "Each bar represents a one increment change in church attendance from the 2010 baseline", caption = "Data: CCES Panel (2010-2014)")


ggsave(file="att_bars.png", type = "cairo-png", width = 26, height = 15)

