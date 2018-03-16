
cces <- cces %>% 
  filter(pew_churatd_10 < 7) %>% 
  filter(pew_churatd_14 < 7) %>% 
  mutate(att10 = 7 - pew_churatd_10) %>% 
  mutate(att14 = 7 - pew_churatd_14) %>% 
  mutate(diff =att14 - att10) %>% 
  mutate(diff3 = recode(diff, "-5:-1 = 'Less Attendance'; 0 = 'Same Attendance'; 1:5 = 'More Attendance'"))

cces <- cces %>% 
  mutate(religpew_10 = recode(religpew_10, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'"))

att_relig <- cces %>% 
  group_by(religpew_10) %>% 
  count(diff3) %>% 
  mutate(pct = prop.table(n)) 

att_relig <- att_relig %>% 
  filter(religpew_10 == "Protestant" | religpew_10 == "Catholic" | religpew_10 == "Agnostic" | religpew_10 == "Nothing" | religpew_10 == "Jewish") 

att_relig <- cces %>% 
  group_by(mainline_10) %>% 
  count(diff3) %>% 
  mutate(pct = prop.table(n)) %>% 
  ungroup(mainline_10) %>% 
  mutate(mainline_10 = recode(mainline_10, "1 = 'Mainline'")) %>% 
  filter(mainline_10 != 0) %>%
  rename(religpew_10 = mainline_10) %>% 
  bind_rows(att_relig)

att_relig <- cces %>% 
  group_by(evangelical_10) %>% 
  count(diff3) %>% 
  mutate(pct = prop.table(n)) %>% 
  ungroup(evangelical_10) %>% 
  mutate(evangelical_10 = recode(evangelical_10, "1 = 'Evangelical'")) %>% 
  filter(evangelical_10 != 0) %>%
  rename(religpew_10 = evangelical_10) %>% 
  bind_rows(att_relig)

att_relig <- att_relig %>% 
  filter(religpew_10 != "Protestant")

att_relig$diff3 = factor(att_relig$diff3 , levels=c('More Attendance','Same Attendance','Less Attendance'))

att_relig %>% 
  ggplot(., aes(1, pct)) + geom_col(aes(fill= diff3), colour = "black") + coord_flip() +
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("") + 
  scale_y_continuous(labels = scales::percent) + facet_grid(religpew_10 ~ .)  +
  flip_bar_rb() +  
  guides(fill = guide_legend(reverse = TRUE)) + 
  labs(x= "", y = "", title = "Attendance Changes by Religious Tradition", subtitle = "Change is between the 2010 reported attendance and the 2014 reported attendance", caption = "Data: CCES Panel (2010-2014)")  + 
  scale_fill_brewer(palette = "Set2")

ggsave(file="D://cces_panel/attend/att_bars_flip_trad.png", type = "cairo-png", width = 20, height = 14)