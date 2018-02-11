
gss <- read.fst("C://gss.fst")


gss$newid <- paste(gss$id, gss$year, sep = "_")


test <- gss %>% 
  filter(denom16 == 14) %>% 
  select(newid, relig, relig16)

test <- melt(test, id = c("newid")) %>% arrange(newid) 


test <- test %>% 
  mutate(value = recode(value, "1 = 'Protestant'; 
                                2 = 'Catholic'; 
                                3 = 'Jewish'; 
                                4 = 'None'; 
                                5 = 'Other'; 
                                6 = 'Buddhist'; 
                                7 = 'Hindu'; 
                                8 = 'Other Eastern'; 
                                9 = 'Muslim'; 
                                10 = 'Orthodox'; 
                                11 = 'Christian';
                                12 = 'Native American';
                                13 = 'Inter-denominational';
                                98 = 'Dont Know'"))




test<- test %>% 
  group_by(newid) %>% 
  filter(all(value != "Other Eastern")) %>% 
  filter(all(value != "Muslim")) %>% 
  filter(all(value != "Orthodox")) %>% 
  filter(all(value != "Native American")) %>% 
  filter(all(value != "Inter-denominational")) %>% 
  ungroup(newid)


test <- test %>% 
  mutate(variable = recode(variable, "'relig16'= 'At 16 Years Old'; 'relig'= 'As Adult'"))


test <- test %>% 
   mutate(variable = fct_relevel(variable, "At 16 Years Old", "As Adult"))



test %>% 
  ggplot(., aes(x = variable, stratum = value, alluvium = newid, fill = value, label = value)) +
  geom_stratum() +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft") + bar_rb() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_manual(values = my.cols) +
  geom_label(fill = "white", stat = "stratum", size = 7, colour = "black") +
  labs(x= "Year", y = "Number of Respondents", title = "The Shifting Protestant Landscape", caption = "Data: CCES Panel (2010-2014)") + theme(legend.position="none")

ggsave(file="sbc.png", type = "cairo-png", width = 18, height = 21)


