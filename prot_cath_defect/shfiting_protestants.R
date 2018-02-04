

test <- cces %>% 
  filter(religpew_protestant_10 ==1 & religpew_protestant_14 ==1) %>% 
  select(caseid, religpew_protestant_10, religpew_protestant_12, religpew_protestant_14)
 
test <- melt(test, id = c("caseid")) %>% arrange(caseid) 


test <- test %>% 
  mutate(value = recode(value, "1 = 'Baptist'; 2 = 'Methodist'; 3 = 'Nondenominational'; 4 = 'Lutheran'; 5 = 'Presbyterian'; 6 = 'Pentecostal'; 7 = 'Episcopalian'; 8 = 'DoC'; 9 = 'UCC'; 10 = 'Holiness'; 11 = 'Reformed'; 12 = 'Adventist'; 13 = 'Jehovahs Witness'"))

 
 test<- test %>% 
   group_by(caseid) %>% 
   filter(all(value != "90")) %>% 
   filter(all(value != "Adventist")) %>% 
   filter(all(value != "Jehovahs Witness")) %>% 
   filter(all(value != "Reformed")) %>% 
   filter(all(value != "Holiness")) %>% 
   ungroup(caseid)

# test <- test %>% 
#   mutate(value = fct_relevel(value, "Agnostic", "Atheist", "Nothing", "Catholic", "Protestant", "Orthodox", "Mormon", "Jewish", "Hindu", "Muslim", "Buddhist"))


test <- test %>% 
  mutate(variable = recode(variable, "'religpew_protestant_10'= '2010'; 'religpew_protestant_12'= '2012'; 'religpew_protestant_14'= '2014'"))

test %>% 
  ggplot(., aes(x = variable, stratum = value, alluvium = caseid, fill = value, label = value)) +
  geom_stratum() +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft") + bar_rb() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_manual(values = my.cols) +
  geom_label(fill = "white", stat = "stratum", size = 7, colour = "black") +
  labs(x= "Year", y = "Number of Respondents", title = "The Shifting Protestant Landscape", caption = "Data: CCES Panel (2010-2014)") + theme(legend.position="none")



ggsave(file="3bar_prot2prot.png", type = "cairo-png", width = 18, height = 21)