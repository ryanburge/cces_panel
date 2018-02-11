

test <- cces %>% 
  filter(religpew_10 ==1) %>% 
  select(caseid, religpew_10, religpew_12, religpew_14)

test <- melt(test, id = c("caseid")) %>% arrange(caseid) 

test <- test %>% 
  mutate(value = recode(value, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'"))


test<- test %>% 
  group_by(caseid) %>% 
  filter(all(value != "12")) %>% 
  ungroup(caseid)

test <- test %>% 
  mutate(value = fct_relevel(value, "Agnostic", "Atheist", "Nothing",  "Protestant", "Catholic", "Orthodox", "Mormon", "Jewish", "Hindu", "Muslim", "Buddhist"))


test <- test %>% 
  mutate(variable = recode(variable, "'religpew_10'= '2010'; 'religpew_12'= '2012'; 'religpew_14'= '2014'"))



my.cols <- brewer.pal(9, "Set1")

my.cols[4] <- "#FF7F00"
my.cols[5] <- "#984EA3"
my.cols[10] <- "black"
my.cols[11] <- "black"

test %>% 
  ggplot(., aes(x = variable, stratum = value, alluvium = caseid, fill = value, label = value)) +
  geom_stratum() +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft") + bar_rb() +
  scale_fill_manual(values = my.cols) +
  scale_color_manual(values = my.cols) +
  geom_label(fill = "white", stat = "stratum", size = 7, colour = "black") +
  labs(x= "Year", y = "Number of Respondents", title = "How Have Protestants Shifted?", caption = "Data: CCES Panel (2010-2014)") + theme(legend.position="none") +
  annotate("text", x = 3.3, y = 3900, label = "7.4%", size = 10) +
  annotate("text", x = 3.3, y = 25, label = "1.4%", size = 10) +
  annotate("text", x = 3.3, y = 1900, label = "91.2%", size = 10) 

ggsave(file="3bar_protestants.png", type = "cairo-png", width = 18, height = 21, dpi = 300)

cces %>% 
  filter(religpew_10 ==1) %>% 
  filter(religpew_14 != 12) %>% 
  tabyl(religpew_14)



