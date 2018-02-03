



test <- cces %>% 
  filter(evangelical_10 ==1 & evangelical_14 ==1) %>% 
  select(caseid, pid7_10, pid7_12, pid7_14) 

all <- test %>% 
  group_by(pid7_10) %>% 
  count(pid7_14) %>% 
  mutate(pct = prop.table(n)) %>% 
  ungroup(pid7_10)
 

all$change <- 1

all$change <- ifelse(all$pid7_10 == all$pid7_14, 0, all$change)

all <- all %>% na.omit()
all <- all %>% 
  filter(pid7_10 <8) %>% 
  filter(pid7_14 <8)


all <- all %>% 
  mutate(pid7_10 = recode(pid7_10, " 1= 'Strong Democrat'; 2= 'Not Strong Democrat'; 3= 'Lean Democrat'; 4 = 'Independent'; 5=  'Lean Republican' ; 6 = 'Not Strong Republican'; 7 = 'Strong Republican'")) %>% 
  mutate(pid7_14 = recode(pid7_14, " 1= 'Strong Democrat'; 2= 'Not Strong Democrat'; 3= 'Lean Democrat'; 4 = 'Independent'; 5=  'Lean Republican' ; 6 = 'Not Strong Republican'; 7 = 'Strong Republican'")) %>% 
  mutate(pid10 = fct_relevel(pid7_10, "Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican")) %>% 
  mutate(pid14 = fct_relevel(pid7_14, "Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican")) 


ggplot(all, aes(weight = n, axis1 = pid10, axis2 = pid14)) +
  geom_alluvium(aes(fill = factor(pid10)), width = 1/6) +
  geom_stratum(width = 1/6, fill = "azure3", color = "black") +
  geom_label(stat = "stratum", label.strata = TRUE, size = 7) +
  bar_rb() +
  scale_fill_brewer(palette = "RdBu", direction = -1) +
  scale_color_brewer(palette = "RdBu") +
  scale_x_continuous(breaks = 1:2, labels = c("2010", "2014")) + 
  theme(legend.position="none") +
  labs(x = "Year", y = "Number of Respondents", title = "Has Evangelicals Partisanship Shifted?", caption = "Data: CCES Panel (2010-2014)")


### USE THIS ONE


test <- cces %>% 
  filter(evangelical_10 ==1 & evangelical_14 ==1) %>% 
  select(caseid, pid7_10, pid7_12, pid7_14) 


test <- melt(test, id = c("caseid")) %>% arrange(caseid) 


test<- test %>% 
  group_by(caseid) %>% 
  filter(all(value != 8)) %>% 
  filter(all(value != "NA")) %>% 
  ungroup(caseid)


test <- test %>% 
  mutate(variable = recode(variable, "'pid7_10'= '2010'; 'pid7_12'= '2012'; 'pid7_14'= '2014'"))



test <- test %>% 
  mutate(value = recode(value, " 1= 'Strong Democrat'; 2= 'Not Strong Democrat'; 3= 'Lean Democrat'; 4 = 'Independent'; 5=  'Lean Republican' ; 6 = 'Not Strong Republican'; 7 = 'Strong Republican'")) %>% 
  mutate(value = fct_relevel(value, "Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican")) 


my.cols <- brewer.pal(7, "RdBu")

my.cols[4] <- "#777676"

ggplot(test,
       aes(x = variable, stratum = value, alluvium = caseid,
           fill = value, label = value)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft") +
  geom_stratum() +
  geom_label(fill = "white", stat = "stratum", size = 7, colour = "black") +
  bar_rb() +
  scale_fill_manual(values = rev(my.cols)) +
  scale_color_manual(values = my.cols) +
  theme(legend.position="none") +
  labs(x = "Year", y = "Number of Respondents", title = "Has Evangelicals' Partisanship Shifted?", caption = "Data: CCES Panel (2010-2014)")

ggsave(file="evangelical_pid_shifts_3bars.png", type = "cairo-png", width = 15, height = 21)



