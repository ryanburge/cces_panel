
cces <- cces %>% 
  mutate(ba_10 = recode(pew_bornagain_10, "1=1; else=0")) %>% 
  mutate(ba_12 = recode(pew_bornagain_12, "1=1; else=0")) %>% 
  mutate(ba_14 = recode(pew_bornagain_14, "1=1; else=0")) %>% 
  mutate(prot_10 = recode(religpew_10, "1=1; else=0")) %>% 
  mutate(prot_12 = recode(religpew_12, "1=1; else=0")) %>% 
  mutate(prot_14 = recode(religpew_14, "1=1; else=0")) %>% 
  mutate(baprot_10 = ba_10 + prot_10) %>% 
  mutate(baprot_12 = ba_12 + prot_12) %>% 
  mutate(baprot_14 = ba_14 + prot_14) %>% 
  mutate(baprot_10 = recode(baprot_10, "2=1; else=0")) %>% 
  mutate(baprot_12 = recode(baprot_12, "2=1; else=0")) %>% 
  mutate(baprot_14 = recode(baprot_14, "2=1; else=0")) %>% 
  mutate(nba_10 = recode(pew_bornagain_10, "2=1; else=0")) %>% 
  mutate(nba_12 = recode(pew_bornagain_12, "2=1; else=0")) %>% 
  mutate(nba_14 = recode(pew_bornagain_14, "2=1; else=0")) %>% 
  mutate(nbaprot_10 = nba_10 + prot_10) %>% 
  mutate(nbaprot_12 = nba_12 + prot_12) %>% 
  mutate(nbaprot_14 = nba_14 + prot_14) %>% 
  mutate(nbaprot_10 = recode(nbaprot_10, "2=1; else=0")) %>% 
  mutate(nbaprot_12 = recode(nbaprot_12, "2=1; else=0")) %>% 
  mutate(nbaprot_14 = recode(nbaprot_14, "2=1; else=0")) 
  
ba <- cces %>% 
  filter(baprot_10 ==1) %>% 
  count(religpew_14) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(label = c("Born Again Protestants"))


nba <- cces %>% 
  filter(nbaprot_10 ==1) %>% 
  count(religpew_14) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(label = c("Not Born Again Protestants"))


def <- bind_rows(ba, nba) %>% na.omit()


def <- def %>% 
  mutate(relig = recode(religpew_14, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'; 12 = 'Something Else'"))


def <- def %>% 
  mutate(relig = fct_relevel(relig, "Protestant", "Catholic", "Agnostic", "Atheist", "Nothing", "Something Else", "Orthodox", "Mormon", "Jewish", "Hindu", "Muslim", "Buddhist"))



def %>% 
  filter(pct > .01) %>% 
  ggplot(., aes(1, pct)) + geom_col(aes(fill= fct_rev(relig)), colour = "black") + coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  scale_y_continuous(labels = scales::percent) + facet_grid(label ~ .)  +
  flip_bar_rb() +  
  guides(fill = guide_legend(reverse = TRUE)) + 
  labs(x= "", y = "Percent of the Population", title = "How Does a Born Again Identity Reduce Defection?", subtitle = "How Religious Affiliation Shifts in 2014 Depending on Born Again Status in 2010", caption = "Data: CCES Panel (2010-2014)")  + 
  scale_fill_brewer(palette = "Spectral")

ggsave(file="ba_nonba_defect.png", type = "cairo-png", width = 21, height = 12)


test <- cces %>% 
  select(caseid, ba_10, ba_12, ba_14)

test <- melt(test, id = c("caseid")) %>% arrange(caseid) 

test <- test %>% 
  mutate(value = recode(value, "1 = 'Born Again'; 0 = 'Not Born Again'"))

# 
# test <- test %>% 
#   mutate(value = fct_relevel(value, "Agnostic", "Atheist", "Nothing", "Catholic", "Protestant", "Orthodox", "Mormon", "Jewish", "Hindu", "Muslim", "Buddhist"))


test <- test %>% 
  mutate(variable = recode(variable, "'ba_10'= '2010'; 'ba_12'= '2012'; 'ba_14'= '2014'"))

small <- test %>% head(500)


small %>% 
  ggplot(., aes(x = variable, stratum = value, alluvium = caseid, fill = value, label = value)) +
  geom_stratum() +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft") + bar_rb() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_manual(values = my.cols) +
  geom_label(fill = "white", stat = "stratum", size = 7, colour = "black") +
  labs(x= "Year", y = "Number of Respondents", title = "Do Born Again Christians Change Their Status?", caption = "Data: CCES Panel (2010-2014)") + theme(legend.position="none") 


ggsave(file="3bar_ba_nonba_300.png", type = "cairo-png", width = 18, height = 21, dpi= 300)

cces %>% 
  filter(religpew_14 ==2) %>% 
  filter(religpew_14 != 12) %>% 
  tabyl(religpew_10)

cces 






