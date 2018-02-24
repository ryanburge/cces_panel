
test <- cces %>% 
  filter(religpew_baptist_10 ==1 & religpew_protestant_10 ==1) %>% 
  select(caseid, relig10, relig12, relig14)

test <- melt(test, id = c("caseid")) %>% arrange(caseid) 


test <- test %>% 
  mutate(value = recode(value, "1 = 'Protestant'; 
                                2 = 'Catholic'; 
                                3 = 'Other Religion'; 
                                4 = 'Other Religion'; 
                                5 = 'Other Religion'; 
                                6 = 'Other Religion'; 
                                7 = 'Other Religion'; 
                                8 = 'Other Religion'; 
                                9 = 'No Religion'; 
                                10 = 'No Religion'; 
                                11 = 'No Religion';
                                101 = 'Baptist';
                                102 = 'Other Protestant';
                                103 = 'Nondenominational';
                                104 = 'Other Protestant';
                                105 = 'Other Protestant';
                                106 = 'Other Protestant'; 
                                107 = 'Other Protestant';
                                108 = 'Other Protestant';
                                109 = 'Other Protestant';
                                110 = 'Other Protestant';
                                111 = 'Other Protestant';
                                112 = 'Other Protestant';
                                113 = 'Other Protestant';
                                190 = 'Other Protestant';
                                201 = 'Southern Baptist';
                                202:290 = 'Other Baptist'"))



test <- test %>% 
  mutate(variable = recode(variable, "'relig10'= '2010'; 'relig12'= '2012'; 'relig14'= '2014'"))


test<- test %>% 
  group_by(caseid) %>% 
  filter(all(value != 0)) %>% 
  filter(all(value != 12)) %>% 
  ungroup(caseid)

test %>% 
  ggplot(., aes(x = variable, stratum = value, alluvium = caseid, fill = value, label = value)) +
  geom_stratum() +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft") + bar_rb() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_manual(values = my.cols) +
  geom_label(fill = "white", stat = "stratum", size = 7, colour = "black") +
  labs(x= "Year", y = "Number of Respondents", title = "How Do Southern Baptists Leave?", caption = "Data: CCES Panel (2010-2014)") + theme(legend.position="none")

ggsave(file="3bar_sbc.png", type = "cairo-png", width = 18, height = 21)



test <- cces %>% 
  filter(religpew_baptist_14 ==1 & religpew_protestant_14 ==1 & religpew_14 ==1) %>% 
  select(caseid, relig10, relig12, relig14)

test <- melt(test, id = c("caseid")) %>% arrange(caseid) 


test <- test %>% 
  mutate(value = recode(value, "1 = 'Protestant'; 
                        2 = 'Catholic'; 
                        3 = 'Other Religion'; 
                        4 = 'Other Religion'; 
                        5 = 'Other Religion'; 
                        6 = 'Other Religion'; 
                        7 = 'Other Religion'; 
                        8 = 'Other Religion'; 
                        9 = 'No Religion'; 
                        10 = 'No Religion'; 
                        11 = 'No Religion';
                        101 = 'Baptist';
                        102 = 'Other Protestant';
                        103 = 'Nondenominational';
                        104 = 'Other Protestant';
                        105 = 'Other Protestant';
                        106 = 'Other Protestant'; 
                        107 = 'Other Protestant';
                        108 = 'Other Protestant';
                        109 = 'Other Protestant';
                        110 = 'Other Protestant';
                        111 = 'Other Protestant';
                        112 = 'Other Protestant';
                        113 = 'Other Protestant';
                        190 = 'Other Protestant';
                        201 = 'Southern Baptist';
                        202:290 = 'Other Baptist'"))



test <- test %>% 
  mutate(variable = recode(variable, "'relig10'= '2010'; 'relig12'= '2012'; 'relig14'= '2014'"))


test<- test %>% 
  group_by(caseid) %>% 
  filter(all(value != 0)) %>% 
  filter(all(value != 12)) %>% 
  ungroup(caseid)




test %>% 
  # filter(variable != 2012) %>% 
  ggplot(., aes(x = variable, stratum = value, alluvium = caseid, fill = value, label = value)) +
  geom_stratum() +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft") + bar_rb() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_manual(values = my.cols) +
  geom_label(fill = "white", stat = "stratum", size = 7, colour = "black") +
  labs(x= "Year", y = "Number of Respondents", title = "Where Do Southern Baptists Come From?", caption = "Data: CCES Panel (2010-2014)") + theme(legend.position="none")

ggsave(file="3bar_to_sbc.png", type = "cairo-png", width = 18, height = 21)



cces %>% 
  mutate(relig10 = recode(relig10, "1 = 'Protestant'; 
                        2 = 'Catholic'; 
                        3 = 'Other Religion'; 
                        4 = 'Other Religion'; 
                        5 = 'Other Religion'; 
                        6 = 'Other Religion'; 
                        7 = 'Other Religion'; 
                        8 = 'Other Religion'; 
                        9 = 'No Religion'; 
                        10 = 'No Religion'; 
                        11 = 'No Religion';
                        101 = 'Baptist';
                        102 = 'Other Protestant';
                        103 = 'Nondenominational';
                        104 = 'Other Protestant';
                        105 = 'Other Protestant';
                        106 = 'Other Protestant'; 
                        107 = 'Other Protestant';
                        108 = 'Other Protestant';
                        109 = 'Other Protestant';
                        110 = 'Other Protestant';
                        111 = 'Other Protestant';
                        112 = 'Other Protestant';
                        113 = 'Other Protestant';
                        190 = 'Other Protestant';
                        201 = 'Southern Baptist';
                        202:290 = 'Other Baptist'")) %>% 
  filter(religpew_baptist_14 ==1) %>% tabyl(relig10)


cces %>% 
  mutate(relig14 = recode(relig14, "1 = 'Protestant'; 
                        2 = 'Catholic'; 
                        3 = 'Other Religion'; 
                        4 = 'Other Religion'; 
                        5 = 'Other Religion'; 
                        6 = 'Other Religion'; 
                        7 = 'Other Religion'; 
                        8 = 'Other Religion'; 
                        9 = 'No Religion'; 
                        10 = 'No Religion'; 
                        11 = 'No Religion';
                        101 = 'Baptist';
                        102 = 'Other Protestant';
                        103 = 'Nondenominational';
                        104 = 'Other Protestant';
                        105 = 'Other Protestant';
                        106 = 'Other Protestant'; 
                        107 = 'Other Protestant';
                        108 = 'Other Protestant';
                        109 = 'Other Protestant';
                        110 = 'Other Protestant';
                        111 = 'Other Protestant';
                        112 = 'Other Protestant';
                        113 = 'Other Protestant';
                        190 = 'Other Protestant';
                        201 = 'Southern Baptist';
                        202:290 = 'Other Baptist'")) %>% 
  filter(religpew_baptist_10 ==1) %>% tabyl(relig14)

cces %>% filter(religpew_baptist_10 ==1 & religpew_protestant_10 ==1) %>% count(relig14) %>% as.data.frame() %>% mutate(relig14 = recode(relig14, "1 = 'Protestant'; 
                        2 = 'Catholic'; 
                                                                                                                  3 = 'Other Religion'; 
                                                                                                                  4 = 'Other Religion'; 
                                                                                                                  5 = 'Other Religion'; 
                                                                                                                  6 = 'Other Religion'; 
                                                                                                                  7 = 'Other Religion'; 
                                                                                                                  8 = 'Other Religion'; 
                                                                                                                  9 = 'No Religion'; 
                                                                                                                  10 = 'No Religion'; 
                                                                                                                  11 = 'No Religion';
                                                                                                                  101 = 'Baptist';
                                                                                                                  102 = 'Other Protestant';
                                                                                                                  103 = 'Nondenominational';
                                                                                                                  104 = 'Other Protestant';
                                                                                                                  105 = 'Other Protestant';
                                                                                                                  106 = 'Other Protestant'; 
                                                                                                                  107 = 'Other Protestant';
                                                                                                                  108 = 'Other Protestant';
                                                                                                                  109 = 'Other Protestant';
                                                                                                                  110 = 'Other Protestant';
                                                                                                                  111 = 'Other Protestant';
                                                                                                                  112 = 'Other Protestant';
                                                                                                                  113 = 'Other Protestant';
                                                                                                                  190 = 'Other Protestant';
                                                                                                                  201 = 'Southern Baptist';
                                                                                                                  202:290 = 'Other Baptist'")) %>% group_by(relig10) %>% count()
