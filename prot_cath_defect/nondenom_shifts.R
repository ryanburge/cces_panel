##Make the nondenom shift graph

relig <- cces %>% 
  filter(religpew_protestant_10 ==3) %>% 
  tabyl(religpew_14)


relig <- relig %>% 
  mutate(trad = recode(religpew_14, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'")) %>% 
  filter(trad != 12) %>% 
  mutate(label = c("All Religions"))
  
  
relig %>% 
  filter(valid_percent > .005) %>% 
  ggplot(., aes(x=reorder(label, -valid_percent), y = valid_percent, fill = factor(trad, levels =c("Atheist", "Agnostic", "Nothing", "Catholic", "Protestant")))) + geom_col(color = "black") + coord_flip() +
  scale_fill_brewer(palette = "Spectral") + flip_bar_rb() +
  scale_x_discrete(labels = c("")) + xlab("") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(title = "Where Do Nondenominational Christians Migrate?", subtitle = "Individuals Who Reported a Nondenominational Christian Affiliation in 2010", y = "Percent of Nondenominational Christians in 2014")

ggsave(file="D://cces_panel/prot_cath_defect/nondenoms.png", type = "cairo-png", width = 15, height = 3)



relig <- cces %>% 
  filter(religpew_protestant_10 ==3) %>% 
  tabyl(religpew_protestant_14)







relig <- relig %>% 
  mutate(trad = recode(religpew_protestant_14, "1 = 'Baptist'; 2 = 'Methodist'; 3 = 'Nondenominational'; 4 = 'Lutheran'; 5 = 'Presbyterian'; 6 = 'Pentecostal'; 7 = 'Episcopalian'; 8 = 'DoC'; 9 = 'UCC'; 10 = 'Holiness'; 11 = 'Reformed'; 12 = 'Adventist'; 13 = 'Jehovahs Witness'; 90 = 'None of These'")) %>%   
  mutate(label = c("All Religions"))


relig %>% 
  filter(percent > .01) %>% 
  ggplot(., aes(x=reorder(label, -percent), y = percent, fill = factor(trad, levels =c("Reformed","Presbyterian", "DoC","Lutheran","Methodist", "Pentecostal", "None of These", "Baptist", "Nondenominational")))) + geom_col(color = "black") + coord_flip() +
  scale_fill_brewer(palette = "Spectral") + flip_bar_rb() +
  scale_x_discrete(labels = c("")) + xlab("") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(title = "Where Do Nondenominational Christians Migrate?", subtitle = "Individuals Who Reported a Nondenominational Christian Affiliation in 2010", y = "Percent of Nondenominational Christians in 2014")

ggsave(file="D://cces_panel/prot_cath_defect/nondenoms_protestant.png", type = "cairo-png", width = 15, height = 4)

