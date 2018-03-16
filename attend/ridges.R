sbc <- cces %>% 
  filter(religpew_baptist_14 ==1) %>% 
  mutate(group = c("Southern Baptist")) %>% 
  select(group, diff)


nd <- cces %>% 
  filter(religpew_protestant_14 ==3) %>% 
  mutate(group = c("Nondenominational")) %>% 
  select(group, diff)


umc <- cces %>% 
  filter(religpew_methodist_14 ==1) %>% 
  mutate(group = c("United Methodist")) %>% 
  select(group, diff)

elca <- cces %>% 
  filter(religpew_lutheran_14 ==1) %>% 
  mutate(group = c("ELCA")) %>% 
  select(group, diff)

pcusa <- cces %>% 
  filter(religpew_presby_14 ==1) %>% 
  mutate(group = c("PCUSA")) %>% 
  select(group, diff)

epis<- cces %>% 
  filter(religpew_protestant_14 ==7) %>% 
  mutate(group = c("Episcopal")) %>% 
  select(group, diff)

cath <- cces %>% 
  filter(religpew_14 =="Catholic") %>% 
  mutate(group = c("Catholic")) %>% 
  select(group, diff)

mormon <- cces %>% 
  filter(religpew_14 =="Mormon") %>% 
  mutate(group = c("Mormon")) %>% 
  select(group, diff)

rplot <- bind_rows(sbc, nd, umc, elca, pcusa, epis, cath, mormon)


rplot %>% 
  mutate(attend = as.numeric(diff), group  = as.factor(group)) %>% 
  ggplot(., aes(x=diff, y=group, height = ..density.., fill = group, alpha =.4)) + geom_density_ridges(stat = "density") +
  mean_rb() +
  scale_x_continuous(breaks = c(-4, 0, 4),  label = c("Attending Much Less","Attending the Same","Attending Much More")) +
  theme(axis.text.x = element_text(family = "Product Sans", size =16, angle = 45, hjust = 1)) + theme(legend.position="none") +
  labs(x = "", y ="", title = "Distribution of Changes in Church Attendance", caption = "Data: CCES Panel (2010-2014)", subtitle = "Change is the difference between the 2010 reported attendance and the 2014 report attendance") +
  theme(plot.title = element_text(size=34)) + 
  theme(axis.text.x = element_text(family = "Product Sans", size =22, angle = 0)) +
  scale_fill_brewer(palette="Dark2") + theme(axis.text.x = element_text(family = "Product Sans", size =26, hjust = .50))

ggsave(file="D://cces_panel/attend/ridges.png", type = "cairo-png", width = 22, height = 14)
