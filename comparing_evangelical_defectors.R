
cces %>% 
  group_by(evangelical_10) %>% 
  count(evangelical_14) %>% 
  mutate(pct = prop.table(n))
  
d1 <- cces %>% 
  filter(evangelical_10 ==1 & evangelical_14 ==0) %>% 
  filter(pid7_10 <8) %>% 
  summarise(mean = mean(pid7_10),
            sd = sd(pid7_10), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(status = c("Defect")) %>% 
  mutate(year = c("2010"))
  

d2 <- cces %>% 
  filter(evangelical_10 ==1 & evangelical_14 ==0) %>% 
  filter(pid7_14 <8) %>% 
  summarise(mean = mean(pid7_14),
            sd = sd(pid7_14), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(status = c("Defect")) %>% 
  mutate(year = c("2014"))


d3 <- cces %>% 
  filter(evangelical_10 ==1 & evangelical_14 ==1) %>% 
  filter(pid7_10 <8) %>% 
  summarise(mean = mean(pid7_10, na.rm = TRUE),
            sd = sd(pid7_10, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(status = c("Not Defect")) %>% 
  mutate(year = c("2010"))


d4 <- cces %>% 
  filter(evangelical_10 ==1 & evangelical_14 ==1) %>% 
  filter(pid7_14 <8) %>% 
  summarise(mean = mean(pid7_14),
            sd = sd(pid7_14), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(status = c("Not Defect")) %>% 
  mutate(year = c("2014"))

total <- bind_rows(d1, d2, d3, d4)

total <- total %>% 
  mutate(year = fct_relevel(year, "2014", "2010"))

total %>% 
  # filter(type == "White Mainline") %>% 
  ggplot(., aes(x = mean, y = year))  +
  geom_point(shape=21, size =4, aes(fill = factor(year)), show.legend = FALSE) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper, colour = factor(year)), height=0, size = 1, show.legend = FALSE) + 
  scale_color_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey")) +
  scale_fill_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey")) +
  facet_grid(status~.) +
  scale_x_continuous(limits = c(1,7), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican")) +
  mean_rb() +
  labs(x= "Mean Party Identification", y = "Year", title = "How are Evangelical Defectors Different than Those Who Stay?")

ggsave(file="evangelical_pid_defectors.png", type = "cairo-png", width = 18, height = 15)


cces %>% 
  filter(evangelical_10 ==1 & evangelical_14 ==0) %>% 
  mutate(age = 2014 - birthyr_14) %>% 
  summarise(mean = mean(age, na.rm = TRUE),
            sd = sd(age, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(status = c("Defect")) 
##58.3

cces %>% 
  filter(evangelical_10 ==1 & evangelical_14 ==1) %>% 
  mutate(age = 2014 - birthyr_14) %>% 
  summarise(mean = mean(age),
            sd = sd(age), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(status = c("Not Defect")) 
##60.3 



a1 <- cces %>% 
  filter(evangelical_10 ==1 & evangelical_14 ==0) %>% 
  mutate(attend = recode(pew_churatd_10, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend <10) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(status = c("Defect")) %>% 
  mutate(year = c("2010"))


a2 <- cces %>% 
  filter(evangelical_10 ==1 & evangelical_14 ==0) %>% 
  mutate(attend = recode(pew_churatd_14, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend <10) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(status = c("Defect")) %>% 
  mutate(year = c("2014"))


a3 <- cces %>% 
  filter(evangelical_10 ==1 & evangelical_14 ==1) %>% 
  mutate(attend = recode(pew_churatd_10, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend <10) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>%  
  mutate(status = c("Not Defect")) %>% 
  mutate(year = c("2010"))


a4 <- cces %>% 
  filter(evangelical_10 ==1 & evangelical_14 ==1) %>% 
  mutate(attend = recode(pew_churatd_14, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend <10) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>%  
  mutate(status = c("Not Defect")) %>% 
  mutate(year = c("2014"))

att <- bind_rows(a1, a2, a3, a4)



att %>% 
  # filter(type == "White Mainline") %>% 
  ggplot(., aes(x = mean, y = year))  +
  geom_point(shape=21, size =4, aes(fill = factor(year)), show.legend = FALSE) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper, colour = factor(year)), height=0, size = 1, show.legend = FALSE) + 
  scale_color_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey")) +
  scale_fill_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey")) +
  facet_grid(status~.) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  mean_rb() +
  labs(x= "Mean Church Attendance", y = "Year", title = "How are Evangelical Defectors Different than Those Who Stay?")

ggsave(file="evangelical_att_defectors.png", type = "cairo-png", width = 18, height = 15)


cces %>% 
  filter(evangelical_10 ==1 & evangelical_14 ==0) %>% 
  filter(pid7_14 <8) %>% 
  count(pid7_14) %>% 
  mutate(pct = prop.table(n))

