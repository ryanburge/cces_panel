
cces <- cces %>% 
  mutate(prot_10 = religpew_protestant_10 + 100)

prot <- cces %>% 
  select(caseid, prot_10, religpew_10) %>% 
  filter(religpew_10 ==1) %>% 
  select(-religpew_10) %>% 
  rename(relig10 = prot_10)

nprot <- cces %>% 
  select(caseid, religpew_10) %>% 
  filter(religpew_10 !=1) %>% 
  rename(relig10 = religpew_10)

r10 <- bind_rows(prot, nprot)

cces <- cces %>% 
  mutate(prot_12 = religpew_protestant_12 + 100)

prot <- cces %>% 
  select(caseid, prot_12, religpew_12) %>% 
  filter(religpew_12 ==1) %>% 
  select(-religpew_12) %>% 
  rename(relig12 = prot_12)

nprot <- cces %>% 
  select(caseid, religpew_12) %>% 
  filter(religpew_12 !=1) %>% 
  rename(relig12 = religpew_12)

r12 <- bind_rows(prot, nprot)

cces <- cces %>% 
  mutate(prot_14 = religpew_protestant_14 + 100)

prot <- cces %>% 
  select(caseid, prot_14, religpew_14) %>% 
  filter(religpew_14 ==1) %>% 
  select(-religpew_14) %>% 
  rename(relig14 = prot_14)

nprot <- cces %>% 
  select(caseid, religpew_14) %>% 
  filter(religpew_14 !=1) %>% 
  rename(relig14 = religpew_14)

r14 <- bind_rows(prot, nprot)

full <- left_join(r10, r12) %>% left_join(., r14)

cces <- left_join(cces, full, by='caseid')
