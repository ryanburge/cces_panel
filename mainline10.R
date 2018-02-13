
cces <- cces %>% 
  mutate(abc_10 = recode(cces$religpew_baptist_10, "2=1; 4=1; else=0"))

cces <- cces %>% 
  mutate(epis_10 = recode(cces$religpew_episcop_10, "1:90=1; else=0"))

cces <- cces %>% 
  mutate(luth_10 = recode(cces$religpew_lutheran_10, "1=1; 4=1; else=0"))

cces <- cces %>% 
  mutate(meth_10 = recode(cces$religpew_methodist_10, "1=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(pres_10 = recode(cces$religpew_presby_10, "1=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(cong_10 = recode(cces$religpew_congreg_10, "1=1; 3=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(doc_10 = recode(cces$religpew_protestant_10, "8=1; else=0"))

cces <- cces %>% 
  mutate(reform_10 = recode(cces$religpew_protestant_10, "11=1; else=0"))

cces <- cces %>% 
  mutate(mainline_10 = abc_10 + epis_10 + luth_10 + meth_10 + pres_10 + cong_10 + doc_10 + reform_10) %>% 
  mutate(mainline_10 = recode(mainline_10, "1:5=1; else=0"))