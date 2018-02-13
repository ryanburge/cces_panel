
cces <- cces %>% 
  mutate(abc_12 = recode(cces$religpew_baptist_12, "2=1; 4=1; else=0"))

cces <- cces %>% 
  mutate(epis_12 = recode(cces$religpew_episcop_12, "1:90=1; else=0"))

cces <- cces %>% 
  mutate(luth_12 = recode(cces$religpew_lutheran_12, "1=1; 4=1; else=0"))

cces <- cces %>% 
  mutate(meth_12 = recode(cces$religpew_methodist_12, "1=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(pres_12 = recode(cces$religpew_presby_12, "1=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(cong_12 = recode(cces$religpew_congreg_12, "1=1; 3=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(doc_12 = recode(cces$religpew_protestant_12, "8=1; else=0"))

cces <- cces %>% 
  mutate(reform_12 = recode(cces$religpew_protestant_12, "11=1; else=0"))

cces <- cces %>% 
  mutate(mainline_12 = abc_12 + epis_12 + luth_12 + meth_12 + pres_12 + cong_12 + doc_12 + reform_12) %>% 
  mutate(mainline_12 = recode(mainline_12, "1:5=1; else=0"))