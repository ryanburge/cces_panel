
cces <- cces %>% 
  mutate(abc_14 = recode(cces$religpew_baptist_14, "2=1; 4=1; else=0"))

cces <- cces %>% 
  mutate(epis_14 = recode(cces$religpew_episcop_14, "1:90=1; else=0"))

cces <- cces %>% 
  mutate(luth_14 = recode(cces$religpew_lutheran_14, "1=1; 4=1; else=0"))

cces <- cces %>% 
  mutate(meth_14 = recode(cces$religpew_methodist_14, "1=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(pres_14 = recode(cces$religpew_presby_14, "1=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(cong_14 = recode(cces$religpew_congreg_14, "1=1; 3=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(doc_14 = recode(cces$religpew_protestant_14, "8=1; else=0"))

cces <- cces %>% 
  mutate(reform_14 = recode(cces$religpew_protestant_14, "11=1; else=0"))

cces <- cces %>% 
  mutate(mainline_14 = abc_14 + epis_14 + luth_14 + meth_14 + pres_14 + cong_14 + doc_14 + reform_14) %>% 
  mutate(mainline_14 = recode(mainline_14, "1:5=1; else=0"))