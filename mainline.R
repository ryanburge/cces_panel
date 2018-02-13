

cces16 <- cces16 %>% 
  mutate(abc = recode(cces16$religpew_baptist, "2=1; 4=1; else=0"))

cces16 <- cces16 %>% 
  mutate(epis = recode(cces16$religpew_episcop, "1:90=1; else=0"))

cces16 <- cces16 %>% 
  mutate(luth = recode(cces16$religpew_lutheran, "1=1; 4=1; else=0"))

cces16 <- cces16 %>% 
  mutate(meth = recode(cces16$religpew_methodist, "1=1; 90=1; else=0"))

cces16 <- cces16 %>% 
  mutate(pres = recode(cces16$religpew_presby, "1=1; 90=1; else=0"))

cces16 <- cces16 %>% 
  mutate(cong = recode(cces16$religpew_congreg, "1=1; 3=1; 90=1; else=0"))

cces16 <- cces16 %>% 
  mutate(doc = recode(cces16$religpew_protestant, "8=1; else=0"))

cces16 <- cces16 %>% 
  mutate(reform = recode(cces16$religpew_protestant, "11=1; else=0"))

cces16 <- cces16 %>% 
  mutate(mainline = abc + epis + luth + meth + pres + cong + doc + reform) %>% 
  mutate(mainline = recode(mainline, "1:5=1; else=0"))