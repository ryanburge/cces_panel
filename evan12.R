## 2012


cces <- cces %>% 
  mutate(white = recode(race_12, "1=1; else=0"))

## Baptist

cces <- cces %>%
  mutate(sbc_12 = recode(cces$religpew_baptist_12, "1=1; else=0")) %>% 
  mutate(sbc_12 = white + sbc_12) %>% 
  mutate(sbc_12 = recode(sbc_12, "2=1; else=0"))

cces <- cces %>%
  mutate(abc_12 = recode(cces$religpew_baptist_12, "2=1; else=0")) %>% 
  mutate(abc_12 = white + abc_12) %>% 
  mutate(abc_12 = recode(abc_12, "2=1; else=0"))

cces <- cces %>%
  mutate(ibc_12 = recode(cces$religpew_baptist_12, "5=1; else=0")) 

cces <- cces %>%
  mutate(bgc_12 = recode(cces$religpew_baptist_12, "6=1; else=0")) 

cces <- cces %>%
  mutate(mbc_12 = recode(cces$religpew_baptist_12, "7=1; else=0")) %>% 
  mutate(mbc_12 = white + mbc_12) %>% 
  mutate(mbc_12 = recode(mbc_12, "2=1; else=0"))

cces <- cces %>%
  mutate(cb_12 = recode(cces$religpew_baptist_12, "8=1; else=0")) 

cces <- cces %>%
  mutate(fwb_12 = recode(cces$religpew_baptist_12, "9=1; else=0")) 

cces <- cces %>%
  mutate(gabb_12 = recode(cces$religpew_baptist_12, "10=1; else=0")) 

cces <- cces %>%
  mutate(obc_12 = recode(cces$religpew_baptist_12, "90=1; else=0")) %>% 
  mutate(obc_12 = white + obc_12) %>% 
  mutate(obc_12 = recode(obc_12, "2=1; else=0"))

cces <- cces %>% 
  mutate(evanbap_12 = sbc_12 + abc_12 + ibc_12 + bgc_12 + mbc_12 + cb_12 + fwb_12 + gabb_12 + obc_12)

## Methodist
cces <- cces %>%
  mutate(fmc_12 = recode(cces$religpew_methodist_12, "2=1; else=0")) 

cces <- cces %>%
  mutate(omc_12 = recode(cces$religpew_methodist_12, "90=1; else=0")) %>% 
  mutate(omc_12 = white + omc_12) %>% 
  mutate(omc_12 = recode(omc_12, "2=1; else=0"))

cces <- cces %>% 
  mutate(evanmeth_12 = fmc_12 + omc_12)

##Non-Denom

cces <- cces %>% 
  mutate(hiatt_12 = recode(pew_churatd_12, "1:3=1; else=0")) %>% 
  mutate(nd_12 = recode(religpew_nondenom_12, "1:90=1; else=0")) %>% 
  mutate(evannd_12 = nd_12 + hiatt_12) %>% 
  mutate(evannd_12 =  recode(evannd_12, "2=1; else=0"))

## Lutheran 

cces <- cces %>% 
  mutate(mz_12 = recode(religpew_lutheran_12, "2=1; else=0")) %>% 
  mutate(wi_12 = recode(religpew_lutheran_12, "3=1; else=0")) %>% 
  mutate(evanluth_12 = mz_12 + wi_12)

## Presbyterian

cces <- cces %>% 
  mutate(pca_12 = recode(religpew_presby_12, "2=1; else=0")) %>% 
  mutate(epc_12 = recode(religpew_presby_12, "6=1; else=0")) %>% 
  mutate(evanpres_12 = pca_12 + epc_12)

## Pentecostal 

cces <- cces %>% 
  mutate(evanpent_12 = recode(religpew_pentecost_12, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

cces <- cces %>% 
  mutate(evancong_12 = recode(religpew_congreg_12, "2=1; else=0"))

## Holiness
cces <- cces %>% 
  mutate(evanholy_12 = recode(religpew_holiness_12, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

cces <- cces %>% 
  mutate(evangelical_12 = evanbap_12 + evanmeth_12 + evannd_12 + evanluth_12 + evanpres_12 + evanpent_12 + evancong_12 + evanholy_12) %>% 
  mutate(evangelical_12 = recode(evangelical_12, "1:4=1; else=0"))