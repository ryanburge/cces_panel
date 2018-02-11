## 2012


cces <- cces %>% 
  mutate(white = recode(race_14, "1=1; else=0"))

## Baptist

cces <- cces %>%
  mutate(sbc_14 = recode(cces$religpew_baptist_14, "1=1; else=0")) %>% 
  mutate(sbc_14 = white + sbc_14) %>% 
  mutate(sbc_14 = recode(sbc_14, "2=1; else=0"))

cces <- cces %>%
  mutate(abc_14 = recode(cces$religpew_baptist_14, "2=1; else=0")) %>% 
  mutate(abc_14 = white + abc_14) %>% 
  mutate(abc_14 = recode(abc_14, "2=1; else=0"))

cces <- cces %>%
  mutate(ibc_14 = recode(cces$religpew_baptist_14, "5=1; else=0")) 

cces <- cces %>%
  mutate(bgc_14 = recode(cces$religpew_baptist_14, "6=1; else=0")) 

cces <- cces %>%
  mutate(mbc_14 = recode(cces$religpew_baptist_14, "7=1; else=0")) %>% 
  mutate(mbc_14 = white + mbc_14) %>% 
  mutate(mbc_14 = recode(mbc_14, "2=1; else=0"))

cces <- cces %>%
  mutate(cb_14 = recode(cces$religpew_baptist_14, "8=1; else=0")) 

cces <- cces %>%
  mutate(fwb_14 = recode(cces$religpew_baptist_14, "9=1; else=0")) 

cces <- cces %>%
  mutate(gabb_14 = recode(cces$religpew_baptist_14, "10=1; else=0")) 

cces <- cces %>%
  mutate(obc_14 = recode(cces$religpew_baptist_14, "90=1; else=0")) %>% 
  mutate(obc_14 = white + obc_14) %>% 
  mutate(obc_14 = recode(obc_14, "2=1; else=0"))

cces <- cces %>% 
  mutate(evanbap_14 = sbc_14 + abc_14 + ibc_14 + bgc_14 + mbc_14 + cb_14 + fwb_14 + gabb_14 + obc_14)

## Methodist
cces <- cces %>%
  mutate(fmc_14 = recode(cces$religpew_methodist_14, "2=1; else=0")) 

cces <- cces %>%
  mutate(omc_14 = recode(cces$religpew_methodist_14, "90=1; else=0")) %>% 
  mutate(omc_14 = white + omc_14) %>% 
  mutate(omc_14 = recode(omc_14, "2=1; else=0"))

cces <- cces %>% 
  mutate(evanmeth_14 = fmc_14 + omc_14)

##Non-Denom

cces <- cces %>% 
  mutate(hiatt_14 = recode(pew_churatd_14, "1:3=1; else=0")) %>% 
  mutate(nd_14 = recode(religpew_nondenom_14, "1:90=1; else=0")) %>% 
  mutate(evannd_14 = nd_14 + hiatt_14) %>% 
  mutate(evannd_14 =  recode(evannd_14, "2=1; else=0"))

## Lutheran 

cces <- cces %>% 
  mutate(mz_14 = recode(religpew_lutheran_14, "2=1; else=0")) %>% 
  mutate(wi_14 = recode(religpew_lutheran_14, "3=1; else=0")) %>% 
  mutate(evanluth_14 = mz_14 + wi_14)

## Presbyterian

cces <- cces %>% 
  mutate(pca_14 = recode(religpew_presby_14, "2=1; else=0")) %>% 
  mutate(epc_14 = recode(religpew_presby_14, "6=1; else=0")) %>% 
  mutate(evanpres_14 = pca_14 + epc_14)

## Pentecostal 

cces <- cces %>% 
  mutate(evanpent_14 = recode(religpew_pentecost_14, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

cces <- cces %>% 
  mutate(evancong_14 = recode(religpew_congreg_14, "2=1; else=0"))

## Holiness
cces <- cces %>% 
  mutate(evanholy_14 = recode(religpew_holiness_14, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

cces <- cces %>% 
  mutate(evangelical_14 = evanbap_14 + evanmeth_14 + evannd_14 + evanluth_14 + evanpres_14 + evanpent_14 + evancong_14 + evanholy_14) %>% 
  mutate(evangelical_14 = recode(evangelical_14, "1:4=1; else=0"))