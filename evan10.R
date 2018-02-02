cces <- cces %>% 
  mutate(white = recode(race_10, "1=1; else=0"))

## Baptist

cces <- cces %>%
  mutate(sbc_10 = recode(cces$religpew_baptist_10, "1=1; else=0")) %>% 
  mutate(sbc_10 = white + sbc_10) %>% 
  mutate(sbc_10 = recode(sbc_10, "2=1; else=0"))

cces <- cces %>%
  mutate(abc_10 = recode(cces$religpew_baptist_10, "2=1; else=0")) %>% 
  mutate(abc_10 = white + abc_10) %>% 
  mutate(abc_10 = recode(abc_10, "2=1; else=0"))

cces <- cces %>%
  mutate(ibc_10 = recode(cces$religpew_baptist_10, "5=1; else=0")) 

cces <- cces %>%
  mutate(bgc_10 = recode(cces$religpew_baptist_10, "6=1; else=0")) 

cces <- cces %>%
  mutate(mbc_10 = recode(cces$religpew_baptist_10, "7=1; else=0")) %>% 
  mutate(mbc_10 = white + mbc_10) %>% 
  mutate(mbc_10 = recode(mbc_10, "2=1; else=0"))

cces <- cces %>%
  mutate(cb_10 = recode(cces$religpew_baptist_10, "8=1; else=0")) 

cces <- cces %>%
  mutate(fwb_10 = recode(cces$religpew_baptist_10, "9=1; else=0")) 

cces <- cces %>%
  mutate(gabb_10 = recode(cces$religpew_baptist_10, "10=1; else=0")) 

cces <- cces %>%
  mutate(obc_10 = recode(cces$religpew_baptist_10, "90=1; else=0")) %>% 
  mutate(obc_10 = white + obc_10) %>% 
  mutate(obc_10 = recode(obc_10, "2=1; else=0"))

cces <- cces %>% 
  mutate(evanbap_10 = sbc_10 + abc_10 + ibc_10 + bgc_10 + mbc_10 + cb_10 + fwb_10 + gabb_10 + obc_10)

## Methodist
cces <- cces %>%
  mutate(fmc_10 = recode(cces$religpew_methodist_10, "2=1; else=0")) 

cces <- cces %>%
  mutate(omc_10 = recode(cces$religpew_methodist_10, "90=1; else=0")) %>% 
  mutate(omc_10 = white + omc_10) %>% 
  mutate(omc_10 = recode(omc_10, "2=1; else=0"))

cces <- cces %>% 
  mutate(evanmeth_10 = fmc_10 + omc_10)

##Non-Denom

cces <- cces %>% 
  mutate(hiatt_10 = recode(pew_churatd_10, "1:3=1; else=0")) %>% 
  mutate(nd_10 = recode(religpew_nondenom_10, "1:90=1; else=0")) %>% 
  mutate(evannd_10 = nd_10 + hiatt_10) %>% 
  mutate(evannd_10 =  recode(evannd_10, "2=1; else=0"))

## Lutheran 

cces <- cces %>% 
  mutate(mz_10 = recode(religpew_lutheran_10, "2=1; else=0")) %>% 
  mutate(wi_10 = recode(religpew_lutheran_10, "3=1; else=0")) %>% 
  mutate(evanluth_10 = mz_10 + wi_10)

## Presbyterian

cces <- cces %>% 
  mutate(pca_10 = recode(religpew_presby_10, "2=1; else=0")) %>% 
  mutate(epc_10 = recode(religpew_presby_10, "6=1; else=0")) %>% 
  mutate(evanpres_10 = pca_10 + epc_10)

## Pentecostal 

cces <- cces %>% 
  mutate(evanpent_10 = recode(religpew_pentecost_10, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

cces <- cces %>% 
  mutate(evancong_10 = recode(religpew_congreg_10, "2=1; else=0"))

## Holiness
cces <- cces %>% 
  mutate(evanholy_10 = recode(religpew_holiness_10, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

cces <- cces %>% 
  mutate(evangelical_10 = evanbap_10 + evanmeth_10 + evannd_10 + evanluth_10 + evanpres_10 + evanpent_10 + evancong_10 + evanholy_10) %>% 
  mutate(evangelical_10 = recode(evangelical_10, "1:4=1; else=0"))

