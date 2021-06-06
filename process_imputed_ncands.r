#### 
## read in imputed ncands from cps_lifetables imputation run
## for mandated reporters analysis
### output full file for main read.r
rm(list=ls())
gc()
library(tidyverse)
library(mice)
library(stringr)
library(data.table)


##### transform functions

nat_reports<-function(x){
  temp<-x %>% 
    mutate(chage = ifelse(chage==77, 0, chage)) %>% 
    mutate(chage = ifelse(chage>100, chage-100, chage)) %>% 
    filter(chage == 0) %>% 
    mutate(id = paste(.imp, chid, staterr))
  
  temp<-temp[!duplicated(temp$id),] 
  
  temp<-temp %>% 
    group_by(.imp, subyr, race_ethn, chage, rptsrc, chmal1,
             rptvictim, fcdrug, cddrug, fcalc, cdalc) %>% 
    summarise(n_total = n()) %>% 
    ungroup()%>% 
    mutate(rptsrc = str_to_title(rptsrc))
  
  return(temp)
}

state_reports<-function(x){
  temp<-x %>% 
    mutate(chage = ifelse(chage==77, 0, chage)) %>% 
    mutate(chage = ifelse(chage>100, chage-100, chage)) %>%
    filter(chage == 0) %>% 
    mutate(id = paste(.imp, chid, staterr)) 
    
  temp<-temp[!duplicated(temp$id),] 
  
  temp<-temp %>% 
    group_by(.imp, staterr, subyr, race_ethn, chage, rptsrc, chmal1,
             rptvictim, fcdrug, cddrug, fcalc, cdalc) %>% 
    summarise(n_total = n()) %>% 
    ungroup()%>% 
    mutate(rptsrc = str_to_title(rptsrc))
  
  return(temp)
}


ipse_nat<-function(x){
  temp<-x %>% 
    mutate(chage = ifelse(chage==77, 0, chage)) %>% 
    filter(chage == 0) %>% 
    filter(rptsrc == "medical") %>% 
    mutate(ipse = 
             case_when(
               cddrug == 1 ~ T,
               fcdrug == 1 ~ T,
               fcalc == 1 ~ T,
               cdalc == 1 ~ T,
               cddrug == 2 | fcdrug == 2 | fcalc == 2 | cdalc == 2 ~ F, 
               is.na(cddrug) | is.na(fcdrug) | is.na(fcalc) | is.na(cdalc) ~ F
             )) %>% 
    mutate(id = paste(.imp, chid, staterr))
  
  temp<-temp[!duplicated(temp$id),]  
  
  
  temp<-temp %>% 
    group_by(.imp, subyr, race_ethn, ipse) %>% 
    summarise(n_total = n()) %>% 
    ungroup()
  return(temp)
}

ipse_st<-function(x){
  
  temp<-x %>% 
    mutate(chage = ifelse(chage==77, 0, chage)) %>% 
    filter(chage == 0) %>% 
    filter(rptsrc == "medical") %>% 
    mutate(ipse = 
             case_when(
               cddrug == 1 ~ T,
               fcdrug == 1 ~ T,
               fcalc == 1 ~ T,
               cdalc == 1 ~ T,
               cddrug == 2 | fcdrug == 2 | fcalc == 2 | cdalc == 2 ~ F, 
               is.na(cddrug) | is.na(fcdrug) | is.na(fcalc) | is.na(cdalc) ~ F
             )) %>% 
    mutate(id = paste(.imp, chid, staterr))
  
  temp<-temp[!duplicated(temp$id),]  
  
  temp<- temp %>% 
    group_by(.imp, staterr, subyr, race_ethn, ipse) %>% 
    summarise(n_total = n()) %>% 
    ungroup()
  
  return(temp)
}


#### run the whole thing through a loop to speed up
path_files<-paste("./data/", list.files("./data"), sep = "")
filenames<-path_files[grep("subst", path_files)]
filenames<-filenames[c(1, 3:10, 2)]

#### drug/alc variables got messed up by 
#### bug in pre-processing code
#### will use imputed vars, attach observed for subst ab vars
ncands_path<-"~/Projects/ndacan_data/ncands/"
ncands_files<-paste(ncands_path,
                    list.files(ncands_path),
                    sep = "")
ncands_files<-paste(ncands_path, list.files(ncands_path), sep = "")[9:18]


#for(i in 1:length(filenames)){
for(i in 1:length(filenames)){
  
  #### there are errors in the subst ab variables
  #### remove these from imputed, re-attach from observed
  temp<-readRDS(filenames[i])%>% 
    mice::complete(action = "long", include = F) %>% 
    select(.imp, rptid, chid, staterr, rptsrc, chmal1, chsex, chage, race_ethn, rptvictim, subyr)
  
  temp_ncands<-fread(ncands_files[i]) %>% 
    rename_all(tolower) %>% 
    select(rptid, chid, staterr, fcdrug, cddrug, fcalc, cdalc) %>% 
    mutate(fcdrug = ifelse(fcdrug==9, NA, fcdrug),
           cddrug = ifelse(cddrug==9, NA, cddrug),
           fcalc = ifelse(fcalc==9, NA, fcalc),
           cdalc = ifelse(cdalc==9, NA, cdalc))
  
  temp_join<-left_join(temp, temp_ncands)
  
  ### set up 10-19 for 1o year panel on NCANDS
  
  
  nat_temp<-nat_reports(temp_join)
  state_temp<-state_reports(temp_join)
  nat_ipse_temp<-ipse_nat(temp_join)
  state_ipse_temp<-ipse_st(temp_join)
  
  
  if(i==1){
    nat<-nat_temp
    state<-state_temp
    nat_ipse<-nat_ipse_temp
    state_ipse<-state_ipse_temp
    
  } else{
    nat<-nat %>%
      bind_rows(nat_temp)
    state<-state %>%
      bind_rows(state_temp)
    nat_ipse<-nat_ipse %>% 
      bind_rows(nat_ipse_temp)
    state_ipse<-state_ipse %>% 
      bind_rows(state_ipse_temp)
  }
  
  rm(temp)
  rm(temp_ncands)
  gc()
}


saveRDS(nat, file = "nat_ncands.RDS")
saveRDS(state, file = "state_ncands.RDS")
saveRDS(nat_ipse, file = "nat_ncands_ipse.RDS")
saveRDS(state_ipse, file = "state_ncands_ipse.RDS")
# 
# 
# #### CONFIRM COUNTS AGAINST OBSERVED
# ### FOR FY19 see table 7-5
# ### https://www.acf.hhs.gov/sites/default/files/documents/cb/cm2019.pdf#page=112
# 
# state %>% filter(.imp==1, rptsrc=="Medical", chage==0, subyr==2019) %>% 
#   filter(fcdrug==1) %>% group_by(staterr) %>% summarise(n = sum(n_total))
