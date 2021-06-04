#### 
## read in imputed ncands from cps_lifetables imputation run
## for mandated reporters analysis
### output full file for main read.r
rm(list=ls())
gc()
library(tidyverse)
library(mice)
library(stringr)


##### transform functions

nat_reports<-function(x){
  temp<-x %>% 
    mutate(chage = ifelse(chage==77, 0, chage)) %>% 
    mutate(chage = ifelse(chage>100, chage-100, chage)) %>% ### two sets, 113, 114 will assume they are typos
    group_by(.imp, subyr, race_ethn, chage, rptsrc, chmal1,
             rptvictim, drugs, alc) %>% 
    summarise(n_total = n()) %>% 
    ungroup()%>% 
    mutate(rptsrc = str_to_title(rptsrc))
  
  return(temp)
}

state_reports<-function(x){
  temp<-x %>% 
    mutate(chage = ifelse(chage==77, 0, chage)) %>% 
    mutate(chage = ifelse(chage>100, chage-100, chage)) %>% ### two sets, 113, 114 will assume they are typos
    group_by(.imp, staterr, subyr, race_ethn, chage, rptsrc, chmal1,
             rptvictim, drugs, alc) %>% 
    summarise(n_total = n()) %>% 
    ungroup()%>% 
    mutate(rptsrc = str_to_title(rptsrc))
  
  return(temp)
}


#### run the whole thing through a loop to speed up
path_files<-paste("./data/", list.files("./data"), sep = "")
filenames<-path_files[grep("subst", path_files)]

#### drug/alc variables got messed up by 
#### bug in pre-processing code
#### will use imputed vars, attach observed for subst ab vars


#for(i in 1:length(filenames)){
for(i in 1:length(filenames)){

  temp<-readRDS(filenames[i])%>% 
    mice::complete(action = "long", include = F)
  
  nat_temp<-nat_reports(temp)
  state_temp<-state_reports(temp)
  
  if(i==1){
    nat<-nat_temp
    state<-state_temp
  } else{
    nat<-nat %>% 
      bind_rows(nat_temp)
    state<-state %>% 
      bind_rows(state_temp)
  }
  
  rm(temp)
  gc()
}


saveRDS(nat, file = "nat_ncands.RDS")
saveRDS(state, file = "state_ncands.RDS")
