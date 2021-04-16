#### 
## read in imputed ncands from cps_lifetables imputation run
## for mandated reporters analysis
### output full file for main read.r
rm(list=ls())
gc()
library(tidyverse)
library(mice)


#### PAUSE TO MAKE IT SO I DONT NEED TO JOIN!!!!

ncands1<-readRDS("./data/ncands_imps_subst1.RDS") %>%
  mice::complete(action = "long", include = F)

ncands2<-readRDS("./data/ncands_imps_subst2.RDS")%>%
  mice::complete(action = "long", include = F)

ncands3<-readRDS("./data/ncands_imps_subst3.RDS")%>%
  mice::complete(action = "long", include = F)

ncands4<-readRDS("./data/ncands_imps_subst4.RDS")%>%
  mice::complete(action = "long", include = F)

ncands5<-readRDS("./data/ncands_imps_subst5.RDS")%>% 
  mice::complete(action = "long", include = F) 




nat_reports<-function(x){
  temp<-x %>% 
    group_by(.imp, subyr, race_ethn, chage, rptsrc, chmal1,
             rptvictim, drugs) %>% 
    summarise(n_total = n()) %>% 
    ungroup()
  
  return(temp)
}

state_reports<-function(x){
  temp<-x %>% 
    group_by(.imp, staterr, subyr, race_ethn, chage, rptsrc, chmal1,
             rptvictim, drugs) %>% 
    summarise(n_total = n()) %>% 
    ungroup()
  
  return(temp)
}

nat_1<-nat_reports(ncands1)
nat_2<-nat_reports(ncands2)
nat_3<-nat_reports(ncands3)
nat_4<-nat_reports(ncands4)
nat_5<-nat_reports(ncands5)

nat_ncands<-bind_rows(
  nat_1, nat_2, nat_3, nat_4, nat_5)


state_1<-state_reports(ncands1)
state_2<-state_reports(ncands2)
state_3<-state_reports(ncands3)
state_4<-state_reports(ncands4)
state_5<-state_reports(ncands5)

state_ncands<-bind_rows(state_1,
                        state_2, 
                        state_3,
                        state_4,
                        state_5)

saveRDS(nat_ncands, file = "nat_ncands.RDS")
saveRDS(state_ncands, file = "state_ncands.RDS")
