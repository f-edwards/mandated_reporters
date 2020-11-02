#### 
## read in imputed ncands from cps_lifetables imputation run
## for mandated reporters analysis
### output full file for main read.r

gc()
library(tidyverse)
library(mice)


#### PAUSE TO MAKE IT SO I DONT NEED TO JOIN!!!!

# ncands1<-readRDS("./data/ncands_imps1.RDS") %>% 
#   mice::complete(action = "long", include = F) %>% 
#   select(-pct_aian, -pct_api, -pct_Black, -pct_Latinx,
#          -sex)
# ncands2<-readRDS("./data/ncands_imps2.RDS")%>% 
#   mice::complete(action = "long", include = F) %>% 
#   select(-pct_aian, -pct_api, -pct_Black, -pct_Latinx,
#          -sex)
# ncands3<-readRDS("./data/ncands_imps3.RDS")%>% 
#   mice::complete(action = "long", include = F) %>% 
#   select(-pct_aian, -pct_api, -pct_Black, -pct_Latinx,
#          -sex)
# ncands4<-readRDS("./data/ncands_imps4.RDS")%>% 
#   mice::complete(action = "long", include = F) %>% 
#   select(-pct_aian, -pct_api, -pct_Black, -pct_Latinx,
#          -sex)
ncands5<-readRDS("./data/ncands_imps5.RDS")%>% 
  mice::complete(action = "long", include = F) %>% 
  select(-pct_aian, -pct_api, -pct_Black, -pct_Latinx,
         -sex)

nat_reports<-function(x){
  temp<-x %>% 
    mutate(race_ethn = as.character(race_ethn)) %>% 
    mutate(race_ethn =
             ifelse(race_ethn=="AI/AN", "AIAN", race_ethn),
           race_ethn = ifelse(race_ethn=="Hispanic", "Latinx", race_ethn)) %>%
    mutate(rptsrc = ifelse(is.na(rptsrc), "unknown", rptsrc)) %>% 
    filter(!(is.na(race_ethn)), !is.na(age)) %>%  # check the 700+ missing post imputation
    group_by(.imp, year, race_ethn, age, rptsrc, chmal1) %>% 
    summarise(n_total = n()) %>% 
    ungroup()
  
  return(temp)
}

state_reports<-function(x){
  temp<-x %>% 
    mutate(race_ethn = as.character(race_ethn)) %>% 
    mutate(race_ethn =
             ifelse(race_ethn=="AI/AN", 
                    "AIAN", 
                    race_ethn),
    race_ethn = ifelse(race_ethn=="Hispanic", "Latinx", race_ethn)) %>%
    mutate(rptsrc = ifelse(is.na(rptsrc), "unknown", rptsrc)) %>% 
    filter(!(is.na(race_ethn)), !is.na(age)) %>%  # check the 700+ missing post imputation
    group_by(.imp, staterr, year, race_ethn, age, rptsrc, chmal1) %>% 
    summarise(n_total = n())
  
  ### later add nreports, but can't be subset by kid, must be by rptsrc, can 
  ### have variation across kids within reports
  
  return(temp)
}

# nat_1<-nat_reports(ncands1)
# nat_2<-nat_reports(ncands2)
# nat_3<-nat_reports(ncands3)
# nat_4<-nat_reports(ncands4)
nat_5<-nat_reports(ncands5)

# nat_ncands<-bind_rows(
#   nat_1, nat_2, nat_3, nat_4, nat_5)


state_5<-state_reports(ncands5)

nat_ncands<-nat_5
state_ncands<-state_5
