### run lots of descriptives
### all drug involved infants by state / year / rptsrc / race
### tpr for drug cases
### maybe life tables?

rm(list=ls()); gc()
library(data.table)
library(tidyverse)
library(lubridate)
library(mice)

pop<-read_fwf("~/Projects/cps_lifetables/data/us.1990_2018.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop")))

pop<-pop%>%
  mutate(pop = as.integer(pop))%>%
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "AIAN", 
             race==4 ~ "Asian/PI",
             hisp==1 ~ "Latinx"))

pop_st <- pop %>% 
  filter(age=="00") %>% 
  group_by(state, year, st_fips, race_ethn) %>% 
  summarise(infant_pop = sum(pop)) %>% 
  ungroup() %>% 
  rename(staterr = state, subyr = year)

ncands_path<-"~/Projects/ndacan_data/ncands/"
ncands_files<-paste(ncands_path,
                    list.files(ncands_path),
                    sep = "")

### set up for 17, 18 imputation
ncands_files<-paste(ncands_path, list.files(ncands_path), sep = "")[17]

ncands<-lapply(ncands_files, fread)
### set up to grab rptyear variable, bind to rptdt//chid for index join
for(i in 1:length(ncands_files)){
  ncands[[i]]<-ncands[[i]]%>%
    rename_all(tolower) %>%
    mutate(rptdt = ymd(rptdt)) %>%
    mutate(race_ethn =
             ifelse(chracbl==1,
                    "Black",
                    ifelse(chracai==1, "AI/AN",
                           ifelse(chracas==1 | chracnh==1,
                                  "Asian/PI",
                                  ifelse(cethn==1, "Hispanic",
                                         ifelse(chracwh == 1, "White",
                                                NA)))))) %>%
    select(chid, staterr,
           rptfips, rptdt,
           chage, race_ethn,
           rptvictim, subyr, 
           cddrug, fcdrug, rptsrc)
}

ncands1<-bind_rows(ncands)

ncands_st_rpt<-ncands1 %>% 
  mutate(rptsrc = 
           case_when(
             rptsrc==1 ~ "social services",
             rptsrc==2 ~ "medical",
             rptsrc==3 ~ "mental health",
             rptsrc==4 ~ "police",
             rptsrc==5 ~ "education",
             rptsrc==6 ~ "day care",
             rptsrc==7 ~ "substitute care",
             rptsrc==99 ~ "unknown",
             rptsrc>7 ~ "non - mandated"
           )) %>% 
  group_by(staterr, rptsrc) %>% 
  summarise(total_report = n()) %>% 
  mutate(proportion = total_report/sum(total_report)) %>% 
  ungroup() 

###make total pop for join
write_csv(ncands_st_rpt, "./data/ncands_st_rpt.csv")
