library(tidyverse)
library(bookdown)
library(data.table)
library(viridisLite)
library(RColorBrewer)
theme_set(theme_minimal())
options(scipen=999)

### load pre-processed data
## from read.r and process_imputed_ncands.r and check_state_quality.r
nat_ncands<-readRDS("nat_ncands.RDS")

nat_ncands<-nat_ncands %>% 
  rename(age = chage,
         year = subyr) %>% 
  filter(age<=0)

state_ncands<-readRDS("state_ncands.RDS")

state_ncands<-state_ncands %>% 
  rename(age = chage,
         year = subyr) %>% 
  filter(age<=0)

pop<-readRDS("pop.RDS")
pop<-pop %>% 
  filter(age == 0)

nat_ipse<-readRDS("nat_ncands_ipse.RDS")
state_ipse<-readRDS("state_ncands_ipse.RDS")

ncands_quality<-read_csv("./data/ncands_drugs_quality_check.csv")

### push all quality processing to check_state_quality.r
### merge files and set up for plotting

state_ncands<-state_ncands %>% 
  group_by(.imp, staterr, year, race_ethn, rptsrc) %>% 
  summarise(n_total = sum(n_total))


nat_ncands<-nat_ncands %>% 
  group_by(.imp, year, race_ethn, rptsrc) %>% 
  summarise(n_total = sum(n_total))
