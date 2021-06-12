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

state_ipse<-state_ipse %>% 
  left_join(ncands_quality) %>% 
  filter(flag_1==T) 

#### look at R
qual_check2<-state_ipse %>% 
  filter(ipse==T) %>% 
  rename(state = staterr,
         year = subyr) %>% 
  group_by(state) %>% 
  summarise(n = mean(n_total),
            nmax = max(n_total),
            nmin = min(n_total)) %>% 
  ungroup() %>% 
  left_join(pop %>% 
              group_by(state, age) %>% 
              summarise(pop = sum(pop))) %>% 
  mutate(r = n / pop * 1000,
         rmax = nmax/pop * 1000,
         rmin = nmin/pop * 1000) %>% 
  filter(r>0.1) %>% 
  mutate(flag_2 = T)

state_ipse<-state_ipse %>% 
  left_join(qual_check2 %>% 
              rename(staterr = state)) %>% 
  filter(flag_2==T)
