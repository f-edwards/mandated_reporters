library(tidyverse)

#### update the births data for 10-19
### pull from CDC Wonder
### how to think about matching
### mother / father race data to child race data.... excluding for now
### match won't be straightforward, just use age 0 SEER data
# births<-read_tsv("./data/natality18.txt",
#                  guess_max = (1e5)) %>% 
#   slice(1:115) %>% 
#   filter(is.na(Notes)) %>% 
#   select(-Notes)
# 
# births_grp<-births %>% 
#   mutate(race_ethn = 
#            case_when(
#              str_detect(`Mother's Single/Multi Race 31`,
#                         "AIAN") ~ "AIAN",
#              str_detect(`Mother's Single/Multi Race 31`,
#                         "Black") ~ "Black",
#              str_detect(`Mother's Single/Multi Race 31`,
#                         "Asian") ~ "Asian/PI",
#              str_detect(`Mother's Single/Multi Race 31`,
#                         "NHOPI") ~ "Asian/PI",
#              `Mother's Hispanic Origin` == "Hispanic or Latino" ~ "Latinx",
#              `Mother's Single/Multi Race 31`=="White (only)" ~ "White"
#            )) %>% 
#   group_by(race_ethn) %>% 
#   summarise(births = sum(Births))


pop<-read_fwf("./data/us.1990_2019.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop"))) %>% 
  filter(year>2009) 

pop<-pop%>%
  mutate(pop = as.integer(pop),
         age = as.integer(age))%>%
  filter(age==0) %>% 
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "AIAN", 
             race==4 ~ "Asian/PI",
             hisp==1 ~ "Latinx")) %>% 
  group_by(year, state, st_fips, race_ethn, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()


# #compare births to age 0 pop
# pop %>% 
#   filter(year==2018, age==0) %>% 
#   group_by(race_ethn) %>% 
#   summarise(pop = sum(pop))
# ### looks fine
# 
# saveRDS(births_grp, file = "births.RDS")

saveRDS(pop, file = "pop.RDS")