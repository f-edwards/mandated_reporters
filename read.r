
pop<-read_fwf("~/Projects/cps_lifetables/data/us.1990_2018.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop"))) %>% 
  filter(year>2013) 

pop<-pop%>%
  mutate(pop = as.integer(pop),
         age = as.integer(age))%>%
  filter(age<=18) %>% 
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "AIAN", 
             race==4 ~ "Asian/PI",
             hisp==1 ~ "Latinx")) %>% 
  group_by(year, state, st_fips, cnty_fips, race_ethn, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

