# --- Amy Jiravisitcul. 22 May 2023 ----
rm(list = ls()) # clear working environment
setwd("~/Documents/DCSC/member_data/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)
install.packages('segregation')
library(segregation)
getwd()

# ----- ENROLLMENT BY RACE --------
# data from http://profiles.doe.mass.edu/statereport/enrollmentbyracegender.aspx
enrl <- read_excel("raw data/MA_enrollmentbyracegender.xlsx")
names(enrl) <- tolower(enrl[1,])
enrl <- enrl[-1,]
enrl <- enrl %>% 
  separate(`school name`,c("district","school")," - ",extra = "merge") %>% 
  mutate(district = case_when(school == "Boston Collegiate Charter School" ~ "Boston",
                            TRUE ~ district))
str(enrl)
enrl <- enrl %>% 
  mutate(amind = .01 * as.numeric(`native american`),
         asian = .01 * (as.numeric(asian)+as.numeric(`native hawaiian, pacific islander`)),
         black = .01 * as.numeric(`african american`),
         hispanic = .01 * as.numeric(hispanic),
         white = .01 * as.numeric(white),
         multiple = .01 * as.numeric(`multi-race, non-hispanic`)) %>% 
  select(district, school, amind, asian, black, hispanic, white, multiple)

# - need the other data set for total student enrollment calculations by school
# https://profiles.doe.mass.edu/statereport/selectedpopulations.aspx
sub <- read_excel("raw data/MA_selectedpopulations.xlsx")
names(sub) <- tolower(sub[1,])
sub <- sub[-1,]
sub <- sub[,c(1,6,8,10,9)]
names(sub)[c(2:5)] <- c("ell","swd","lowincome","n_lowincome")

str(sub)
sub <- sub %>%
  separate(`school name`,c("district","school")," - ",extra = "merge") %>% 
  mutate(total = (as.numeric(n_lowincome)*100)/as.numeric(lowincome),
         ell = as.numeric(ell)*.01,
         swd = as.numeric(swd)*.01,
         lowincome = as.numeric(lowincome)*.01) %>% 
  select(school, district, total, ell, swd, lowincome)
enrl <- merge(enrl,sub, by= "school") %>% select(-district.y)
names(enrl)[2] <- "district"
memb_enrl <- enrl %>% 
  filter(str_detect(school,"Boston Collegiate"))

# ----- LOCAL SEGREGATION BY DISTRICT -------
boston <- enrl %>% 
  gather(race, n, amind:multiple) %>% 
  select(school, district, race, n) %>% 
  filter(district == "Boston") %>% 
  mutual_local("race","school",weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  filter(str_detect(school, "Boston Collegiate")) %>% 
  select(school, ls_dist)

memb_enrl<- merge(memb_enrl, boston)

# --- DISTRICT AGGREGATE BY RACE, ECON DISADVANTAGE, SWD, ELL -----
# data from http://profiles.doe.mass.edu/statereport/selectedpopulations.aspx

agg <- enrl %>% 
  mutate(amind = amind * total,
         asian = asian * total,
         black = black * total,
         hispanic = hispanic * total,
         white = white * total,
         multiple = multiple * total,
         lowincome = lowincome * total,
         ell = ell * total,
         swd = swd * total) %>% 
  group_by(district) %>% 
  summarize(dist_total = sum(total),
            dist_amind = sum(amind)/dist_total,
            dist_asian = sum(asian)/dist_total,
            dist_black = sum(black)/dist_total,
            dist_hispanic = sum(hispanic)/dist_total,
            dist_white = sum(white)/dist_total,
            dist_multiple = sum(multiple)/dist_total,
            dist_ecdis = sum(lowincome)/dist_total,
            dist_ell = sum(ell)/dist_total,
            dist_swd = sum(swd)/dist_total) %>% 
  filter(district == "Boston")

memb_enrl <- merge(memb_enrl,agg) 

# ---- COUNTY AGGREGATE BY RACE, ECON DISADVANTAGE, SWD, ELL -----
# https://www.k12academics.com/national-directories/school-district/Massachusetts/Middlesex
# https://massgis.maps.arcgis.com/apps/webappviewer/index.html?id=5b652451857f41ecabc5aa96ef53641c

cty_agg <- enrl %>%
  filter(str_detect(district,"Boston")| # School districts listed in Suffolk County
           str_detect(district, "Chelsea")|
           str_detect(district, "Revere")|
           str_detect(district,"Winthrop")) %>% 
  mutate(amind = amind * total,
         asian = asian * total,
         black = black * total,
         hispanic = hispanic * total,
         white = white * total,
         multiple = multiple * total,
         lowincome = lowincome * total,
         ell = ell * total,
         swd = swd * total,
         county = "Suffolk") %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  group_by(county) %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(amind)/cty_total,
            cty_asian = sum(asian)/cty_total,
            cty_black = sum(black)/cty_total,
            cty_hisp = sum(hispanic)/cty_total,
            cty_white = sum(white)/cty_total,
            cty_mult = sum(multiple)/cty_total,
            cty_ecdis = sum(lowincome)/cty_total,
            cty_ell = sum(ell)/cty_total,
            cty_swd = sum(swd)/cty_total)

# https://www.k12academics.com/national-directories/school-district/Massachusetts/Suffolk
suffolk_agg <- enrl %>% 
  filter(str_detect(district,"Boston")| # School districts listed in Suffolk County
           str_detect(district, "Chelsea")|
           str_detect(district, "Revere")|
           str_detect(district,"Winthrop")) %>% 
  mutate(amind = amind * total,
         asian = asian * total,
         black = black * total,
         hispanic = hispanic * total,
         white = white * total,
         multiple = multiple * total,
         lowincome = lowincome * total,
         ell = ell * total,
         swd = swd * total)
suffolk_locseg <- suffolk_agg %>% 
  select(school, amind:multiple) %>% 
  gather(race, n, amind:multiple) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>% 
  filter(str_detect(school, "Boston Collegiate")) %>% 
  mutate(ls_cty = ls,
         county = "Suffolk") %>% select(school, county, ls_cty)

suffolk_locseg <- merge(suffolk_locseg, cty_agg, by="county")

memb_enrl <- merge(memb_enrl,suffolk_locseg)
names(memb_enrl)
memb_enrl <- memb_enrl %>% 
  select(school, total, amind:multiple, lowincome, ell, swd, ls_dist, ls_cty, district, dist_total:county,
         cty_total:cty_swd)

write.csv(memb_enrl, file = file.path('output data/ma_enrl.csv'),row.names = FALSE)

# ---- NEXTGEN MCAS DATA -------
acad <- read_excel('raw data/MA_NextGenMCAS.xlsx')
names(acad) <- tolower(acad[1,])
acad <- acad[-1,]
str(acad)
memb_acad <- acad %>% 
  filter(`district name` == "Boston"|
           str_detect(`district name`,"Boston Collegiate"),
         subject == "ELA"|
           subject == "MATH") %>% 
  select(`district name`,subject,`m+e %`)

write.csv(memb_acad,file = file.path('output data/ma_acad.csv'),row.names = FALSE)
