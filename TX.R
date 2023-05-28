# --- Amy Jiravisitcul. 21 May 2023 ----
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

# Data downloaded
# https://rptsvr1.tea.texas.gov/adhocrpt/adste.html
# --- STATEWIDE SCHOOL-LEVEL ETHNICITY ENROLLMENT FILE ------
enrl <- read.csv('raw data/TX_Enrollment Report_Statewide_Campuses_Ethnicity_2022-2023.csv', header = FALSE)
names(enrl) <- tolower(enrl[3,])
enrl <- enrl[-(1:3),]
str(enrl)
enrl <- enrl %>% 
  mutate(county = as.factor(`county name`),
         district = as.factor(`district name`),
         school = as.factor(`campus name`),
         race = as.factor(ethnicity),
         n = as.numeric(gsub('[^0-9.-]',"",enrollment))) %>% 
  select(county, district, school:n)
levels(enrl$race) <- c("","amind","asian","black","hisp","nhpi","mult","white")


memb_enrl <- enrl %>% 
  mutate(district = case_when(school == "MAGNOLIA MONTESSORI FOR ALL" ~ "AUSTIN ISD",
                              school == "THE GATHERING PLACE" ~ "NORTHSIDE ISD")) %>% 
  filter(str_detect(school, "MAGNOLIA MONTESSORI")|
           str_detect(school, "THE GATHERING")) %>% 
  spread(race,n) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(total = amind + asian + black +hisp + mult+ white,
         amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         white = white/total,
         mult = mult/total) %>% 
  select(school, district, county, total, amind:white, mult)

enrl <- enrl %>% 
  mutate(district = case_when(school == "MAGNOLIA MONTESSORI FOR ALL" ~ "AUSTIN ISD",
                              school == "THE GATHERING PLACE" ~ "NORTHSIDE ISD",
                              TRUE ~ district))
         
dist_agg <- enrl %>% 
  filter(district == "AUSTIN ISD"|
           district == "NORTHSIDE ISD") %>% 
  spread(race,n) %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  mutate(total = amind + asian + black +hisp + mult+ white) %>% 
  group_by(district) %>% 
  summarize(dist_total = sum(total),
            dist_amind = sum(amind)/sum(total),
            dist_asian = (sum(asian) + sum(nhpi))/sum(total),
            dist_black = sum(black)/sum(total),
            dist_hisp = sum(hisp)/sum(total),
            dist_white = sum(white)/sum(total),
            dist_mult = sum(mult)/sum(total))

memb_enrl <- merge(memb_enrl, dist_agg)

d_ls <- enrl %>% 
  filter(district == "AUSTIN ISD") %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist) %>% 
  filter(school == "MAGNOLIA MONTESSORI FOR ALL")

d_ls <- enrl %>% 
  filter(district == "NORTHSIDE ISD") %>%
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist) %>% 
  filter(str_detect(school, "THE GATHERING")) %>% 
  rbind(d_ls)

memb_enrl <- memb_enrl %>% 
  merge(rbind(d_ls))

cty_agg <- enrl %>% 
  filter(county == "BEXAR COUNTY"|
           county == "TRAVIS COUNTY") %>% 
  spread(race,n) %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  mutate(total = amind + asian + black +hisp + mult+ white) %>% 
  group_by(county) %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(amind)/sum(total),
            cty_asian = (sum(asian) + sum(nhpi))/sum(total),
            cty_black = sum(black)/sum(total),
            cty_hisp = sum(hisp)/sum(total),
            cty_white = sum(white)/sum(total),
            cty_mult = sum(mult)/sum(total))

memb_enrl <- merge(memb_enrl, cty_agg)

c_ls <- enrl %>% 
  filter(county == "TRAVIS COUNTY") %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_cty = ls) %>% 
  select(school, ls_cty) %>% 
  filter(school == "MAGNOLIA MONTESSORI FOR ALL")

c_ls <- enrl %>% 
  filter(county == "BEXAR COUNTY") %>%
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_cty = ls) %>% 
  select(school, ls_cty) %>% 
  filter(str_detect(school, "THE GATHERING")) %>% 
  rbind(c_ls)
memb_enrl <- merge(memb_enrl, c_ls)
# SWD, EL, FRPL ------
# Data downloaded from https://rptsvr1.tea.texas.gov/adhocrpt/adspr.html
subgroups <- read.csv('raw data/TX_StudPgmStateCampus23state.csv',header = FALSE)
names(subgroups) <- tolower(subgroups[4,])
subgroups <- subgroups[-(1:4),]
str(subgroups)

subgroups <- subgroups %>% 
  mutate(school = as.factor(`campus name`),
         district = as.factor(`district name`),
         county = as.factor(`county name`),
         total = as.numeric(`all enrollment`),
         ecdis = as.numeric(`economically disadvantaged`),
         el = as.numeric(`emergent bilingual/english learner`),
         swd = as.numeric(`special education`)) %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  select(school:swd)

memb_sub <- subgroups %>% 
  filter(str_detect(school, "MAGNOLIA MONTE")|
           str_detect(school, "THE GATHERING")) %>% 
  mutate(ecdis = ecdis/total,
         el = el/total,
         swd = swd/total) %>% 
  select(-total, -district)

memb_enrl <- merge(memb_enrl,memb_sub)

# Downloaded statewide county totals
agg_sub <- read.csv('raw data/TX_StudPgmStateCounty23state.csv', header = FALSE)
names(agg_sub) <- tolower(agg_sub[4,])
agg_sub <- agg_sub[-(1:4),]
str(agg_sub)

agg_sub <- agg_sub %>% 
  mutate(county = as.factor(`county name`),
         total = as.numeric(`all enrollment`),
         cty_ecdis = as.numeric(`economically disadvantaged`)/total,
         cty_el = as.numeric(`emergent bilingual/english learner`)/total,
         cty_swd = as.numeric(`special education`)/total,
         cty_total = total) %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  filter(county == "BEXAR COUNTY"|
           county == "TRAVIS COUNTY"& region == "13") %>% 
  select(county, cty_el, cty_swd, cty_ecdis)

memb_enrl <- merge(memb_enrl, agg_sub)

# Downloaded statewide district totals
agg_sub <- read.csv('raw data/TX_StudPgmStateDistrict23state.csv', header = FALSE)
names(agg_sub) <- tolower(agg_sub[4,])
agg_sub <- agg_sub[-(1:4),]
str(agg_sub)

agg_sub <- agg_sub %>% 
  mutate(district = as.factor(`district name`),
         total = as.numeric(`all enrollment`),
         dist_ecdis = as.numeric(`economically disadvantaged`)/total,
         dist_el = as.numeric(`emergent bilingual/english learner`)/total,
         dist_swd = as.numeric(`special education`)/total,
         dist_total = total) %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  filter(district == "AUSTIN ISD"|
           district == "NORTHSIDE ISD" & region == "20") %>% 
  select(district, dist_el, dist_swd, dist_ecdis)

memb_enrl <- merge(memb_enrl,agg_sub) %>% 
  select(school, total, amind:hisp, white, mult, ecdis, el, swd, ls_dist, ls_cty, 
         district, dist_total, dist_amind:dist_mult, dist_ecdis, dist_el, dist_swd,
         county, cty_total:cty_mult,cty_ecdis, cty_el, cty_swd)

write.csv(memb_enrl, file = file.path('output data/tx_enrl.csv'), row.names = FALSE)

# ------ ACADEMIC ASSESSMENT DATA -----
# Downloaded from https://rptsvr1.tea.texas.gov/perfreport/tapr/2022/xplore/DownloadSelData.html
# documentation: https://rptsvr1.tea.texas.gov/perfreport/tapr/2022/xplore/dstaar_all.html 
acad <- read.csv('raw data/TX_DSTAAR_ALL.csv')
names(acad) <- tolower(names(acad))
str(acad)
acad <- acad %>% 
  mutate(d_code = district,
         district = as.factor(distname),
         n_math = (as.numeric(dda00am01322r) + as.numeric(dda00am01222r))* .01, # masters plus meets grade standards for all grades
         n_ela = (as.numeric(dda00ar01322r) + as.numeric(dda00ar01222r))* .01) %>%
  select(d_code, district, n_math, n_ela)

memb_acad <- acad %>% 
  filter(district == "AUSTIN ISD"|
           district == "NORTHSIDE ISD"|
           district == "MONTESSORI FOR ALL"|
           district == "THE GATHERING PLACE")

write.csv(memb_acad, file = file.path('output data/tx_acad.csv'), row.names = FALSE)
