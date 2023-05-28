# --- Amy Jiravisitcul. 23 May 2023 ----
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

# ---- 
# data definitions http://www.eride.ri.gov/doc/DataCollections/EnrollmentCensusCollection.pdf
# asp file downloaded and resaved as .xls to be accessible https://www.eride.ri.gov/reports/reports.asp

# --- BVP - ALL SCHOOLS - OCTOBER ENROLLMENT - 2022-2023 ------
enrl <- read_excel('raw data/RI_download.xlsx')
names(enrl) <- tolower(names(enrl))
enrl <- enrl %>% 
  mutate(district = as.factor(district),
         school = as.factor(school),
         total = as.numeric(total),
         amind = as.numeric(gsub("[^0-9.-]",0,`native american (female)`)) +
           as.numeric(gsub("[^0-9.-]",0,`native american (male)`)),
         asian = as.numeric(gsub("[^0-9.-]",0,`asian pacific (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`asian pacific (male)`)),
         black = as.numeric(gsub("[^0-9.-]",0,`black (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`black (male)`)),
         hisp = as.numeric(gsub("[^0-9.-]",0,`hispanic (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`hispanic (male)`)),
         white = as.numeric(gsub("[^0-9.-]",0,`white (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`white (male)`)),
         mult = as.numeric(gsub("[^0-9.-]",0,`multi-race (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`multi-race (male)`)),
         frl = as.numeric(gsub("[^0-9.-]",0,`frl (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`frl (male)`)),
         lep = as.numeric(gsub("[^0-9.-]",0,`lep (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`lep (male)`)),
         swd = as.numeric(gsub("[^0-9.-]",0,`iep (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`iep (male)`))) %>% 
  select(district, school,total:swd)
str(bvp)
bvp <- enrl %>% 
  filter(str_detect(school, "Blackstone Valley")) %>% 
  mutate(amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         white = white/total,
         mult = mult/total,
         frl = frl/total,
         lep = lep/total,
         swd = swd/total) %>% 
  arrange(school)

bvp$district = c("Cumberland", # Add comparison districts based on geographic proximity
                   "Cumberland",
                   "Cumberland",
                   "Cumberland",
                   "Cumberland",
                   "Central Falls",
                   "Lincoln")
bvp %>% select(school, district)
enrl <- enrl %>% 
  mutate(district = case_when(school == "Blackstone Valley Prep Admin School" ~ "Cumberland",
                              school == "Blackstone Valley Prep Elementary 2 School" ~ "Cumberland",
                              school == "Blackstone Valley Prep Elementary 3 School" ~ "Cumberland",
                              school == "Blackstone Valley Prep Elementary School" ~ "Cumberland",
                              school == "Blackstone Valley Prep High School" ~ "Cumberland",
                              school == "Blackstone Valley Prep Junior High School" ~ "Central Falls",
                              school == "Blackstone Valley Prep Upper Elementary School" ~ "Lincoln",
                              TRUE ~ district))

# ------ DISTRICT AND STATE-WIDE REPRESENTATION ------
cumberland <- enrl %>% 
  filter(district == "Cumberland") %>% 
  select(district, school, amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  filter(str_detect(school, "Blackstone Valley Prep")) %>% 
  select(school, ls_dist)

lincoln <- enrl %>% 
  filter(district == "Lincoln") %>% 
  select(district, school, amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  filter(str_detect(school, "Blackstone Valley Prep")) %>% 
  select(school, ls_dist)

central_falls <- enrl %>% 
  filter(district == "Central Falls") %>% 
  select(district, school, amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>%
  filter(str_detect(school, "Blackstone Valley Prep")) %>% 
  select(school, ls_dist)

bvp <- enrl %>% 
  merge(rbind(cumberland, central_falls, lincoln)) %>% 
  select(school, ls_dist) %>% 
  mutate(state = "Rhode Island") %>% 
  merge(bvp)

ls_ri <- enrl %>% 
  select(school, amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE) %>% 
  mutate(ls_ri = ls) %>%
  filter(str_detect(school, "Blackstone Valley Prep")) %>% 
  select(school, ls_ri)

bvp <- merge(bvp,ls_ri)

# aggregates by state and district
bvp <- enrl %>% 
  group_by(district) %>% 
  summarize(dist_total = sum(total),
            dist_amind = sum(amind)/dist_total,
            dist_asian = sum(asian)/dist_total,
            dist_black = sum(black)/dist_total,
            dist_hisp = sum(hisp)/dist_total,
            dist_white = sum(white)/dist_total,
            dist_mult = sum(mult)/dist_total,
            dist_frl = sum(frl)/dist_total,
            dist_lep = sum(lep)/dist_total,
            dist_swd = sum(swd)/dist_total) %>% 
  filter(district == "Central Falls"|
           district == "Cumberland"|
           district == "Lincoln") %>% 
  merge(bvp)

bvp <- enrl %>% 
  mutate(state = "Rhode Island") %>% 
  group_by(state) %>% 
  summarize(ri_total = sum(total),
            ri_amind = sum(amind)/ri_total,
            ri_asian = sum(asian)/ri_total,
            ri_black = sum(black)/ri_total,
            ri_hisp = sum(hisp)/ri_total,
            ri_white = sum(white)/ri_total,
            ri_mult = sum(mult)/ri_total,
            ri_frl = sum(frl)/ri_total,
            ri_lep = sum(lep)/ri_total,
            ri_swd = sum(swd)/ri_total) %>% 
  merge(bvp)
names(bvp)
rm(ls_ri)
bvp <- bvp %>% 
  select(school,total:swd, ls_dist, ls_ri, district:dist_swd, state, ri_total:ri_swd)

write.csv(bvp, file = file.path('output data/ri_enrl.csv'), row.names = FALSE)
