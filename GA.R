# --- Amy Jiravisitcul. 16 May 2023 ----
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

#--- ENROLLMENT BY RACE AND ETHNICITY -----
# data imported from https://oraapp.doe.k12.ga.us/ows-bin/owa/fte_pack_ethnicsex_pub.entry_form
# All Schools All districts from 5 Oct 2021
enrl <- read.csv("raw data/GA_FTE Enrollment by Race_Ethnicity and Gender Fiscal Year2023-3 Data Report.csv", header = FALSE)
str(enrl)
enrl <- enrl[-c(1:4),] # Remove rows with no data
names(enrl) <- tolower(enrl[1,]) # rename columns to text in row 1
enrl <- enrl[-1,]

names(enrl) <- c("dist_code","district","school","gender",
                 "c_hisp","c_amind","c_asian","c_black",
                 "c_pi","c_white","c_mult")
enrl <- enrl %>% 
  mutate(c_hisp = as.numeric(gsub("[^0-9.-]","",c_hisp)), 
         c_amind = as.numeric(gsub("[^0-9.-]","",c_amind)),
         c_asian = as.numeric(gsub("[^0-9.-]","",c_asian)) + as.numeric(gsub("[^0-9.-]","",c_pi)),
         c_black = as.numeric(gsub("[^0-9.-]","",c_black)),
         c_white = as.numeric(gsub("[^0-9.-]","",c_white)),
         c_mult = as.numeric(gsub("[^0-9.-]","",c_mult)))
enrl[is.na(enrl)] = 0 # replace asterisks with 0

enrl <- enrl %>% 
  mutate(total = c_hisp + c_amind + c_asian + c_black + c_white + c_mult,
         amind = c_amind/total,
         asian = c_asian/total,
         black = c_black/total,
         hisp = c_hisp/total,
         white = c_white/total,
         mult = c_mult/total)%>% 
  select(dist_code, district, school, gender, total, amind, asian, black, hisp, white, mult,
         c_amind, c_asian, c_black, c_hisp, c_white, c_mult) %>% 
  na.omit() %>% 
  filter(gender == "Total")

memb_enrl <- enrl %>% 
  filter(str_detect(school, "Atlanta Neigh")|
           str_detect(school, "Museum")|
           str_detect(school, "Susie King Ta"))

# ---- LOCAL SEGREGATION BY DISTRICT ----
dist_enrl <- enrl %>% 
  filter(district == "Atlanta Public Schools"|
           district == "DeKalb County"|
           district == "Savannah-Chatham County") 
names(dist_enrl)
unique(dist_enrl$district)

sav_locseg <- dist_enrl %>% 
  filter(district == "Savannah-Chatham County") %>% 
  select(dist_code,district, school, total, c_amind:c_mult) %>% 
  gather(subgroup, total, c_amind:c_mult, factor_key=TRUE) %>%  # change from wide to long
  mutual_local("subgroup","school", weight = "total", wide = TRUE) %>% 
  arrange(ls)

memb_locseg <- sav_locseg %>% 
  filter(str_detect(school,"Susie King Taylor")) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)

atl_locseg <- dist_enrl %>% 
  filter(district == "Atlanta Public Schools") %>% 
  select(dist_code,district, school, total, c_amind:c_mult) %>% 
  gather(subgroup, total, c_amind:c_mult, factor_key=TRUE) %>%  # change from wide to long
  mutual_local("subgroup","school", weight = "total", wide = TRUE) %>% 
  arrange(ls) %>% 
  filter(str_detect(school,"Atlanta Neigh")) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)

dek_locseg <- dist_enrl %>% 
  filter(district == "DeKalb County") %>% 
  select(dist_code,district, school, total, c_amind:c_mult) %>%
  gather(subgroup, total, c_amind:c_mult, factor_key=TRUE) %>% # change from wide to long
  mutual_local("subgroup","school", weight = "total", wide = TRUE) %>% 
  arrange(ls) %>% 
  filter(str_detect(school,"Museum")) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)

memb_locseg <- rbind(memb_locseg,atl_locseg, dek_locseg)
memb_enrl <- merge(memb_enrl, memb_locseg)

# ---- STATEWIDE AND DISTRICT AGGREGATES ----
agg <- dist_enrl %>% 
  group_by(district) %>% # district wide totals and percentages for DeKalb, APS, Sav-Chat
  summarize(dist_total = sum(total),
            dist_amind = sum(c_amind)/dist_total,
            dist_asian = sum(c_asian)/dist_total,
            dist_black = sum(c_black)/dist_total,
            dist_hisp = sum(c_hisp)/dist_total,
            dist_white = sum(c_white)/dist_total,
            dist_mult = sum(c_mult)/dist_total)
memb_enrl <- merge(memb_enrl,agg) # Add district wide aggregate columns to clean data
state <- enrl[1,c(5:11)]  # Select statewide totals row
names(state) <- c("ga_total","ga_amind","ga_asian","ga_black","ga_hisp","ga_white","ga_mult")
state$dummy <- "dummy"
memb_enrl$dummy = "dummy"
memb_enrl <- merge(memb_enrl,state) # Add statewide aggregate columns to clean data

# ---- LOCAL SEGREGATION BY STATE ----
ga_locseg <- enrl %>% 
  filter(dist_code != "") %>% # Remove statewide totals row
  select(dist_code,district, school, total, c_amind:c_mult) %>%
  gather(subgroup, total, c_amind:c_mult, factor_key=TRUE) %>%  # change from wide to long
  mutual_local("subgroup","school", weight = "total", wide = TRUE) %>% 
  arrange(ls) %>% 
  mutate(ls_ga = ls) %>% 
  select(school, ls_ga)


memb_enrl <- merge(memb_enrl,ga_locseg,by="school") # Add Georgia local segregation numbers by campus
#---- FRPM ----
# frpm data imported from https://oraapp.doe.k12.ga.us/ows-bin/owa/fte_pack_frl001_public.entry_form
# All Schools All Districts. Data 
frpm <- read.csv("raw data/GA_Free Reduced Lunch (FRL) Fiscal Year2023 Data Report.csv", header = FALSE)
frpm <- frpm[-c(1:4),] # Remove rows with no data
names(frpm) <- tolower(frpm[1,]) # rename columns to text in row 1
frpm <- frpm[-1,]
names(frpm) <- c("dist_code","district","school","frpm")
memb_frpm <- frpm %>% 
  filter(str_detect(school, "Atlanta Neigh")| # filter to members only
           str_detect(school, "Museum")|
           str_detect(school, "Susie King Ta")) %>% 
  mutate(frpm = as.numeric(frpm)*.01) %>% 
  select(school, district, frpm)

# Download and save report from "ALL Districts" to get state and district totals
# Rename file to distinguish from school data set
dist_frpm <- read.csv("raw data/GA_agg_Free Reduced Lunch (FRL) Fiscal Year2023 Data Report.csv",
                      header = FALSE)
dist_frpm <- dist_frpm[-c(1:5),] # Remove rows with no data
names(dist_frpm) <- c("dist_code","district","dist_frpm") # match column names from earlier
dist_frpm <- dist_frpm %>% 
  filter(district == "State-Wide Total"| # Ended up being .5618
           district == "Atlanta Public Schools"|
           district == "DeKalb County"|
           district == "Savannah-Chatham County") %>% 
  mutate(dist_frpm = as.numeric(dist_frpm)*.01) %>% 
  select(district, dist_frpm)
memb_frpm <- merge(memb_frpm,dist_frpm)
memb_frpm$ga_frpm = .5931
# Match the memb_enrl dataset
memb_frpm$school <- str_sub(memb_frpm$school,9)
memb_enrl$school <- str_sub(memb_enrl$school,6)
memb_enrl <- merge(memb_enrl, memb_frpm, by= c("school","district"))

#---- EL AND SWD -----
# data from https://download.gosa.ga.gov/2022/Enrollment_by_Subgroups_Programs_2022_Dec072022.csv
elswd <- read.csv("raw data/GA_Enrollment_by_Subgroups_Programs_2022_Dec072022.csv")
str(elswd)
names(elswd) <- tolower(names(elswd))
levels(as.factor(elswd$detail_lvl_desc))
memb_elswd <- elswd %>% 
  mutate(school = instn_name,
         swd = as.numeric(enroll_percent_swd) * .01,
         lep = as.numeric(enroll_percent_lep) * .01) %>% 
  filter(str_detect(instn_name, "Atlanta Neigh")| # filter to members only
           str_detect(instn_name, "Museum")|
           str_detect(instn_name, "Susie King Ta")) %>%
  select(school, swd, lep) %>% arrange(school)

agg_elswd <- elswd %>% 
  filter(detail_lvl_desc == "State"|
           instn_name == "All Column Values") %>% 
  mutate(district = school_dstrct_nm,
         dist_swd = as.numeric(enroll_percent_swd) * .01,
         dist_lep = as.numeric(enroll_percent_lep) * .01) %>% 
  select(detail_lvl_desc, district, dist_swd, dist_lep)

agg_elswd <- agg_elswd %>% 
  filter(district == "All Column Values"| # This row is GA totals
           district == "Atlanta Public Schools"|
           district == "DeKalb County"|
           district == "Savannah-Chatham County") %>% 
  arrange(district) %>% 
  select(district, dist_swd, dist_lep)

# Add district name for easy matching during the data frame merge
memb_elswd$district = c("Atlanta Public Schools", "Atlanta Public Schools",
                        "DeKalb County","Savannah-Chatham County")
memb_elswd <- merge(memb_elswd,agg_elswd)

memb_elswd$ga_lep = 0.10
memb_elswd$ga_swd = .127 # manually add values from agg_elswd[1,]

memb_enrl <- merge(memb_elswd,memb_enrl,by=c("school","district"))

#---- RE-ORDER COLUMNS FOR EASY TRANSFER TO MEMBER LOG -----
sort(names(memb_enrl))
memb_enrl <- memb_enrl %>% 
  select(school, total, amind, asian, black, hisp, white, mult,
         frpm, lep, swd, ls_dist,ls_ga, district, dist_total, 
         dist_amind, dist_asian, dist_black, dist_hisp, dist_white, 
         dist_mult, dist_frpm, dist_lep, dist_swd,, ga_total, ga_amind,
         ga_asian, ga_black, ga_hisp, ga_white, ga_mult,ga_frpm,ga_lep,ga_swd)

write.csv(memb_enrl, file = file.path('output data/ga_enrl.csv'),row.names = FALSE)

# ---- ASSESSMENT EOG MILESTONES -----
acad <- read.csv('raw data/GA_EOG_2022_DEC082022.csv')
names(acad) <- tolower(names(acad))
str(acad)
levels(as.factor(acad$school_dstrct_nm))

acad <- acad %>% 
  mutate(test = as.factor(test_cmpnt_typ_nm),
         school = as.factor(instn_name),
         district = as.factor(school_dstrct_nm),
         n_tested = as.numeric(num_tested_cnt),
         n_prof = as.numeric(proficient_cnt)+as.numeric(distinguished_cnt)) %>% 
  filter(test == "English Language Arts"|
           test == "Mathematics",
         subgroup_name == "All Students",
         district == "Atlanta Public Schools"|
           district == "DeKalb County"|
           district == "Savannah-Chatham County") %>% 
  select(test:n_prof,proficient_pct,distinguished_pct)

memb_acad <- acad %>% 
  filter(str_detect(school, "Atlanta Neigh")|
           str_detect(school, "Museum")|
           str_detect(school, "Susie King Ta")) %>% 
  mutate(p_prof = (as.numeric(proficient_pct) + as.numeric(distinguished_pct))*.01,
         n_prof = p_prof * n_tested) %>% 
  select(district, school,n_tested, test, p_prof) %>% 
  spread(key = test, value = p_prof)
names(memb_acad)[4:5] <- c("p_ela","p_math")

agg_acad <- acad %>% 
  filter(school == "ALL") %>% 
  mutate(dist_prof = n_prof/n_tested) %>% 
  select(district, test, dist_prof) %>% 
  spread(key = test, value = dist_prof)

names(agg_acad)[2:3] <- c("dist_ela","dist_math")

memb_acad <- merge(memb_acad,agg_acad) %>% select(school, district,n_tested, p_math,dist_math,p_ela,dist_ela)
write.csv(memb_acad,file = file.path('output data/ga_acad.csv'),row.names = FALSE)
# ---- END ------
