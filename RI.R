rm(list = ls()) # clear working environment
setwd("~/Downloads/States/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("ggplot2")
library(ggplot2)

# data definitions http://www.eride.ri.gov/doc/DataCollections/EnrollmentCensusCollection.pdf
# asp file downloaded and resaved as .xls to be accessible https://www.eride.ri.gov/reports/reports.asp
#---- enrollment -----
ri_enrl <- read_excel("raw_data/RI_summaryreport_excel.xlsx")
str(ri_enrl)
names(ri_enrl) <- tolower(names(ri_enrl))
ri_enrl <- ri_enrl %>% 
  mutate(district = as.factor(district),
         school = as.factor(school),
         amindmale = as.numeric(`native american (male)`),
         amindfem = as.numeric(`native american (female)`),
         asianmale = as.numeric(`asian pacific (male)`),
         asianfem = as.numeric(`asian pacific (female)`),
         blackmale = as.numeric(`black (male)`),
         blackfem = as.numeric(`black (female)`),
         hispmale = as.numeric(`hispanic (male)`),
         hispfem = as.numeric(`hispanic (female)`),
         whitemale = as.numeric(`white (male)`),
         whitefem = as.numeric(`white (female)`),
         multmale = as.numeric(`multi-race (male)`),
         multfem = as.numeric(`multi-race (female)`),
         frplmale = as.numeric(`frl (male)`),
         frplfem = as.numeric(`frl (female)`),
         lepmale = as.numeric(`lep (male)`),
         lepfem = as.numeric(`lep (female)`),
         iepmale = as.numeric(`iep (male)`),
         iepfem = as.numeric(`iep (female)`)) %>% 
  mutate_if(is.numeric, ~replace(.,is.na(.),0)) %>% 
  mutate(amind = (amindmale + amindfem)/total,
         asian = (asianmale + asianfem)/total,
         black = (blackmale + blackfem)/total,
         hispanic = (hispmale + hispfem)/total,
         white = (whitemale + whitefem)/total,
         multiple = (multmale + multfem)/total,
         frpl = (frplmale + frplfem)/total,
         lep = (lepmale + lepfem)/total,
         iep = (iepmale + iepfem)/total)%>% 
  select(district, school, total, amind, asian, black, hispanic, white, multiple, frpl, lep, iep)
bvp <- ri_enrl %>% 
  filter(str_detect(school,"Blackstone Valley")==TRUE) %>% 
  arrange(school)
ri_enrl <- ri_enrl %>% 
  filter(district == "Cumberland"|
           district=="Central Falls"|
           district == "Pawtucket"|
           district == "Lincoln")
bvp$district <- c(NA,"Cumberland","Cumberland","Cumberland",
                  "Cumberland","Central Falls","Lincoln")
ri_comp <- ri_enrl %>% 
  group_by(district) %>% 
  summarize(m_amind = mean(amind),
            m_asian = mean(asian),
            m_black = mean(black),
            m_hispanic = mean(hispanic),
            m_white = mean(white),
            m_multiple = mean(multiple),
            m_frpl = mean(frpl),
            m_lep = mean(lep),
            m_iep = mean(iep),
            sd_amind = sd(amind),
            sd_asian = sd(asian),
            sd_black = sd(black),
            sd_hispanic = sd(hispanic),
            sd_white = sd(white),
            sd_multiple = sd(multiple),
            sd_frpl = sd(frpl),
            sd_lep = sd(lep),
            sd_iep = sd(iep))
ri_stan_enrl <- merge(bvp, ri_comp, by="district")
ri_stan_enrl <- ri_stan_enrl %>% 
  mutate(comp_amind = (amind - m_amind)/sd_amind,
         comp_asian = (asian - m_asian)/sd_asian,
         comp_black = (black - m_black)/sd_black,
         comp_hispanic = (hispanic - m_hispanic)/sd_hispanic,
         comp_white = (white - m_white)/sd_white,
         comp_multiple = (multiple - m_multiple)/sd_multiple,
         comp_frpl = (frpl - m_frpl)/sd_frpl,
         comp_lep = (lep - m_lep)/sd_lep,
         comp_iep = (iep - m_iep)/sd_iep) %>% 
  select(district, school, total, amind, m_amind, comp_amind,
         asian, m_asian, comp_asian, black, m_black, comp_black,
         hispanic, m_hispanic, comp_hispanic, white, m_white, comp_white,
         multiple, m_multiple, comp_multiple, frpl, m_frpl, comp_frpl,
         lep, m_lep, comp_lep, iep, m_iep, comp_iep)
write.csv(ri_stan_enrl, file = file.path("output_data/ri_stan_enrl.csv"), row.names = FALSE)

#---- academic performance ----- 
# RICAS data from https://www3.ride.ri.gov/ADP#
ri_ela <- read_excel("raw_data/RICAS_-_English_Language_Arts_Literacy__2018-19_school_QuickReport.xlsx")
names(ri_ela) <- tolower(names(ri_ela))
str(ri_ela)
ri_ela <- ri_ela %>% 
  mutate(school = as.factor(schoolname),
         district = as.factor(districtname),
         n_ela = as.numeric(`number tested`),
         p_meetexc = as.numeric(`percent meeting or exceeding expectations`)*.01) %>% 
  select(school, district, n_ela, p_meetexc) %>% 
  filter(str_detect(school,"Blackstone Valley")==TRUE|
           district == "Cumberland"|
           district=="Central Falls"|
           district == "Pawtucket"|
           district == "Lincoln")%>% 
  na.omit()
ri_ela_comp <- ri_ela %>% 
  group_by(district) %>% 
  summarize(m_meetexc = mean(p_meetexc),
            sd_meetexc = sd(p_meetexc))
ri_memb <- ri_ela %>% 
  filter(str_detect(school, "Blackstone Valley")==TRUE)
ri_memb$district <- c("Cumberland","Cumberland","Cumberland",
                      "Central Falls","Lincoln")
ri_stan_ela <- merge(ri_memb, ri_ela_comp, by="district")
ri_stan_ela<- ri_stan_ela %>% 
  mutate(comp_ela =  (p_meetexc - m_meetexc)/sd_meetexc) %>% 
  select(district,school,n_ela,p_meetexc,m_meetexc,comp_ela)
write.csv(ri_stan_ela,file = file.path("output_data/ri_stan_ela.csv"),row.names = FALSE)
ri_ela <- read_excel("raw_data/RICAS_-_English_Language_Arts_Literacy__2018-19_school_QuickReport.xlsx")
names(ri_ela) <- tolower(names(ri_ela))
str(ri_ela)
ri_ela <- ri_ela %>% 
  mutate(school = as.factor(schoolname),
         district = as.factor(districtname),
         n_ela = as.numeric(`number tested`),
         p_meetexc = as.numeric(`percent meeting or exceeding expectations`)*.01) %>% 
  select(school, district, n_ela, p_meetexc) %>% 
  filter(str_detect(school,"Blackstone Valley")==TRUE|
           district == "Cumberland"|
           district=="Central Falls"|
           district == "Pawtucket"|
           district == "Lincoln")%>% 
  na.omit()
ri_ela_comp <- ri_ela %>% 
  group_by(district) %>% 
  summarize(m_meetexc = mean(p_meetexc),
            sd_meetexc = sd(p_meetexc))
ri_memb <- ri_ela %>% 
  filter(str_detect(school, "Blackstone Valley")==TRUE)
ri_memb$district <- c("Cumberland","Cumberland","Cumberland",
                      "Central Falls","Lincoln")
ri_stan_ela <- merge(ri_memb, ri_ela_comp, by="district")
ri_stan_ela<- ri_stan_ela %>% 
  mutate(comp_ela =  (p_meetexc - m_meetexc)/sd_meetexc) %>% 
  select(district,school,n_ela,p_meetexc,m_meetexc,comp_ela)
write.csv(ri_stan_ela,file = file.path("output_data/ri_stan_ela.csv"),row.names = FALSE)

ri_math <- read_excel("raw_data/RICAS_-_Mathematics_2018-19_school_QuickReport.xlsx")
names(ri_math) <- tolower(names(ri_math))
str(ri_math)
ri_math <- ri_math %>% 
  mutate(school = as.factor(schoolname),
         district = as.factor(districtname),
         n_math = as.numeric(`number tested`),
         p_meetexc = as.numeric(`percent meeting or exceeding expectations`)*.01) %>% 
  select(school, district, n_math, p_meetexc) %>% 
  filter(str_detect(school,"Blackstone Valley")==TRUE|
           district == "Cumberland"|
           district=="Central Falls"|
           district == "Pawtucket"|
           district == "Lincoln")%>% 
  na.omit()
ri_math_comp <- ri_math %>% 
  group_by(district) %>% 
  summarize(m_meetexc = mean(p_meetexc),
            sd_meetexc = sd(p_meetexc))
ri_memb <- ri_math %>% 
  filter(str_detect(school, "Blackstone Valley")==TRUE)
ri_memb$district <- c("Cumberland","Cumberland","Cumberland",
                      "Central Falls","Lincoln")
ri_stan_math <- merge(ri_memb, ri_math_comp, by="district")
ri_stan_math<- ri_stan_math %>% 
  mutate(comp_math =  (p_meetexc - m_meetexc)/sd_meetexc) %>% 
  select(district,school,n_math,p_meetexc,m_meetexc,comp_math)
write.csv(ri_stan_math,file = file.path("output_data/ri_stan_math.csv"),row.names = FALSE)
