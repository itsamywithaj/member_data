rm(list = ls()) # clear working environment
setwd("~/Downloads/States/")
install.packages("backports")
library(backports)
install.packages(c("tidyverse","readxl","ggplot2"),
                 dependencies = TRUE,
                 repos = c("http://rstudio.org/_packages",
                           "http://cran.rstudio.com"))
library(tidyverse)
library(readxl)

# ----enrollment ------
# data imported from http://apps.schools.nc.gov/ords/f?p=145:73:::NO:::

nc_en <- read_excel("raw_data/NC_Pupils_by_Race_and_Sex.xlsx")
names(nc_en) <- tolower(nc_en[2,])
nc_en <- nc_en[-c(1:2),]
str(nc_en)
memb_enrl <- nc_en %>% 
  filter(`charter school name`=="Charlotte Lab School"|
           `charter school name`=="Central Park School For Children") %>% 
  mutate(total = as.numeric(total),
         amind = (as.numeric(`indian male`)+as.numeric(`indian female`))/total,
         asian = (as.numeric(`asian male`)+as.numeric(`asian female`)+
                    as.numeric(`pacific island  male`)+as.numeric(`pacific island  female`))/total,
         black = (as.numeric(`black male`)+as.numeric(`black female`))/total,
         hispanic = (as.numeric(`hispanic male`)+as.numeric(`hispanic female`))/total,
         white = (as.numeric(`white male`)+as.numeric(`white female`))/total,
         multiple = (as.numeric(`two or more male`)+as.numeric(`two or more female`))/total)%>% 
  select(`charter school name`,total,amind,asian,black,hispanic,white,multiple)
# comparative data for Charlotte
# http://apps.schools.nc.gov/ords/f?p=145:220:9220123861332::NO::P220_SELECTLEA:600
char_en <- read.csv("raw_data/NC_report.csv")
str(char_en)
names(char_en) <- tolower(names(char_en))
char_en <- char_en[-nrow(char_en),]
names(char_en)[3] <- "district"
char_en <- char_en %>% 
  mutate(total = as.numeric(gsub(",","",total)),
         amind = (as.numeric(indian.male)+as.numeric(indian.female))/total,
         asian = (as.numeric(asian.male)+as.numeric(asian.female)+
                    as.numeric(pacific.island..male)+as.numeric(pacific.island..female))/total,
         black = (as.numeric(black.male)+as.numeric(black.female))/total,
         hispanic = (as.numeric(hispanic.male)+as.numeric(hispanic.female))/total,
         white = (as.numeric(gsub(",","",white.male))+as.numeric(gsub(",","",white.female)))/total,
         multiple = (as.numeric(two.or.more.male)+as.numeric(two.or.morefemale))/total)%>% 
  select(district,school.name,total,amind,asian,black,hispanic,white,multiple) %>% 
  group_by(district) %>% 
  summarize(m_amind = mean(amind),
            m_asian = mean(asian),
            m_black = mean(black),
            m_hispanic = mean(hispanic),
            m_white = mean(white),
            m_multiple = mean(multiple),
            sd_amind = sd(amind),
            sd_asian = sd(asian),
            sd_black = sd(black),
            sd_hispanic = sd(hispanic),
            sd_white = sd(white),
            sd_multiple = sd(multiple))

# comparative data for Durham
# http://apps.schools.nc.gov/ords/f?p=145:220:9220123861332::NO::P220_SELECTLEA:600
dur_en <- read.csv("raw_data/NC_report2.csv")
str(dur_en)
names(dur_en) <- tolower(names(dur_en))
dur_en <- dur_en[-nrow(dur_en),]
names(dur_en)[3] <- "district"
dur_en <- dur_en %>% 
  mutate(total = as.numeric(gsub(",","",total)),
         amind = (as.numeric(indian.male)+as.numeric(indian.female))/total,
         asian = (as.numeric(asian.male)+as.numeric(asian.female)+
                    as.numeric(pacific.island..male)+as.numeric(pacific.island..female))/total,
         black = (as.numeric(black.male)+as.numeric(black.female))/total,
         hispanic = (as.numeric(hispanic.male)+as.numeric(hispanic.female))/total,
         white = (as.numeric(gsub(",","",white.male))+as.numeric(gsub(",","",white.female)))/total,
         multiple = (as.numeric(two.or.more.male)+as.numeric(two.or.morefemale))/total)%>% 
  select(district,school.name,total,amind,asian,black,hispanic,white,multiple) %>% 
  group_by(district) %>% 
  summarize(m_amind = mean(amind),
            m_asian = mean(asian),
            m_black = mean(black),
            m_hispanic = mean(hispanic),
            m_white = mean(white),
            m_multiple = mean(multiple),
            sd_amind = sd(amind),
            sd_asian = sd(asian),
            sd_black = sd(black),
            sd_hispanic = sd(hispanic),
            sd_white = sd(white),
            sd_multiple = sd(multiple))
memb_enrl$district <- c("Durham County Schools","Charlotte-Mecklenburg County Schools")
z <- rbind(char_en,dur_en)
nc_stan_enrl <- merge(memb_enrl,z,by="district")
nc_stan_enrl <- nc_stan_enrl %>% 
  mutate(comp_amind = (amind - m_amind)/sd_amind,
         comp_asian = (asian - m_asian)/sd_asian,
         comp_black = (black - m_black)/sd_black,
         comp_hispanic = (hispanic - m_hispanic)/sd_hispanic,
         comp_white = (white - m_white)/sd_white,
         comp_multiple = (multiple - m_multiple)/sd_multiple) %>% 
  select(district, `charter school name`,total,amind, m_amind, comp_amind,
         asian, m_asian, comp_asian, black, m_black, comp_black,
         hispanic, m_hispanic, comp_hispanic, white, m_white, comp_white,
         multiple, m_multiple, comp_multiple)
write.csv(nc_stan_enrl, file = file.path("output_data/nc_stan_enrl.csv"),row.names = FALSE)

# ---- frpl ----
# LEA 2019-2020 data from https://childnutrition.ncpublicschools.gov/information-resources/eligibility/data-reports/data-reports
nc_frpl <- read_excel("raw_data/NC_sy2019-20_eds-preliminary.xlsx")
str(nc_frpl)
names(nc_frpl) <- tolower(nc_frpl[5,])
nc_frpl <- nc_frpl[-c(1:5),]
summary(as.factor(nc_frpl$`% eds`))
nc_frpl <- nc_frpl %>% 
  mutate(district = as.factor(`sfa name`),
         school = as.factor(`site name`),
         adm = as.numeric(adm),
         free = as.numeric(free),
         reduced = as.numeric(reduced)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(c_fr = free + reduced,
         frpl = c_fr/adm) %>% 
  select(district, school, adm, free, reduced, c_fr, frpl) %>% 
  filter(district == "Charlotte-Mecklenburg Schools"|
           district == "Durham Public Schools")
summary_frpl <- nc_frpl %>% 
  group_by(district) %>% 
  summarize(m_frpl = mean(frpl),
            sd_frpl = sd(frpl))
# ED data on CPSFC and Charlotte Lab downloaded from https://www.dpi.nc.gov/districts-schools/testing-and-school-accountability/school-accountability-and-reporting/accountability-data-sets-and-reports
memb_frpl <- read_excel("raw_data/nc_spg-report2019_final.xlsx", sheet = "School Performance Grades")
names(memb_frpl) <- tolower(memb_frpl[2,])
str(memb_frpl)
memb_frpl <- memb_frpl %>% 
  mutate(frpl = as.numeric(`percent eds`)*.01) %>% 
  select(`school name`,frpl) %>% 
  filter(`school name` == "Central Park School For Child"|
           `school name` == "Charlotte Lab School")
memb_frpl$district = c("Durham Public Schools","Charlotte-Mecklenburg Schools")
nc_stan_frpl <- merge(memb_frpl,summary_frpl,by="district")
nc_stan_frpl <- nc_stan_frpl %>% 
  mutate(comp_frpl = (frpl - m_frpl)/sd_frpl) %>% 
  select(district,`school name`,frpl,m_frpl,comp_frpl) 
write.csv(nc_stan_frpl,file = file.path("output_data/nc_stan_frpl.csv"),row.names =FALSE)

#----- ELs and SWD data ----
# source https://www.dpi.nc.gov/data-reports/school-report-cards/school-report-card-resources-researchers
nc_elswd <- read_excel("raw_data/nc_src_datasets-2020-06/rcd_charter.xlsx")
str(nc_elswd)
nc_elswd <- nc_elswd %>% 
  filter(year == "2019",
         str_detect(agency_code,"60M") == TRUE|
           str_detect(agency_code,"32K")==TRUE,
         subgroup == "SWD"|
           subgroup == "ELS"|
           subgroup == "EDS") 
nc_elswd$pct_enrolled <- nc_elswd$pct_enrolled *.01
memb_elswd <- nc_elswd
nc_elswd <- read_excel("raw_data/nc_src_datasets-2020-06/rcd_acc_elp.xlsx")
nc_elswd <- nc_elswd %>% 
  filter(year == "2019",
         str_detect(substr(as.character(agency_code),start = 1, stop = 3),"600")==TRUE|
           str_detect(substr(as.character(agency_code),start = 1, stop = 3),"320")==TRUE,
         str_detect(agency_code,"LEA")==FALSE,
         subgroup == "SWD"|
           subgroup == "ELS") %>% 
  mutate(pct = pct*.01)
nc_elswd$agency_code <- substr(as.character(nc_elswd$agency_code), start = 1, stop = 3)
nc_elswd$agency_code <- as.factor(nc_elswd$agency_code)
elswd <- nc_elswd %>% 
  group_by(agency_code, subgroup) %>% 
  summarize(m_pct = mean(pct),
            sd_pct = sd(pct),
            n= n())
levels(elswd$agency_code) <- c("32K000","60M000")
nc_stan_elswd <- merge(memb_elswd,elswd,by=c("agency_code","subgroup"))
nc_stan_elswd <- nc_stan_elswd %>% 
  mutate(comp_pct = (pct_enrolled - m_pct)/sd_pct) %>% 
  select(agency_code, subgroup, pct_enrolled, m_pct, comp_pct,sd_pct)
write.csv(nc_stan_elswd, file = file.path("output_data/nc_stan_elswd.csv"),row.names = FALSE)

#---- academic performance ----
# data pulled from SAS dashboard https://ncreportcards.ondemand.sas.com/SASVisualAnalyticsViewer/VisualAnalyticsViewer_guest.jsp?reportPath=/ReportCard/NC_SRC&reportName=NC+Report+Cards
memb_acad <- data.frame("school" = c("Central Park School for Child",
                                     "Charlotte Lab School"),
                        "district" = c("Durham County Schools",
                                       "Charlotte-Mecklenburg County Schools"),
                        "math_prof" = c(.625,.637),
                        "m_math" = c(.474,.636),
                       "ela_prof" = c(.682,.741),
                       "m_ela" = c(.477,.544))
write.csv(memb_acad, file = file.path('output_data/nc_acad.csv'),row.names = FALSE)
