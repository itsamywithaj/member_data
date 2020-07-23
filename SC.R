rm(list = ls()) # clear working environment
setwd("~/Downloads/States/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("ggplot2")
library(ggplot2)

#---- enrollment ----
# data imported from https://ed.sc.gov/data/other/student-counts/active-student-headcounts/2019-20-active-student-head-counts/45-day-school-headcount-by-gender-ethnicity-and-pupils-in-poverty-2019-20/

sc_enrl <- read_excel("raw_data/sc_School Headcount by Gender, Ethnicity and Pupils in Poverty d45_2019_20.xlsx")
names(sc_enrl) <- sc_enrl[5,]
names(sc_enrl) [c(5:15)] <- sc_enrl[6,c(5:15)]
sc_enrl <- sc_enrl[-c(1:6),-c(7,15)]
names(sc_enrl) <- tolower(names(sc_enrl))
names(sc_enrl)[c(4,7:8,10:14)] <- c("total","black","amind","hispanic","hawpi","multiple","white","frpm")
sc_stan <- sc_enrl %>% 
  mutate(district = as.factor(district),
         school = as.factor(school),
         total = as.numeric(total),
         c_amind = as.numeric(amind),
         c_asian = as.numeric(asian) + as.numeric(hawpi),
         c_black = as.numeric(black),
         c_hispanic = as.numeric(hispanic),
         c_white = as.numeric(white),
         c_multiple = as.numeric(multiple),
         c_poverty = as.numeric(frpm)) %>% 
  select(district, school, total, c_amind, c_asian, c_black, c_hispanic, c_white, c_multiple, c_poverty) %>%
  mutate(amind = c_amind/total,
         asian = c_asian/total,
         black = c_black/total,
         hispanic = c_hispanic/total,
         white = c_white/total,
         multiple = c_multiple/total,
         poverty = c_poverty/total) %>% 
  filter(district == "Greenville 01")
sc_stan <- sc_stan %>% 
  summarize(m_amind = mean(amind),
            sd_amind = sd(amind),
            m_asian = mean(asian),
            sd_asian = sd(asian),
            m_black = mean(black),
            sd_black = sd(black),
            m_hispanic = mean(hispanic),
            sd_hispanic = sd(hispanic),
            m_white = mean(white),
            sd_white = sd(white),
            m_multiple = mean(multiple),
            sd_multiple = sd(multiple),
            m_poverty = mean(poverty),
            sd_poverty = sd(poverty)) %>% 
  mutate(link = "A")
lead <- sc_enrl %>% 
  mutate(district = as.factor(district),
         school = as.factor(school),
         total = as.numeric(total),
         c_amind = as.numeric(amind),
         c_asian = as.numeric(asian) + as.numeric(hawpi),
         c_black = as.numeric(black),
         c_hispanic = as.numeric(hispanic),
         c_white = as.numeric(white),
         c_multiple = as.numeric(multiple),
         c_poverty = as.numeric(frpm)) %>% 
  select(district, school, total, c_amind, c_asian, c_black, c_hispanic, c_white, c_multiple, c_poverty) %>%
  mutate(amind = c_amind/total,
         asian = c_asian/total,
         black = c_black/total,
         hispanic = c_hispanic/total,
         white = c_white/total,
         multiple = c_multiple/total,
         poverty = c_poverty/total,
         link = "A") %>% 
  filter(school == "Lead Academy")
sc_stan_enrl <- merge(lead, sc_stan, by="link")
sc_stan_enrl<- sc_stan_enrl %>% 
  mutate(comp_amind = (amind - m_amind)/sd_amind,
         comp_asian = (asian - m_asian)/sd_asian,
         comp_black = (black - m_black)/sd_black,
         comp_hispanic = (hispanic - m_hispanic)/sd_hispanic,
         comp_white = (white - m_white)/sd_white,
         comp_multiple = (multiple - m_multiple)/sd_multiple,
         comp_poverty = (poverty - m_poverty)/sd_poverty) %>% 
  select(school, total, amind, m_amind, comp_amind, asian, m_asian, comp_asian,
         black, m_black, comp_black, hispanic, m_hispanic, comp_hispanic,
         white, m_white, comp_white, multiple, m_multiple, comp_multiple,
         poverty, m_poverty, comp_poverty)
write.csv(sc_stan_enrl, file = file.path('output_data/sc_stan_enrl.csv'),row.names = FALSE)
#--- disabilities ----
# https://ed.sc.gov/districts-schools/special-education-services/data-and-technology-d-t/data-collection-and-reporting/sc-data-collection-history/idea-child-count-data/2018-2019-child-count-data/

swd <- read.csv("raw_data/sc_CC_Counts_18_19.csv", header = FALSE)

#--- math and ela ----
# import data from https://ed.sc.gov/data/test-scores/state-assessments/sc-ready/2019/

test <- read_excel("raw_data/SCREADY 2018-2019 Press Release v2.xlsx", sheet = "School")
names(test) <- tolower(names(test))
test <- test %>% 
  select(recordtype, distcode, districtname, schoolname, demoid, testgrade, elan, elapct34,
         mathn,mathpct34) %>% 
  mutate(recordtype = as.factor(recordtype),
         distcode = as.factor(distcode),
         districtname = as.factor(districtname),
         testgrade = as.factor(testgrade),
         elapct34 = elapct34*.01,
         mathpct34 = mathpct34*.01,
         ela_n34 = elapct34 * elan,
         math_n34 = mathpct34 * mathn) %>% 
  filter(districtname == "Greenville 01"|
           schoolname == "Lead Academy",
         demoid == "01ALL") %>% 
  select(districtname,schoolname,elan,ela_n34,mathn,math_n34)
DT <- data.table(test)
test <- DT[, lapply(.SD,sum),by=list(districtname,schoolname)]
test_stan <- test %>% 
  mutate(p_ela = ela_n34/elan,
         p_math = math_n34/mathn) %>% 
  group_by(districtname) %>% 
  summarize(m_ela = mean(p_ela),
            sd_ela = sd(p_ela),
            m_math = mean(p_math),
            sd_math = sd(p_math),
            n= n())
lead_test <- test %>% 
  filter(schoolname == "Lead Academy") %>% 
  mutate(p_ela = ela_n34/elan,
         p_math = math_n34/mathn,
         districtname = 'Greenville 01')
lead_test <- merge(lead_test, test_stan, by="districtname")
lead_test <- lead_test %>% 
  mutate(comp_ela = (p_ela - m_ela)/sd_ela,
         comp_math = (p_math - m_math)/sd_math) %>% 
  select(schoolname, elan, p_ela, m_ela, comp_ela, mathn, p_math, m_math, comp_math)
write.csv(lead_test, file = file.path('output_data/sc_stan_test.csv'),row.names = FALSE)
