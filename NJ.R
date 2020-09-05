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

#----- enrollment -----
# data from https://www.nj.gov/education/data/enr/enr19/
# https://rc.doe.state.nj.us/ReportsDatabase.aspx
nj_enrl <- read_excel("raw_data/NJ_EnrollmentReport.xlsx")
head(nj_enrl)
nj_enrl[1,1]
names(nj_enrl) <- tolower(nj_enrl[2,])
nj_enrl <- nj_enrl[-c(1:2),]
str(nj_enrl)
nj_enrl <- nj_enrl %>% 
  filter(grade_level == "Total",
         prgcode == "55",
         district_name == "Hoboken City"&
           school_name!= "District Total"|
           school_name == "Hoboken Charter School") %>%
  mutate(total = as.numeric(row_total),
         c_amind = as.numeric(am_m)+as.numeric(am_f),
         c_asian = as.numeric(as_m)+as.numeric(as_f)+as.numeric(pi_m)+as.numeric(pi_f),
         c_black = as.numeric(bl_m)+as.numeric(bl_f),
         c_hispanic = as.numeric(hi_m)+as.numeric(hi_f),
         c_white = as.numeric(wh_m)+as.numeric(wh_f),
         c_multiple = as.numeric(mu_m)+as.numeric(mu_f),
         c_frpl = as.numeric(free_lunch)+as.numeric(reduced_price_lunch),
         c_el = as.numeric(english_learners),
         amind = c_amind/total,
         asian = c_asian/total,
         black = c_black/total,
         hispanic = c_hispanic/total,
         white = c_white/total,
         multiple = c_multiple/total,
         frpl = c_frpl/total,
         el = c_el/total)%>%
  select(district_name, school_name, total, amind, asian, black, hispanic, white, multiple,frpl, el)
enrl_comp <- nj_enrl %>% 
  group_by(district_name) %>% 
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
            m_frpl = mean(frpl),
            sd_frpl = sd(frpl),
            m_el = mean(el),
            sd_el = sd(el))
hoboken <- nj_enrl %>% 
  filter(school_name == "Hoboken Charter School") %>% 
  mutate(district_name = "Hoboken City")
nj_stan_enrl <- merge(hoboken, enrl_comp, by="district_name")
nj_stan_enrl <- nj_stan_enrl %>% 
  mutate(comp_amind = (amind - m_amind)/sd_amind,
         comp_asian = (asian - m_asian)/sd_asian,
         comp_black = (black - m_black)/sd_black,
         comp_hispanic = (hispanic - m_hispanic)/sd_hispanic,
         comp_white = (white - m_white)/sd_white,
         comp_multiple = (multiple - m_multiple)/sd_multiple,
         comp_frpl = (frpl - m_frpl)/sd_frpl,
         comp_el = (el - m_el)/sd_el) %>% 
  select(district_name, school_name, total, amind, m_amind, comp_amind,
         asian, m_asian, comp_asian, black, m_black, comp_black,
         hispanic, m_hispanic, comp_hispanic, white, m_white, comp_white,
         multiple, m_multiple, comp_multiple, frpl, m_frpl, comp_frpl,
         el, m_el, comp_el)
write.csv(nj_stan_enrl,file = file.path("output_data/nj_stan_enrl.csv"),row.names = FALSE)

# ---- SWD ----
swd <- read_excel("raw_data/NJ_PerformanceReports (1).xlsx", sheet = "EnrollmentTrendsByStudentGroup")
names(swd) <- tolower(names(swd))
str(swd)
swd <- swd %>%
  filter(districtname == "Hoboken Public School District"|
           schoolname == "Hoboken Charter School") %>% 
  mutate(c_swd = as.numeric(`students with disabilities`),
         total = nj_enrl$total,
         swd = c_swd/total) %>% 
  select(districtname,schoolname,total,swd)
nj_swd <- swd %>% 
  group_by(districtname) %>% 
  summarize(m_swd = mean(swd),
            sd_swd = sd(swd))
swd <- swd %>% 
  filter(schoolname == "Hoboken Charter School") %>% 
  mutate(comp_swd = (swd - nj_swd$m_swd[2])/nj_swd$sd_swd[2],
         m_swd = nj_swd$m_swd[2]) %>% 
  select(schoolname,total,swd, m_swd,comp_swd)
View(swd)
write.csv(swd,file = file.path("output_data/nj_stan_swd.csv"),row.names = FALSE)

#---- math ----
math <- read_excel("raw_data/NJ_PerformanceReports (1).xlsx", sheet = "MathParticipationPerform")
View(math)
levels(as.factor(math$StudentGroup))
names(math)<-tolower(names(math))
str(math)
math <- math %>% 
  filter(schoolname == "Hoboken Charter School"|
           districtname == "Hoboken Public School District",
         studentgroup == "Schoolwide") %>% 
  mutate(n_math = as.numeric(validscores),
         math_prof = as.numeric(schoolperformance)*.01) %>% 
  select(districtname, schoolname,n_math,math_prof) %>% 
  na.omit() 
nj_math <- math %>% 
  group_by(districtname) %>% 
  summarize(m_math = mean(math_prof),
            sd_math = sd(math_prof))
math[6,1] <- "Hoboken Public School District"
nj_math <- merge(math, nj_math, by="districtname")
nj_math <- nj_math %>% 
  mutate(comp_math = (math_prof - m_math)/sd_math) %>% 
  select(districtname,schoolname, n_math,math_prof, m_math, comp_math) %>% 
  filter(schoolname == "Hoboken Charter School")

# ---- ela ----
ela <- read_excel("raw_data/NJ_PerformanceReports (1).xlsx", sheet = "ELALiteracyParticipationPerform")
names(ela)<-tolower(names(ela))
str(ela)
ela <- ela %>% 
  filter(schoolname == "Hoboken Charter School"|
           districtname == "Hoboken Public School District",
         studentgroup == "Schoolwide") %>% 
  mutate(n_ela = as.numeric(validscores),
         ela_prof = as.numeric(schoolperformance)*.01) %>% 
  select(districtname, schoolname,n_ela,ela_prof) %>% 
  na.omit() 
nj_ela <- ela %>% 
  group_by(districtname) %>% 
  summarize(m_ela = mean(ela_prof),
            sd_ela = sd(ela_prof))
ela[7,1] <- "Hoboken Public School District"
nj_ela <- merge(ela, nj_ela, by="districtname")
nj_ela <- nj_ela %>% 
  mutate(comp_ela = (ela_prof - m_ela)/sd_ela) %>% 
  select(districtname,schoolname,n_ela,ela_prof, m_ela, comp_ela) %>% 
  filter(schoolname == "Hoboken Charter School")
nj_acad <- merge(nj_math, nj_ela, by=c("districtname","schoolname"))
write.csv(nj_acad,file=file.path("output_data/nj_stan_acad.csv"),row.names=FALSE)
