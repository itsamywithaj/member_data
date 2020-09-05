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

# data imported from https://www.doe.in.gov/accountability/find-school-and-corporation-data-reports

in_enrl <- read_excel("raw_data/IN_school-enrollment-ethnicity-and-free-reduced-price-meal-status-2006-20.xlsx", sheet = "2020")
View(in_enrl)
names(in_enrl) <- tolower(names(in_enrl))
str(in_enrl)
in_enrl <- in_enrl %>% 
  mutate_if(is_numeric, replace_na, replace = 0) %>% 
  mutate(total = `total enrollment`,
         amind = `american indian`/total,
         asian = (asian + `native hawaiian or other pacific islander`)/total,
         black = black/total,
         hispanic = hispanic/total,
         white = white/total,
         multiple = multiracial/total,
         frpl = `free/reduced price meals`/total,
         district = as.factor(`corp name`),
         school = as.factor(`schl name`)) %>% 
  select(district, school, total,amind,asian,black,hispanic,white, multiple, frpl) %>% 
  filter(school == "Herron High School"|
           school == "Riverside High School"|
           district == "Indianapolis Public Schools")
in_m <- in_enrl %>% 
  group_by(district) %>% 
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
            n=n())
in_enrl <- in_enrl %>% 
  filter(school == "Herron High School"|
           school == "Riverside High School") %>% 
  mutate(district = "Indianapolis Public Schools")
in_enrl <- merge(in_enrl, in_m,by="district")
in_enrl <- in_enrl %>% 
  mutate(comp_amind = (amind - m_amind)/sd_amind,
         comp_asian = (asian - m_asian)/sd_asian,
         comp_black = (black - m_black)/sd_black,
         comp_hispanic = (hispanic - m_hispanic)/sd_hispanic,
         comp_white = (white - m_white)/sd_white,
         comp_multiple = (multiple - m_multiple)/sd_multiple,
         comp_frpl = (frpl - m_frpl)/sd_frpl) %>% 
  select(district, school, total, amind, m_amind, comp_amind,
         asian, m_asian, comp_asian, black, m_black, comp_black,
         hispanic, m_hispanic, comp_hispanic, white, m_white, comp_white,
         multiple, m_multiple, comp_multiple, frpl, m_frpl, comp_frpl)
write.csv(in_enrl,file = file.path("output_data/in_stan_enrl.csv"),row.names = FALSE)

# --- EL and SWD ----
in_elswd <- read_excel("raw_data/IN_school-enrollment-ell-special-education-2006-19.xlsx", sheet = "2019")
View(in_elswd)
str(in_elswd)
in_elswd <- in_elswd %>% 
  mutate(district = as.factor(`Corp Name`),
         school = as.factor(`School Name`),
         ell = `ELL %`,
         sped = `Special Education %`) %>% 
  select(district,school,ell,sped) %>%
  filter(district == "Indianapolis Public Schools"|
           school == "Herron High School"|
           school == "Riverside High School")
in_m <- in_elswd %>% 
  group_by(district) %>% 
  summarize(m_ell = mean(ell),
            sd_ell = sd(ell),
            m_sped = mean(sped),
            sd_sped = sd(sped),
            n= n())
in_elswd <- in_elswd %>% 
  filter(school == "Herron High School"|
           school == "Riverside High School") %>% 
  mutate(district = "Indianapolis Public Schools")
in_elswd <- merge(in_elswd, in_m, by="district")
in_elswd <- in_elswd %>% 
  mutate(comp_ell = (ell - m_ell)/sd_ell,
         comp_sped = (sped - m_sped)/sd_sped) %>% 
  select(district,school,ell,m_ell,comp_ell,sped, m_sped, comp_sped)
write.csv(in_elswd,file = file.path("output_data/in_stan_elswd.csv"),row.names = FALSE)

#------ academic performance -----
in_acad <- read_excel("raw_data/IN_istep-2019-grade10-final-school.xlsx",)
names(in_acad) <- tolower(in_acad[1,])
in_acad <- in_acad[-1,]
names(in_acad)
str(in_acad)
in_acad <- in_acad %>% 
  mutate(n_ela = as.numeric(`ela\r\ntest n`),
         n_math = as.numeric(`math\r\ntest n`),
         ela_pass = as.numeric(`ela\r\npass n`),
         math_pass = as.numeric(`math\r\npass n`),
         school = as.factor(`school name`),
         district = as.factor(`corp name`),
         ela_p = ela_pass/n_ela,
         math_p = math_pass/n_math) %>% 
  select(district, school, n_math, math_p,math_pass, n_ela, ela_p,ela_pass) %>%na.omit()
in_m <- in_acad %>%
  summarize(m_math = mean(math_p),
            sd_math = sd(math_p),
            m_ela = mean(ela_p),
            sd_ela = sd(ela_p))
in_acad <- in_acad %>% 
  filter(school == "Herron High School"|
           school == "Riverside High School")
in_acad <- merge(in_acad,in_m)
in_acad <- in_acad %>% 
  mutate(comp_ela = (ela_p - m_ela)/sd_ela,
         comp_math = (math_p-m_math)/sd_math) %>% 
  select(district, school, n_math, math_p, m_math, comp_math,
         n_ela, ela_p, m_ela, comp_ela)
write.csv(in_acad,file = file.path("output_data/in_stan_acad.csv"),row.names = FALSE)
