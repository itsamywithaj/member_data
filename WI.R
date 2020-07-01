rm(list = ls()) # clear working environment
setwd("~/Downloads/States/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("ggplot2")
library(ggplot2)

# data import from https://dpi.wi.gov/sites/default/files/imce/accountability/xls/2018-19_district_reportcard_data.xlsx
school_data <- read_excel("raw_data/WI_2018-19_school_reportcard_data.xlsx", sheet = "Data")
summary(school_data)
names(school_data)[c(4,5,11,13:22)] <- c("district","school","total_enr","amind","asian","black","hispanic","nhawpi","white","multiple",
                                      "swd","ed","lep")
school_data <- school_data %>% 
  mutate(asian = asian + nhawpi) %>% 
  select(district, school, total_enr, amind, asian, black, hispanic, white, multiple, swd, ed, lep) %>%
  filter(district == "Madison Metropolitan")
wi_enrl <- school_data %>% 
  summarize(mean_amind = mean(amind),
            sd_amind = sd(amind),
            mean_asian = mean(asian),
            sd_asian = sd(asian),
            mean_black = mean(black),
            sd_black = sd(black),
            mean_hispanic = mean(hispanic),
            sd_hispanic = sd(hispanic),
            mean_white = mean(white),
            sd_white = sd(white),
            mean_multiple = mean(multiple),
            sd_multiple = sd(multiple),
            mean_ed = mean(ed),
            sd_ed = sd(ed),
            mean_lep = mean(lep),
            sd_lep = sd(lep),
            mean_swd = mean(swd),
            sd_swd = sd(swd)) %>% 
  mutate(district = "Madison Metropolitan")
milestone <- data.frame(school = "Milestone Democratic",district = "Madison Metropolitan", 
                        total = 11, amind = 0,
                        asian = .14, black = .36, 
                        hispanic = .07, white = .14,
                        multiple = .29, ed = .91, swd = .29, lep = .07)
milestone <- merge(milestone, wi_enrl, by="district")
milestone <- milestone %>% 
  mutate(comp_amind = (amind - mean_amind)/sd_amind,
         comp_asian = (asian - mean_asian)/sd_asian,
         comp_black = (black - mean_black)/sd_black,
         comp_hispanic = (hispanic - mean_hispanic)/sd_hispanic,
         comp_white = (white - mean_white)/sd_white,
         comp_multiple = (multiple - mean_multiple)/sd_multiple,
         comp_ed = (ed - mean_ed)/sd_ed,
         comp_lep = (lep - mean_lep)/sd_lep,
         comp_swd = (swd - mean_swd)/sd_swd) %>% 
  select(district, school, total, amind, mean_amind, comp_amind,
         asian, mean_asian, comp_asian, black, mean_black, comp_black,
         hispanic, mean_hispanic, comp_hispanic, white, mean_white, comp_white,
         multiple, mean_multiple, comp_multiple, ed, mean_ed, comp_ed,
         lep, mean_lep, comp_lep, swd, mean_swd, comp_swd)
write.csv(milestone, file = file.path("output_data/wi_stan_enrl.csv"),row.names = FALSE)
