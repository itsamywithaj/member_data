rm(list = ls()) # clear working environment
setwd("~/Downloads/States/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("ggplot2")
library(ggplot2)

# import data from https://www.louisianabelieves.com/docs/default-source/data-management/feb-2020-multi-stats-(total-by-site-and-school-system).xlsx?sfvrsn=6ec89b1f_4

la_enrl <- read_excel("raw_data/LA_feb-2020-multi-stats-(total-by-site-and-school-system).xlsx",sheet = "Total by Site")
la_enrl <- la_enrl[-c(1,2,3,4),] # remove rows with no data
colnames(la_enrl) = la_enrl[1, ] # data from header row to column name
la_enrl <- la_enrl[-1,]
la_enrl <- la_enrl[,-36]
names(la_enrl)<- tolower(names(la_enrl))
summary(la_enrl)
la_enrl <- la_enrl %>% 
  select(`school system name`,sitename, `total students`,amind, asian, black, hispanic, hawpi, white, multiple,
         `%lep`, `ed%`)
names(la_enrl)[c(1,3,11,12)] <- c("district","total","lep","ed")
la_enrl <- la_enrl %>% 
  mutate(total = as.numeric(total),
         perc_amind = as.numeric(amind)/total,
         asian = as.numeric(asian) + as.numeric(hawpi),
         perc_asian = as.numeric(asian)/total,
         perc_black = as.numeric(black)/total,
         perc_hispanic = as.numeric(hispanic)/total,
         perc_white = as.numeric(white)/total,
         perc_multiple = as.numeric(multiple)/total,
         perc_lep = as.numeric(lep)*100,
         perc_ed = as.numeric(ed)*100) %>% 
  select(district, sitename, total, perc_amind, perc_asian, perc_black, perc_hispanic, 
         perc_white, perc_multiple, perc_lep, perc_ed)
la_memb <- la_enrl %>% 
  filter(sitename == "International High School of New Orleans"|
           sitename == "International School of Louisiana"|
           sitename == "Kenner Discovery Health Sciences Academy"|
           sitename == "Lycee Francais de la Nouvelle-Orleans"|
           sitename == "Morris Jeff Community School"|
           sitename == "Edward Hynes Charter School")
la_stan_enrl <- la_enrl %>% 
  filter(district == "Jefferson Parish"|
           district == "Orleans Parish"|
           district == "Type 2 Charters") %>%
  group_by(district) %>% 
  summarize(n_schools = n(),
            mean_amind = mean(perc_amind),
            sd_amind = sd(perc_amind),
            mean_asian = sd(perc_asian),
            sd_asian = sd(perc_asian),
            mean_black = mean(perc_black),
            sd_black = sd(perc_black),
            mean_hispanic = mean(perc_hispanic),
            sd_hispanic = sd(perc_hispanic),
            mean_white = mean(perc_white),
            sd_white = sd(perc_white),
            mean_multiple = mean(perc_multiple),
            sd_multiple = sd(perc_multiple),
            mean_lep = mean(perc_lep),
            sd_lep = sd(perc_lep),
            mean_ed = mean(perc_ed),
            sd_ed = sd(perc_ed))
la_memb <- merge(la_memb, la_stan_enrl, by="district")
la_memb <- la_memb %>% 
  mutate(comp_amind = (perc_amind - mean_amind)/sd_amind,
         comp_asian = (perc_asian - mean_asian)/sd_asian,
         comp_black = (perc_black - mean_black)/sd_black,
         comp_hispanic = (perc_hispanic - mean_hispanic)/sd_hispanic,
         comp_white = (perc_white - mean_white)/sd_white,
         comp_multiple = (perc_multiple - mean_multiple)/sd_multiple,
         comp_lep = (perc_lep - mean_lep)/sd_lep,
         comp_ed = (perc_ed - mean_ed)/sd_ed) %>% 
  select(district, sitename, total, perc_amind, mean_amind, comp_amind, 
         perc_asian, mean_asian, comp_asian, perc_black, mean_black, comp_black,
         perc_hispanic, mean_hispanic, comp_hispanic, perc_white, mean_white, comp_white,
         perc_multiple, mean_multiple, comp_multiple, perc_lep, mean_lep, comp_lep,
         perc_ed, mean_ed, comp_ed)
write.csv(la_memb, file = file.path("output_data/la_stan_enrl.csv"), row.names = FALSE)
