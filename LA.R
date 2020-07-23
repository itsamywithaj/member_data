rm(list = ls()) # clear working environment
setwd("~/Downloads/States/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)
getwd()
# ------ Enrollment ------
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
         `%lep`, `ed%`,sitecd)
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
         perc_lep = as.numeric(lep),
         perc_ed = as.numeric(ed)) %>% 
  select(district, sitename, total, perc_amind, perc_asian, perc_black, perc_hispanic, 
         perc_white, perc_multiple, perc_lep, perc_ed, sitecd)
la_memb <- la_enrl %>% 
  filter(sitename == "International High School of New Orleans"|
           sitename == "International School of Louisiana"|
           sitename == "Kenner Discovery Health Sciences Academy"|
           sitename == "Lycee Francais de la Nouvelle-Orleans"|
           sitename == "Morris Jeff Community School"|
           sitename == "Edward Hynes Charter School"|
           sitename == "Bricolage Academy"|
           sitename == "Homer A. Plessy Community School")
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
         perc_ed, mean_ed, comp_ed) %>% 
  arrange(sitename)
write.csv(la_memb, file = file.path("output_data/la_stan_enrl.csv"), row.names = FALSE)

# ------ Math and ELA ------
# import data from https://www.louisianabelieves.com/docs/default-source/test-results/2019-school-leap-2025-achievement-level-summary.xlsx?sfvrsn=5da19c1f_8
a3 <- read_excel("raw_data/LA_2019-school-leap-2025-achievement-level-summary.xlsx", sheet = "Grade 3")
a4 <- read_excel("raw_data/LA_2019-school-leap-2025-achievement-level-summary.xlsx", sheet = "Grade 4")
a5 <- read_excel("raw_data/LA_2019-school-leap-2025-achievement-level-summary.xlsx", sheet = "Grade 5")
a6 <- read_excel("raw_data/LA_2019-school-leap-2025-achievement-level-summary.xlsx", sheet = "Grade 6")
a7 <- read_excel("raw_data/LA_2019-school-leap-2025-achievement-level-summary.xlsx", sheet = "Grade 7")
a8 <- read_excel("raw_data/LA_2019-school-leap-2025-achievement-level-summary.xlsx", sheet = "Grade 8")
write.csv(a3,file = file.path("raw_data/la_grade3.csv"),row.names = FALSE)
write.csv(a4,file = file.path("raw_data/la_grade4.csv"),row.names = FALSE)
write.csv(a5,file = file.path("raw_data/la_grade5.csv"),row.names = FALSE)
write.csv(a6,file = file.path("raw_data/la_grade6.csv"),row.names = FALSE)
write.csv(a7,file = file.path("raw_data/la_grade7.csv"),row.names = FALSE)
write.csv(a8,file = file.path("raw_data/la_grade8.csv"),row.names = FALSE)

# using for loops to get all the tabs of separate grades into one combined data frame
leap_files <- grep("la_grade",  # the pattern we want to match
                  list.files("raw_data", full.names = TRUE), # a list of files in the raw data dir
                  # including the path to these files
                  value = TRUE) # when we match a pattern return the value of the matched pattern
for (i in seq_along(leap_files)) {
  print(paste0("** reading in ", leap_files[i], "**"))
  # Create the initial dataframe for the first file in the series
  if (i == 1) {
    leap_scores <- read.csv(leap_files[i], stringsAsFactors = FALSE)
    leap_scores <- leap_scores[-c(1,2,3,4),]
    colnames(leap_scores) = leap_scores[1,]
    names(leap_scores)[c(1:13)] <- c("code","school","n_tested",
                            "ela_perc_adv","ela_perc_master","ela_perc_basic","ela_perc_appbasic","ela_perc_un",
                            "math_perc_adv","math_perc_master","math_perc_basic","math_perc_appbasic","math_perc_un")
    names(leap_scores)
    leap_scores <- leap_scores[-c(1:3),-c(14:23)] # remove empty rows and unwanted columns for social studies and science
    leap_scores <- leap_scores %>% 
      select(code, school, n_tested, ela_perc_adv,ela_perc_master,ela_perc_basic,
             math_perc_adv, math_perc_master, math_perc_basic) %>% 
      mutate(school = as.factor(school),
             n_tested = as.numeric(gsub("[^0-9.-]","",as.character(n_tested))),
             ela_perc_adv = .01* as.numeric(gsub("[^0-9.-]","",as.character(ela_perc_adv))),
             ela_perc_master = .01 * as.numeric(gsub("[^0-9.-]","",as.character(ela_perc_master))),
             ela_perc_basic = .01 * as.numeric(gsub("[^0-9.-]","",as.character(ela_perc_basic))),
             math_perc_adv = .01* as.numeric(gsub("[^0-9.-]","",as.character(math_perc_adv))),
             math_perc_master = .01 * as.numeric(gsub("[^0-9.-]","",as.character(math_perc_master))),
             math_perc_basic = .01 * as.numeric(gsub("[^0-9.-]","",as.character(math_perc_basic))),
             ela_amb = ela_perc_adv + ela_perc_master + ela_perc_basic,
             math_amb = math_perc_adv + math_perc_master + math_perc_basic,
             grade = i + 2) %>% 
      select(code, school, n_tested, grade, ela_amb, ela_perc_adv, ela_perc_master, ela_perc_basic,
             math_amb, math_perc_adv, math_perc_master, math_perc_basic) %>% 
      na.omit()
  } else {
    # For each subsequent file create a temporary file
    tmp <- read.csv(leap_files[i], stringsAsFactors = FALSE, colClasses = "character")
      tmp <- tmp[-c(1,2,3,4),]
      colnames(tmp) = tmp[1,]
      names(tmp)[c(1:13)] <- c("code","school","n_tested",
                                       "ela_perc_adv","ela_perc_master","ela_perc_basic","ela_perc_appbasic","ela_perc_un",
                                       "math_perc_adv","math_perc_master","math_perc_basic","math_perc_appbasic","math_perc_un")
      names(tmp)
      tmp <- tmp[-c(1:3),-c(14:23)] # remove empty rows and unwanted columns for social studies and science
      tmp <- tmp %>% 
        select(code, school, n_tested, ela_perc_adv,ela_perc_master,ela_perc_basic,
               math_perc_adv, math_perc_master, math_perc_basic) %>% 
        mutate(school = as.factor(school),
               n_tested = as.numeric(gsub("[^0-9.-]","",as.character(n_tested))),
               ela_perc_adv = .01* as.numeric(gsub("[^0-9.-]","",as.character(ela_perc_adv))),
               ela_perc_master = .01 * as.numeric(gsub("[^0-9.-]","",as.character(ela_perc_master))),
               ela_perc_basic = .01 * as.numeric(gsub("[^0-9.-]","",as.character(ela_perc_basic))),
               math_perc_adv = .01* as.numeric(gsub("[^0-9.-]","",as.character(math_perc_adv))),
               math_perc_master = .01 * as.numeric(gsub("[^0-9.-]","",as.character(math_perc_master))),
               math_perc_basic = .01 * as.numeric(gsub("[^0-9.-]","",as.character(math_perc_basic))),
               ela_amb = ela_perc_adv + ela_perc_master + ela_perc_basic,
               math_amb = math_perc_adv + math_perc_master + math_perc_basic,
               grade = i + 2) %>% 
        select(code, school, n_tested, grade, ela_amb, ela_perc_adv, ela_perc_master, ela_perc_basic,
               math_amb, math_perc_adv, math_perc_master, math_perc_basic) %>% 
        na.omit()
    # Combine the temporary file and the master file
    leap_scores <- rbind(leap_scores, tmp)
    rm(tmp) # not necessary, but removes the temporary file
  }
  # Report out how much data we read in
  print(paste0("** total rows read in = ", nrow(leap_scores), "**"))
}

names(leap_scores)[1] <- "sitecd"
list_district <- la_enrl %>% 
  select(sitecd, sitename, district)
leap_scores <- merge(list_district, leap_scores, by="sitecd")

leap_scores <- leap_scores %>% 
  filter(district == "Jefferson Parish"|
           district == "Orleans Parish"|
           district == "Type 2 Charters")
str(leap_scores)
leap_scores$grade <- as.factor(leap_scores$grade)
levels(leap_scores$grade) <- c("gr3","gr4","gr5","gr6","gr7","gr8")
leap_scores <- leap_scores %>% 
  distinct(sitecd, sitename, school, district, n_tested, .keep_all = TRUE) %>% 
  spread(key = grade, # change grades column to separate columns
         value = n_tested, # number tested for each grade
         fill = 0) %>% 
  mutate(all_grades = gr3 + gr4 + gr5 + gr6 + gr7 + gr8, # counts total students in that row
         n_ela_amb = (gr3 * ela_amb) + (gr4 * ela_amb) + (gr5*ela_amb)+
           (gr6*ela_amb)+ (gr7*ela_amb) + (gr8 * ela_amb),
         n_math_amb = (gr3 * math_amb) + (gr4 * math_amb) + (gr5*math_amb)+
           (gr6*math_amb)+ (gr7*math_amb) + (gr8 * math_amb)) %>%  # counts total students advanced, mastery, or basic
  select(sitename, district, n_ela_amb, n_math_amb, all_grades, gr3, gr4, gr5, gr6, gr7, gr8) 

DT <- data.table(leap_scores)
leap_scores <- DT[, lapply(.SD, sum), by=list(sitename, district)] # Summing the rows for all grades
leap_scores <- leap_scores %>% 
  mutate(perc_math = n_math_amb/all_grades,
         perc_ela = n_ela_amb/all_grades) %>% 
  select(sitename, district, all_grades, perc_math, perc_ela) 
stan_acad <- leap_scores %>% 
  group_by(district) %>% 
  summarize(n_schools = n(),
            mean_ela = mean(perc_ela),
            sd_ela = sd(perc_ela),
            mean_math = mean(perc_math),
            sd_math = sd(perc_math))

la_stan_acad <- merge(leap_scores, stan_acad, by="district")
la_stan_acad <- la_stan_acad %>% 
  filter(sitename == "International School of Louisiana"|
           sitename == "Kenner Discovery Health Sciences Academy"|
           sitename == "Lycee Francais de la Nouvelle-Orleans"|
           sitename == "Morris Jeff Community School"|
           sitename == "Edward Hynes Charter School"|
           sitename == "Bricolage Academy"|
           sitename == "Homer A. Plessy Community School") %>% 
  mutate(comp_ela = (perc_ela - mean_ela)/sd_ela,
         comp_math = (perc_math - mean_math)/sd_math) %>%
  select(district, sitename, all_grades,perc_math, mean_math, comp_math, perc_ela, mean_ela, comp_ela) %>% 
  arrange(sitename)

write.csv(la_stan_acad, file = file.path("output_data/la_stan_acad.csv"),row.names = FALSE)

la_hs <- read_excel("raw_data/LA_2019-school-leap-2025-achievement-level-summary.xlsx", sheet = "High School")
# data imported from https://www.louisianabelieves.com/docs/default-source/test-results/2019-school-leap-2025-achievement-level-summary.xlsx?sfvrsn=5da19c1f_8
View(la_hs)
la_hs <- la_hs[-c(1,2,3,4),]
colnames(la_hs) = la_hs[1, ]
names(la_hs)[c(1:26)] <- c("code","school","n_tested_eng1",
                        "eng1_perc_adv","eng1_perc_master","eng1_perc_basic","eng1_perc_appbasic","eng1_perc_un", 
                        "n_tested_eng2", "eng2_perc_adv","eng2_perc_master","eng2_perc_basic","eng2_perc_appbasic","eng2_perc_un",
                        "n_tested_math1","math1_perc_adv","math1_perc_master","math1_perc_basic","math1_perc_appbasic","math1_perc_un",
                        "n_tested_math2","math2_perc_adv","math2_perc_master","math2_perc_basic","math2_perc_appbasic","math2_perc_un")
names(la_hs)
la_hs <- la_hs[-c(1:3),-c(27:38)] # remove empty rows and unwanted columns for US history and bio
str(la_hs)
la_hs <- la_hs %>% 
  select(code, school, n_tested_eng1, eng1_perc_adv,eng1_perc_master,eng1_perc_basic,
         n_tested_eng2, eng2_perc_adv,eng2_perc_master,eng2_perc_basic,
         n_tested_math1, math1_perc_adv,math1_perc_master,math1_perc_basic,
         n_tested_math2, math2_perc_adv,math2_perc_master,math2_perc_basic) %>% 
  mutate(school = as.factor(school),
         n_tested_eng1 = as.numeric(gsub("[^0-9.-]","",as.character(n_tested_eng1))),
         n_tested_eng2 = as.numeric(gsub("[^0-9.-]","",as.character(n_tested_eng2))),
         n_tested_math1 = as.numeric(gsub("[^0-9.-]","",as.character(n_tested_math1))),
         n_tested_math2 = as.numeric(gsub("[^0-9.-]","",as.character(n_tested_math2))),
         eng1_perc_adv = .01* as.numeric(gsub("[^0-9.-]","",as.character(eng1_perc_adv))),
         eng1_perc_master = .01 * as.numeric(gsub("[^0-9.-]","",as.character(eng1_perc_master))),
         eng1_perc_basic = .01 * as.numeric(gsub("[^0-9.-]","",as.character(eng1_perc_basic))),
         eng2_perc_adv = .01* as.numeric(gsub("[^0-9.-]","",as.character(eng2_perc_adv))),
         eng2_perc_master = .01 * as.numeric(gsub("[^0-9.-]","",as.character(eng2_perc_master))),
         eng2_perc_basic = .01 * as.numeric(gsub("[^0-9.-]","",as.character(eng2_perc_basic))),
         eng1_amb = eng1_perc_adv + eng1_perc_master + eng1_perc_basic,
         eng2_amb = eng2_perc_adv + eng2_perc_master + eng2_perc_basic,
         math1_perc_adv = .01* as.numeric(gsub("[^0-9.-]","",as.character(math1_perc_adv))),
         math1_perc_master = .01 * as.numeric(gsub("[^0-9.-]","",as.character(math1_perc_master))),
         math1_perc_basic = .01 * as.numeric(gsub("[^0-9.-]","",as.character(math1_perc_basic))),
         math2_perc_adv = .01* as.numeric(gsub("[^0-9.-]","",as.character(math2_perc_adv))),
         math2_perc_master = .01 * as.numeric(gsub("[^0-9.-]","",as.character(math2_perc_master))),
         math2_perc_basic = .01 * as.numeric(gsub("[^0-9.-]","",as.character(math2_perc_basic))),
         math1_amb = math1_perc_adv + math1_perc_master + math1_perc_basic,
         math2_amb = math2_perc_adv + math2_perc_master + math2_perc_basic,
         n_math = n_tested_math1 + n_tested_math2,
         n_ela = n_tested_eng1 + n_tested_eng2,
         n_ela_amb = (eng1_amb*n_tested_eng1) + (eng2_amb*n_tested_eng2),
         n_math_amb = (math1_amb*n_tested_math1) + (math2_amb*n_tested_math2),
         ela_amb = n_ela_amb/n_ela,
         math_amb = n_math_amb/n_math,
         sitecd = code)%>% 
  select(sitecd, school, n_math, n_math_amb, math_amb, n_ela, n_ela_amb, ela_amb) %>% 
  na.omit()
list_district <- la_enrl %>% 
  select(sitecd, sitename, district) %>% 
  filter(district == "Type 2 Charters")
la_hs <- merge(la_hs, list_district, by="sitecd")
stan_hs <- la_hs %>% 
  summarize(n_schools = n(),
            mean_math = mean(math_amb),
            sd_math = sd(math_amb),
            mean_ela = mean(ela_amb),
            sd_ela = sd(ela_amb))
la_hs <- la_hs %>% 
  filter(school == "INTERNATIONAL HIGH SCHOOL OF NEW ORLEANS") %>% 
  mutate(mean_math = stan_hs$mean_math,
         mean_ela = stan_hs$mean_ela,
         comp_math = (math_amb - mean_math)/stan_hs$sd_math,
         comp_ela = (ela_amb - mean_ela)/stan_hs$sd_ela) %>% 
  select(sitename, n_math, math_amb, mean_math, comp_math,
         n_ela,ela_amb, mean_ela, comp_ela)
write.csv(la_hs, file = file.path("output_data/la_stan_hs.csv"),row.names = FALSE)
