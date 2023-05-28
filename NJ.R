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

# ----- ENROLLMENT BY RACE AND SUBGROUP --------
# https://www.nj.gov/education/doedata/enr/index.shtml
# Downloaded from zip file https://www.nj.gov/education/doedata/enr/enr23/enrollment_2223.zip

enrl <- read_excel('raw data/NJ_enrollment_2223.xlsx', sheet = "School")
documentation <- names(enrl)[1]
documentation
names(enrl) <- tolower(enrl[2,])
enrl <- enrl[-(1:2),]
str(enrl)
enrl <- enrl %>% 
  mutate(county = as.factor(`county name`),
         district = as.factor(`district name`),
         school = `school name`,
         total = as.numeric(`total enrollment`),
         amind = as.numeric(gsub("[^0-9.-]", "", `native american`)),
         asian = as.numeric(gsub("[^0-9.-]","",asian)) +
           as.numeric(gsub("[^0-9.-]","",`hawaiian native`)),
         black = as.numeric(gsub("[^0-9.-]","",black)),
         hisp = as.numeric(gsub("[^0-9.-]","", hispanic)),
         white = as.numeric(white),
         mult = as.numeric(gsub("[^0-9.-]","", `two or more races`)),
         frpl = (as.numeric(gsub("[^0-9.-]","",`%free lunch`)) +
                   as.numeric(gsub("[^0-9.-]","",`%reduced lunch`))) *.01 * total,
         ell = as.numeric(gsub("[^0-9.-]","",`%english learners`)) * .01 * total) %>% 
  select(county:amind, asian, black,hisp, white, mult:ell)

levels(enrl$county)
levels(enrl$district)

memb_enrl <- enrl %>%
  filter(school == "Hoboken Charter School"|
           school == "Learning Community Charter School"|
           school == "Bergen Arts And Sciences Charter School"| # iLearn
           school == "Passaic Arts And Science Charter School"| # iLearn
           school == "Paterson Arts And Science Charter School"| # iLearn
           school == "Hudson Arts And Science Charter School") %>% # iLearn 
  arrange(school)
# Rename to match comparison counties and districts
memb_enrl$district <- c(levels(enrl$district)[197],levels(enrl$district)[254],levels(enrl$district)[281],
                        levels(enrl$district)[278],levels(enrl$district)[450],levels(enrl$district)[456])

memb_enrl$county <- c(levels(enrl$county)[2],levels(enrl$county)[10],levels(enrl$county)[10],
                      levels(enrl$county)[10],levels(enrl$county)[17],levels(enrl$county)[17])

enrl <- enrl %>% 
  filter(school != "Hoboken Charter School",
         school != "Learning Community Charter School",
         school != "Bergen Arts And Sciences Charter School",
         school != "Passaic Arts And Science Charter School",
         school != "Paterson Arts And Science Charter School",
         school != "Hudson Arts And Science Charter School") %>%
  rbind(memb_enrl)

# district level local segregation -------
dist_ls <- enrl %>% 
  filter(district == levels(enrl$district)[254]) %>% 
  select(school,amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>% 
  filter(school == "Hoboken Charter School") %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)

dist_ls <- enrl %>% 
  filter(district == levels(enrl$district)[278]) %>% 
  select(school,amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>% 
  filter(school == "Learning Community Charter School") %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist) %>% 
  rbind(dist_ls)

dist_ls <- enrl %>% 
  filter(district == levels(enrl$district)[197]) %>% 
  select(school,amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>% 
  filter(school == "Bergen Arts And Sciences Charter School") %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist) %>% 
  rbind(dist_ls)

dist_ls <- enrl %>% 
  filter(district == levels(enrl$district)[450]) %>% 
  select(school,amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>% 
  filter(school == "Passaic Arts And Science Charter School") %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist) %>% 
  rbind(dist_ls)

dist_ls <- enrl %>% 
  filter(district == levels(enrl$district)[456]) %>% 
  select(school,amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>% 
  filter(school == "Paterson Arts And Science Charter School") %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist) %>% 
  rbind(dist_ls)

dist_ls <- enrl %>% 
  filter(district == levels(enrl$district)[281]) %>% 
  select(school,amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>% 
  filter(school == "Hudson Arts And Science Charter School") %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist) %>% 
  rbind(dist_ls)

memb_enrl <- memb_enrl %>% 
  merge(dist_ls, by = "school")

dist_agg <- enrl %>% 
  filter(district == levels(enrl$district)[197]|
           district == levels(enrl$district)[254]|
           district == levels(enrl$district)[281]|
           district == levels(enrl$district)[278]|
           district == levels(enrl$district)[450]|
           district == levels(enrl$district)[456]) %>% 
  group_by(district) %>% 
  summarize(dist_total = sum(total),
            dist_amind = sum(amind)/dist_total,
            dist_asian = sum(asian)/dist_total,
            dist_black = sum(black)/dist_total,
            dist_hisp = sum(hisp)/dist_total,
            dist_white = sum(white)/dist_total,
            dist_mult = sum(mult)/dist_total,
            dist_frpl = sum(frpl)/dist_total,
            dist_ell = sum(ell)/dist_total)
memb_enrl <- memb_enrl %>% 
  merge(dist_agg)

# county level local segregation ------
cty_ls <- enrl %>% 
  filter(county == levels(enrl$county)[2]) %>% 
  select(school,amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>% 
  filter(school == "Bergen Arts And Sciences Charter School") %>% 
  mutate(ls_cty = ls) %>% 
  select(school, ls_cty)

cty_ls <- enrl %>% 
  filter(county == levels(enrl$county)[10]) %>% 
  select(school,amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>% 
  filter(school == "Hudson Arts And Science Charter School"|
           school == "Learning Community Charter School"|
           school == "Hoboken Charter School") %>% 
  mutate(ls_cty = ls) %>% 
  select(school, ls_cty) %>% 
  rbind(cty_ls)

cty_ls <- enrl %>% 
  filter(county == levels(enrl$county)[17]) %>% 
  select(school,amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>% 
  filter(school == "Passaic Arts And Science Charter School"|
           school == "Paterson Arts And Science Charter School") %>% 
  mutate(ls_cty = ls) %>% 
  select(school, ls_cty) %>% 
  rbind(cty_ls)

memb_enrl <- memb_enrl %>% 
  merge(cty_ls)

cty_agg <- enrl %>% 
  filter(county == levels(enrl$county)[2]|
           county == levels(enrl$county)[10]|
           county == levels(enrl$county)[17]) %>% 
  group_by(county) %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(amind)/cty_total,
            cty_asian = sum(asian)/cty_total,
            cty_black = sum(black)/cty_total,
            cty_hisp = sum(hisp)/cty_total,
            cty_white = sum(white)/cty_total,
            cty_mult = sum(mult)/cty_total,
            cty_frpl = sum(frpl)/cty_total,
            cty_ell = sum(ell)/cty_total)
memb_enrl <- memb_enrl %>% 
  merge(cty_agg) %>% 
  mutate(amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         white = white/total,
         mult = mult/total,
         frpl = frpl/total,
         ell = ell/total)

# ---- SPECIAL EDUCATION? ----- 
# https://www.nj.gov/education/specialed/monitor/ideapublicdata/index.shtml
# Only 2021 available https://www.nj.gov/education/specialed/monitor/ideapublicdata/docs/2022%20data/Lea_Classification_Pub.xlsx

swd <- read_excel('raw data/NJ_Lea_Classification_Pub.xlsx')
swd_doc <- swd[1,1]
swd_doc %>% names()
names(swd) <- tolower(swd[5,])
swd <- swd[-(1:5),]
str(swd)

swd <- swd %>% 
  mutate(county = `county name`,
         district = `district name`,
         total = as.numeric(`general ed. enrollment`),
         swd = as.numeric(`special ed. enrollment`)) %>% 
  select(county, district, total, swd)

memb_swd <- swd %>% 
  filter(district == "Hoboken Charter School"|
           district == "Bergen Arts and Science Charter School"|
           district == "Hudson Arts and Science Charter School"|
           district == "Passaic Arts and Science Charter School"|
           district == "Paterson Arts and Science Charter School"|
           district == "The Learning Community Charter School") %>% 
  mutate(school = district,
         swd = swd/total) %>% 
  select(school, swd) %>% 
  arrange(school)

memb_swd$school <- sort(memb_enrl$school)[c(1:3,5,6,4)] #match naming conventions (lowercase 'and', no 'The')
memb_enrl <- merge(memb_enrl,memb_swd)

swd_agg <- swd %>% 
  filter(county == levels(enrl$county)[2]|
           county == levels(enrl$county)[10]|
           county == levels(enrl$county)[17]) %>% 
  group_by(county) %>% 
  summarize(total = sum(total),
            swd = sum(swd)) %>% 
  mutate(cty_swd = swd/total) %>% 
  select(county, cty_swd)
memb_enrl <- merge(memb_enrl,swd_agg)

swd_agg <- swd %>% 
  filter(district == levels(enrl$district)[197]|
           district == levels(enrl$district)[254]|
           district == levels(enrl$district)[281]|
           district == levels(enrl$district)[278]|
           district == levels(enrl$district)[450]|
           district == levels(enrl$district)[456]) %>% 
  mutate(dist_swd = swd/total) %>% 
  select(district, dist_swd)

memb_enrl <- memb_enrl %>% merge(swd_agg)

# Prep for export -----
memb_enrl <- memb_enrl %>% 
  select(school:ell,swd, ls_dist, ls_cty, district, dist_total:dist_ell,dist_swd,
         county, cty_total:cty_ell,cty_swd)
write.csv(memb_enrl,file = file.path('output data/nj_enrl.csv'),row.names = FALSE)

# ---- ASSESSMENT DATA: MATH ------
# Documentation on performance levels https://nj.mypearsonsupport.com/resources/reporting/NJSLA_Parent_Score_Interpretation_Guide_Spring2022.pdf
# Level 4 and 5 = on track for the next grade level
# data on scores downloaded from https://www.nj.gov/education/assessment/results/reports/

math <- read_excel('raw data/NJ_ALG01 NJSLA DATA 2021-22.xlsx')
View(math)
names(math) <- tolower(math[2,])
math <- math[-c(1:2),]
str(math)
memb_enrl$school
levels(as.factor(math$`subgroup type`))
memb_math <- math %>% 
  mutate(district = `district name`,
         school = `school name`,
         n_test = as.numeric(`valid scores`),
         p_prof = as.numeric(`l4 percent`) + as.numeric(`l5 percent`)) %>% 
  filter(district =="Garfield Public School District"|
           district =="Hoboken Public School District"|
           district == "Jersey City Public Schools"|
           district == "Kearny"|
           district == "Passaic City School District"|
           district == "Paterson Public School District"|
           district == "Bergen Arts And Science Charter School"|
           district == "Hoboken Charter School"|
           district == "The Learning Community Charter School"|
           district == "Hudson Arts And Science Charter School"|
           district == "Passaic Arts And Science Charter School"|
           district == "Paterson Arts And Science Charter School",
         school == "District Total",
         subgroup == "Total") %>% 
  select(district, n_test,p_prof) %>% 
  mutate(grade = "Algebra I",
         n_prof = p_prof* .01 * n_test)

# WHY I OMITTED ALGEBRA II and GEOMETRY: Limited data available. Many members/districts had no students with valid tests

# 3rd grade math -----
math <- read_excel('raw data/NJ_MAT03 NJSLA DATA 2021-22.xlsx')
View(math)
names(math) <- tolower(math[2,])
math <- math[-c(1:2),]
str(math)
memb_enrl$school
levels(as.factor(math$`subgroup type`))
memb_math <- math %>% 
  mutate(district = `district name`,
         school = `school name`,
         n_test = as.numeric(`valid scores`),
         p_prof = as.numeric(`l4 percent`) + as.numeric(`l5 percent`)) %>% 
  filter(district =="Garfield Public School District"|
           district =="Hoboken Public School District"|
           district == "Jersey City Public Schools"|
           district == "Kearny"|
           district == "Passaic City School District"|
           district == "Paterson Public School District"|
           district == "Bergen Arts And Science Charter School"|
           district == "Hoboken Charter School"|
           district == "The Learning Community Charter School"|
           district == "Hudson Arts And Science Charter School"|
           district == "Passaic Arts And Science Charter School"|
           district == "Paterson Arts And Science Charter School",
         school == "District Total",
         subgroup == "Total") %>% 
  select(district, n_test,p_prof) %>% 
  mutate(grade = "Grade 3",
         n_prof = p_prof* .01 * n_test) %>% 
  rbind(memb_math)

# 4th grade math -----
math <- read_excel('raw data/NJ_MAT04 NJSLA DATA 2021-22.xlsx')
names(math) <- tolower(math[2,])
math <- math[-c(1:2),]
memb_math <- math %>% 
  mutate(district = `district name`,
         school = `school name`,
         n_test = as.numeric(`valid scores`),
         p_prof = as.numeric(`l4 percent`) + as.numeric(`l5 percent`)) %>% 
  filter(district =="Garfield Public School District"|
           district =="Hoboken Public School District"|
           district == "Jersey City Public Schools"|
           district == "Kearny"|
           district == "Passaic City School District"|
           district == "Paterson Public School District"|
           district == "Bergen Arts And Science Charter School"|
           district == "Hoboken Charter School"|
           district == "The Learning Community Charter School"|
           district == "Hudson Arts And Science Charter School"|
           district == "Passaic Arts And Science Charter School"|
           district == "Paterson Arts And Science Charter School",
         school == "District Total",
         subgroup == "Total") %>% 
  select(district, n_test,p_prof) %>% 
  mutate(grade = "Grade 4",
         n_prof = p_prof* .01 * n_test) %>% 
  rbind(memb_math)

# 5th grade math -----
math <- read_excel('raw data/NJ_MAT05 NJSLA DATA 2021-22.xlsx')
names(math) <- tolower(math[2,])
math <- math[-c(1:2),]
memb_math <- math %>% 
  mutate(district = `district name`,
         school = `school name`,
         n_test = as.numeric(`valid scores`),
         p_prof = as.numeric(`l4 percent`) + as.numeric(`l5 percent`)) %>% 
  filter(district =="Garfield Public School District"|
           district =="Hoboken Public School District"|
           district == "Jersey City Public Schools"|
           district == "Kearny"|
           district == "Passaic City School District"|
           district == "Paterson Public School District"|
           district == "Bergen Arts And Science Charter School"|
           district == "Hoboken Charter School"|
           district == "The Learning Community Charter School"|
           district == "Hudson Arts And Science Charter School"|
           district == "Passaic Arts And Science Charter School"|
           district == "Paterson Arts And Science Charter School",
         school == "District Total",
         subgroup == "Total") %>% 
  select(district, n_test,p_prof) %>% 
  mutate(grade = "Grade 5",
         n_prof = p_prof* .01 * n_test) %>% 
  rbind(memb_math)

# 6th grade math -----
math <- read_excel('raw data/NJ_MAT07 NJSLA DATA 2021-22.xlsx')
names(math) <- tolower(math[2,])
math <- math[-c(1:2),]
memb_math <- math %>% 
  mutate(district = `district name`,
         school = `school name`,
         n_test = as.numeric(`valid scores`),
         p_prof = as.numeric(`l4 percent`) + as.numeric(`l5 percent`)) %>% 
  filter(district =="Garfield Public School District"|
           district =="Hoboken Public School District"|
           district == "Jersey City Public Schools"|
           district == "Kearny"|
           district == "Passaic City School District"|
           district == "Paterson Public School District"|
           district == "Bergen Arts And Science Charter School"|
           district == "Hoboken Charter School"|
           district == "The Learning Community Charter School"|
           district == "Hudson Arts And Science Charter School"|
           district == "Passaic Arts And Science Charter School"|
           district == "Paterson Arts And Science Charter School",
         school == "District Total",
         subgroup == "Total") %>% 
  select(district, n_test,p_prof) %>% 
  mutate(grade = "Grade 6",
         n_prof = p_prof* .01 * n_test) %>% 
  rbind(memb_math)         

# 7th grade math -----
math <- read_excel('raw data/NJ_MAT07 NJSLA DATA 2021-22.xlsx')
names(math) <- tolower(math[2,])
math <- math[-c(1:2),]
memb_math <- math %>% 
  mutate(district = `district name`,
         school = `school name`,
         n_test = as.numeric(`valid scores`),
         p_prof = as.numeric(`l4 percent`) + as.numeric(`l5 percent`)) %>% 
  filter(district =="Garfield Public School District"|
           district =="Hoboken Public School District"|
           district == "Jersey City Public Schools"|
           district == "Kearny"|
           district == "Passaic City School District"|
           district == "Paterson Public School District"|
           district == "Bergen Arts And Science Charter School"|
           district == "Hoboken Charter School"|
           district == "The Learning Community Charter School"|
           district == "Hudson Arts And Science Charter School"|
           district == "Passaic Arts And Science Charter School"|
           district == "Paterson Arts And Science Charter School",
         school == "District Total",
         subgroup == "Total") %>% 
  select(district, n_test,p_prof) %>% 
  mutate(grade = "Grade 7",
         n_prof = p_prof* .01 * n_test) %>% 
  rbind(memb_math)

# 8th grade math -----
math <- read_excel('raw data/NJ_MAT08 NJSLA DATA 2021-22.xlsx')
names(math) <- tolower(math[2,])
math <- math[-c(1:2),]
memb_math <- math %>% 
  mutate(district = `district name`,
         school = `school name`,
         n_test = as.numeric(`valid scores`),
         p_prof = as.numeric(`l4 percent`) + as.numeric(`l5 percent`)) %>% 
  filter(district =="Garfield Public School District"|
           district =="Hoboken Public School District"|
           district == "Jersey City Public Schools"|
           district == "Kearny"|
           district == "Passaic City School District"|
           district == "Paterson Public School District"|
           district == "Bergen Arts And Science Charter School"|
           district == "Hoboken Charter School"|
           district == "The Learning Community Charter School"|
           district == "Hudson Arts And Science Charter School"|
           district == "Passaic Arts And Science Charter School"|
           district == "Paterson Arts And Science Charter School",
         school == "District Total",
         subgroup == "Total") %>% 
  select(district, n_test,p_prof) %>% 
  mutate(grade = "Grade 8",
         n_prof = p_prof* .01 * n_test) %>% 
  rbind(memb_math)
memb_math <- mutate_all(memb_math, ~replace_na(.,0)) # Passaic City SD had 0 students for 8th grade valid tests

# total math ----
memb_math <- memb_math %>% 
  group_by(district) %>% 
  summarize(math_n = sum(n_test),
            math_prof = sum(n_prof)) %>% 
  mutate(math_prof = math_prof/math_n)
  

# ---- ASSESSMENT DATA: ELA ------
# Documentation on performance levels https://nj.mypearsonsupport.com/resources/reporting/NJSLA_Parent_Score_Interpretation_Guide_Spring2022.pdf
# Level 4 and 5 = on track for the next grade level

# 3rd grade ELA ---- 
# ELA files include 16384 columns due to merged top rows. Requires opening Excel, unmerging, saving, then loading the data again
ela <- read_excel('raw data/NJ_ELA03 NJSLA DATA 2021-22.xlsx')
View(ela)
names(ela) <- tolower(ela[2,])
ela <- ela[-c(1:2),]
str(ela)
memb_enrl$school
levels(as.factor(ela$`subgroup type`))
memb_ela <- ela %>% 
  mutate(district = `district name`,
         school = `school name`,
         n_test = as.numeric(`valid scores`),
         p_prof = as.numeric(`l4 percent`) + as.numeric(`l5 percent`)) %>% 
  filter(district =="Garfield Public School District"|
           district =="Hoboken Public School District"|
           district == "Jersey City Public Schools"|
           district == "Kearny"|
           district == "Passaic City School District"|
           district == "Paterson Public School District"|
           district == "Bergen Arts And Science Charter School"|
           district == "Hoboken Charter School"|
           district == "The Learning Community Charter School"|
           district == "Hudson Arts And Science Charter School"|
           district == "Passaic Arts And Science Charter School"|
           district == "Paterson Arts And Science Charter School",
         school == "District Total",
         subgroup == "Total") %>% 
  select(district, n_test,p_prof) %>% 
  mutate(grade = "Grade 3",
         n_prof = p_prof* .01 * n_test)


# 4th grade ELA -----
ela <- read_excel('raw data/NJ_ELA04 NJSLA DATA 2021-22.xlsx')
View(ela)
names(ela) <- tolower(ela[2,])
ela <- ela[-c(1:2),]
str(ela)
memb_enrl$school
levels(as.factor(ela$`subgroup type`))
memb_ela <- ela %>% 
  mutate(district = `district name`,
         school = `school name`,
         n_test = as.numeric(`valid scores`),
         p_prof = as.numeric(`l4 percent`) + as.numeric(`l5 percent`)) %>% 
  filter(district =="Garfield Public School District"|
           district =="Hoboken Public School District"|
           district == "Jersey City Public Schools"|
           district == "Kearny"|
           district == "Passaic City School District"|
           district == "Paterson Public School District"|
           district == "Bergen Arts And Science Charter School"|
           district == "Hoboken Charter School"|
           district == "The Learning Community Charter School"|
           district == "Hudson Arts And Science Charter School"|
           district == "Passaic Arts And Science Charter School"|
           district == "Paterson Arts And Science Charter School",
         school == "District Total",
         subgroup == "Total") %>% 
  select(district, n_test,p_prof) %>% 
  mutate(grade = "Grade 4",
         n_prof = p_prof* .01 * n_test) %>% 
  rbind(memb_ela)

# 5th grade ELA -----
ela <- read_excel('raw data/NJ_ELA05 NJSLA DATA 2021-22.xlsx')
names(ela) <- tolower(ela[2,])
ela <- ela[-c(1:2),]
memb_ela <- ela %>% 
  mutate(district = `district name`,
         school = `school name`,
         n_test = as.numeric(`valid scores`),
         p_prof = as.numeric(`l4 percent`) + as.numeric(`l5 percent`)) %>% 
  filter(district =="Garfield Public School District"|
           district =="Hoboken Public School District"|
           district == "Jersey City Public Schools"|
           district == "Kearny"|
           district == "Passaic City School District"|
           district == "Paterson Public School District"|
           district == "Bergen Arts And Science Charter School"|
           district == "Hoboken Charter School"|
           district == "The Learning Community Charter School"|
           district == "Hudson Arts And Science Charter School"|
           district == "Passaic Arts And Science Charter School"|
           district == "Paterson Arts And Science Charter School",
         school == "District Total",
         subgroup == "Total") %>% 
  select(district, n_test,p_prof) %>% 
  mutate(grade = "Grade 5",
         n_prof = p_prof* .01 * n_test) %>% 
  rbind(memb_ela)

# 6th grade ELA -----
ela <- read_excel('raw data/NJ_ELA06 NJSLA DATA 2021-22.xlsx')
names(ela) <- tolower(ela[2,])
ela <- ela[-c(1:2),]
memb_ela <- ela %>% 
  mutate(district = `district name`,
         school = `school name`,
         n_test = as.numeric(`valid scores`),
         p_prof = as.numeric(`l4 percent`) + as.numeric(`l5 percent`)) %>% 
  filter(district =="Garfield Public School District"|
           district =="Hoboken Public School District"|
           district == "Jersey City Public Schools"|
           district == "Kearny"|
           district == "Passaic City School District"|
           district == "Paterson Public School District"|
           district == "Bergen Arts And Science Charter School"|
           district == "Hoboken Charter School"|
           district == "The Learning Community Charter School"|
           district == "Hudson Arts And Science Charter School"|
           district == "Passaic Arts And Science Charter School"|
           district == "Paterson Arts And Science Charter School",
         school == "District Total",
         subgroup == "Total") %>% 
  select(district, n_test,p_prof) %>% 
  mutate(grade = "Grade 6",
         n_prof = p_prof* .01 * n_test) %>% 
  rbind(memb_ela)         

# 7th grade ELA -----
ela <- read_excel('raw data/NJ_ELA07 NJSLA DATA 2021-22.xlsx')
names(ela) <- tolower(ela[2,])
ela <- ela[-c(1:2),]
memb_ela <- ela %>% 
  mutate(district = `district name`,
         school = `school name`,
         n_test = as.numeric(`valid scores`),
         p_prof = as.numeric(`l4 percent`) + as.numeric(`l5 percent`)) %>% 
  filter(district =="Garfield Public School District"|
           district =="Hoboken Public School District"|
           district == "Jersey City Public Schools"|
           district == "Kearny"|
           district == "Passaic City School District"|
           district == "Paterson Public School District"|
           district == "Bergen Arts And Science Charter School"|
           district == "Hoboken Charter School"|
           district == "The Learning Community Charter School"|
           district == "Hudson Arts And Science Charter School"|
           district == "Passaic Arts And Science Charter School"|
           district == "Paterson Arts And Science Charter School",
         school == "District Total",
         subgroup == "Total") %>% 
  select(district, n_test,p_prof) %>% 
  mutate(grade = "Grade 7",
         n_prof = p_prof* .01 * n_test) %>% 
  rbind(memb_ela)

# 8th grade math -----
ela <- read_excel('raw data/NJ_ELA08 NJSLA DATA 2021-22.xlsx')
names(ela) <- tolower(ela[2,])
ela <- ela[-c(1:2),]
memb_ela <- ela %>% 
  mutate(district = `district name`,
         school = `school name`,
         n_test = as.numeric(`valid scores`),
         p_prof = as.numeric(`l4 percent`) + as.numeric(`l5 percent`)) %>% 
  filter(district =="Garfield Public School District"|
           district =="Hoboken Public School District"|
           district == "Jersey City Public Schools"|
           district == "Kearny"|
           district == "Passaic City School District"|
           district == "Paterson Public School District"|
           district == "Bergen Arts And Science Charter School"|
           district == "Hoboken Charter School"|
           district == "The Learning Community Charter School"|
           district == "Hudson Arts And Science Charter School"|
           district == "Passaic Arts And Science Charter School"|
           district == "Paterson Arts And Science Charter School",
         school == "District Total",
         subgroup == "Total") %>% 
  select(district, n_test,p_prof) %>% 
  mutate(grade = "Grade 8",
         n_prof = p_prof* .01 * n_test) %>% 
  rbind(memb_ela)

#
memb_ela <- memb_ela %>% 
  group_by(district) %>% 
  summarize(ela_n = sum(n_test),
            ela_prof = sum(n_prof)) %>% 
  mutate(ela_prof = ela_prof/ela_n)

memb_acad <- merge(memb_math,memb_ela) 

write.csv(memb_acad, file = file.path('output data/nj_acad.csv'),row.names = FALSE)
