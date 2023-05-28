# --- Amy Jiravisitcul. 24 Apr 2023 ----
rm(list = ls()) # clear working environment
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
# ------ ENROLLMENT BY RACE BY SCHOOL 2022-2023 ------
# import data from https://www.louisianabelieves.com/docs/default-source/data-management/feb-2023-multi-stats-(total-by-site-and-school-system).xlsx?sfvrsn=95896018_2
# Metro Statistical Area = Jefferson, Orleans, Plaquemines, St. Bernard, St. Charles, St. John the Baptist, St. Tammany, and St. James parishes

enrl <- read_excel("raw data/LA_feb-2023-multi-stats-(total-by-site-and-school-system).xlsx",
                   sheet = "Total by Site")
names(enrl) <- enrl[2,]
names(enrl)[7:36] <- enrl[3,c(7:36)]
enrl <- enrl[-c(1:4),] # remove rows with no data
names(enrl)<- tolower(names(enrl))
summary(enrl)
enrl <- enrl %>% 
  select(`school system`,`school system name`,`site name`, 
         `total enrollment`,`american indian`, asian, black, hispanic, `hawaiian/pacific islander`, 
         white, `multiple races (non-hispanic)`,`% limited english proficient`, `% economically disadvantaged`)
enrl <- enrl %>% 
  mutate(dist_code = as.factor(`school system`),
         district = as.factor(`school system name`),
         school = as.factor(`site name`),
         total = as.numeric(`total enrollment`),
         amind = as.numeric(`american indian`)/total,
         asian = as.numeric(asian) + as.numeric(`hawaiian/pacific islander`),
         asian = asian/total,
         black = as.numeric(black)/total,
         hispanic = as.numeric(hispanic)/total,
         white = as.numeric(white)/total,
         multiple = as.numeric(`multiple races (non-hispanic)`)/total,
         lep = as.numeric(`% limited english proficient`),
         ed = as.numeric(`% economically disadvantaged`)) %>% 
  select(dist_code, district, school, total, amind, asian, black, hispanic, white, multiple, lep, ed)
memb <- enrl %>% 
  filter(school == "International High School of New Orleans"|
           school == "International School of Louisiana"|
           school == "Kenner Discovery Health Sciences Academy"|
           school == "Lycee Francais de la Nouvelle-Orleans"|
           school == "Morris Jeff Community School"|
           str_detect(school,"Edward Hynes Charter School")|
           school == "Bricolage Academy"|
           school == "Homer Plessy Community School")

# --- DISTRICT-LEVEL SEGREGATION (PARISH) ------
locseg <- enrl %>% 
  select(school, district, dist_code, amind, asian, black, hispanic, white,multiple) %>% 
  gather(subgroup, n, amind:multiple, factor_key=TRUE) # change from wide to long

unique(memb$district)
jeff <- locseg %>% 
  filter(district == "Jefferson Parish") %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls)
memb_locseg <- jeff %>% 
  filter(school == "International High School of New Orleans"|
           school == "International School of Louisiana"|
           school == "Kenner Discovery Health Sciences Academy"|
           school == "Lycee Francais de la Nouvelle-Orleans"|
           school == "Morris Jeff Community School"|
           str_detect(school,"Edward Hynes Charter School")|
           school == "Bricolage Academy"|
           school == "Homer Plessy Community School") %>% 
  mutate(ls_district = ls,
         p_district = p,
         district = "Jefferson Parish") %>% 
  select(school, district, ls_district, p_district)

unique(memb$district)
type2 <- locseg %>% 
  filter(district == "Type 2 Charters") %>%
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls)
x <- type2 %>% 
  filter(school == "International High School of New Orleans"|
           school == "International School of Louisiana"|
           school == "Kenner Discovery Health Sciences Academy"|
           school == "Lycee Francais de la Nouvelle-Orleans"|
           school == "Morris Jeff Community School"|
           str_detect(school,"Edward Hynes Charter School")|
           school == "Bricolage Academy"|
           school == "Homer Plessy Community School") %>% 
  mutate(ls_district = ls,
         p_district = p,
         district = "Type 2 Charters") %>% 
  select(school, district, ls_district, p_district)
memb_locseg <- rbind(x,memb_locseg)

unique(memb$district)
orleans <- locseg %>% 
  filter(district == "Orleans Parish") %>%
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls)
x <- orleans %>% 
  filter(school == "International High School of New Orleans"|
           school == "International School of Louisiana"|
           school == "Kenner Discovery Health Sciences Academy"|
           school == "Lycee Francais de la Nouvelle-Orleans"|
           school == "Morris Jeff Community School"|
           str_detect(school,"Edward Hynes Charter School")|
           school == "Bricolage Academy"|
           school == "Homer Plessy Community School") %>% 
  mutate(ls_district = ls,
         p_district = p,
         district = "Orleans Parish") %>% 
  select(school, district, ls_district, p_district)
memb_locseg <- rbind(x,memb_locseg)

# ---- GREATER METRO NOLA ----
# Metro Statistical Area = Jefferson, Orleans, Plaquemines, St. Bernard, St. Charles, St. John the Baptist, St. Tammany, and St. James parishes + Type 2 Charters
unique(locseg$district)
metro <- locseg %>% 
  filter(district == "Orleans Parish"|
           district == "Jefferson Parish"|
           district == "Plaquemines Parish"|
           district == "St. Bernard Parish"|
           district == "St. Charles Parish"|
           district == "St. John the Baptist Parish"|
           district == "St. Tammany Parish"|
           district == "St. James Parish"|
           district == "Type 2 Charters") %>%
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls)
x <- metro %>% 
  filter(school == "International High School of New Orleans"|
           school == "International School of Louisiana"|
           school == "Kenner Discovery Health Sciences Academy"|
           school == "Lycee Francais de la Nouvelle-Orleans"|
           school == "Morris Jeff Community School"|
           str_detect(school,"Edward Hynes Charter School")|
           school == "Bricolage Academy"|
           school == "Homer Plessy Community School") %>% 
  mutate(ls_metro = ls,
         p_metro = p,
         metro = "NOLA metro") %>% 
  select(school, metro, ls_metro, p_metro)

memb <- merge(memb, merge(x,memb_locseg),by=c("school","district"))

write.csv(memb, file = file.path("output data/la_enrl.csv"), row.names = FALSE)

# ------ COMPARISON ENTITIES ------
# import data from https://www.louisianabelieves.com/docs/default-source/data-management/feb-2021-multi-stats-(total-by-site-and-school-system).xlsx?sfvrsn=f4b16718_6
# Metro Statistical Area = Jefferson, Orleans, Plaquemines, St. Bernard, St. Charles, St. John the Baptist, St. Tammany, and St. James parishes

enrl <- read_excel("raw data/LA_feb-2023-multi-stats-(total-by-site-and-school-system).xlsx",sheet = "Total by School System")

names(enrl) <- enrl[2,]
names(enrl)[4:34] <- enrl[3,c(4:34)]
enrl <- enrl[-c(1:4),] # remove rows with no data
names(enrl)<- tolower(names(enrl))
summary(enrl)
enrl <- enrl %>% 
  select(`school system`,`school system name`,`total enrollment`,
         `american indian`, asian, black, hispanic, `hawaiian/pacific islander`, 
         white, `multiple races (non-hispanic)`,`% limited english proficiency`, `% economically disadvantaged`)
enrl <- enrl %>% 
  mutate(dist_code = as.factor(`school system`),
         district = as.factor(`school system name`),
         total = as.numeric(`total enrollment`),
         amind = as.numeric(`american indian`)/total,
         asian = as.numeric(asian) + as.numeric(`hawaiian/pacific islander`),
         asian = asian/total,
         black = as.numeric(black)/total,
         hispanic = as.numeric(hispanic)/total,
         white = as.numeric(white)/total,
         multiple = as.numeric(`multiple races (non-hispanic)`)/total,
         lep = as.numeric(`% limited english proficiency`),
         ed = as.numeric(`% economically disadvantaged`)) %>% 
  select(dist_code, district, total, amind, asian, black, hispanic, white, multiple, lep, ed)

metro_totals <- enrl %>% 
  filter(district == "Orleans Parish"|
           district == "Jefferson Parish"|
           district == "Plaquemines Parish"|
           district == "St. Bernard Parish"|
           district == "St. Charles Parish"|
           district == "St. John the Baptist Parish"|
           district == "St. Tammany Parish"|
           district == "St. James Parish"|
           district == "Type 2 Charters")
write.csv(metro_totals,file = file.path('output data/la_entities.csv'),row.names = FALSE)

# ---- SPECIAL EDUCATION ENROLLMENT ----
# Data from https://www.louisianabelieves.com/docs/default-source/academics/2022-oct-swd-rates-by-lea-site_public.xlsx?sfvrsn=73be6018_2

sped <- read_excel("raw data/LA_2022-oct-swd-rates-by-lea-site_public.xlsx",sheet = "Oct Total by Site")
names(sped) <- tolower(sped[6,])
sped <- sped[-c(1:6),]
names(sped)[c(3,5:6)] <- c("district","school","swd")

sped <- sped %>% 
  mutate(swd = case_when(is.na(as.numeric(swd)) ~ .01 * as.numeric(gsub("[^0-9.-]", "", swd)),
                         TRUE ~ as.numeric(swd))) %>% 
  na.omit() %>% 
  select(district, school, swd)
memb_sped <- sped %>% 
  filter(school == "International High School of New Orleans"|
           school == "International School of Louisiana"|
           school == "Kenner Discovery Health Sciences Academy"|
           school == "Lycee Francais de la Nouvelle-Orleans"|
           school == "Morris Jeff Community School"|
           str_detect(school,"Edward Hynes Charter School")|
           school == "Bricolage Academy"|
           school == "Homer Plessy Community School") %>% 
  select(-district)

memb <- merge(memb_sped,memb)
write.csv(memb, file = file.path('output data/la_enrl.csv'),row.names = FALSE)

comp_sped <- read_excel("raw data/LA_2022-oct-swd-rates-by-lea-site_public.xlsx",sheet = "Oct Total by LEA")
names(comp_sped) <- tolower(comp_sped[6,])
comp_sped <- comp_sped[-c(1:6),-c(1:2)]
names(comp_sped)<- c("district","swd")

type2schools <- data.frame(charters = unique(type2$school))
View(type2schools)
unique(comp_sped$district)
comp_sped <- comp_sped %>% 
  filter(district == "Jefferson Parish"|
           district == "Orleans Parish"|
           district == "Plaquemines Parish"|
           district == "St. Bernard Parish"|
           district == "St. Charles Parish"|
           district == "St. John the Baptist Parish"|
           district == "St. Tammany Parish"|
           district == "St. James Parish"|
           district == "Type 2 Charters"|
           match(district,unique(type2$school))>0) %>% 
  mutate(swd = case_when(is.na(as.numeric(swd)) ~ .01 * as.numeric(gsub("[^0-9.-]", "", swd)),
                         TRUE ~ as.numeric(swd)))%>% 
  select(district, swd)
schools <- enrl %>% # go back to variable enrl in lines 15-29
  mutate(total = as.numeric(total),
         district = school) %>% 
  select(district, total)

systems <- enrl %>% # go back to variable enrl in lines 159-168
  mutate(total = as.numeric(total)) %>% 
  select(district, total)
z <- merge(comp_sped,schools, by="district")
y <- merge(comp_sped, systems,by="district")
comp_sped <- rbind(z,y)

write.csv(memb_sped, file = file.path('output data/la_sped_memb.csv'),row.names = FALSE)
write.csv(comp_sped, file = file.path('output data/la_entities_sped.csv'),row.names = FALSE)

# ------ LEAP SCORES ------
# Spring 2022 assessment data downloaded from https://www.louisianabelieves.com/docs/default-source/test-results/2022-state-lea-school-leap-grade-3-8-achievement-level-subgroup-summary.xlsx?sfvrsn=2c1a6218_9

acad <- read_excel("raw data/LA_2022-state-lea-school-leap-grade-3-8-achievement-level-subgroup-summary.xlsx")
names(acad) <- tolower(acad[2,])
acad <- acad[-c(1,2),]

# ---- Math -----
math <- acad[,c(1:7,13:17)]
names(math)[8:12] <- tolower(math[1,8:12])
math <- math[-1,]
math_race <- math %>% 
  mutate(level = as.factor(`summary level`),
         sys_code = as.factor(`school system code`),
         sys_name = as.factor(`school system name`),
         school_code = as.factor(`school code`),
         school = as.factor(`school name`),
         grade = as.factor(grade),
         subgroup = as.factor(subgroup),
         p_adv = as.numeric(gsub("[[:punct:]]", "", `% advanced`)),
         p_mas = as.numeric(`% mastery`),
         p_bas = as.numeric(`% basic`),
         p_appr = as.numeric(`% approaching basic`),
         p_unsat = as.numeric(gsub("[[:punct:]]", "", `% unsatisfactory`)),
         pass = (p_adv+p_mas)*.01) %>% 
  filter(school == "Bricolage Academy"|
           school == "Homer Plessy Community School"|
           str_detect(school,"Edward Hynes")|
           school == "Kenner Discovery Health Sciences Academy"|
           school == "Lycee Francais de la Nouvelle-Orleans"|
           school == "Morris Jeff Community School",
         subgroup == "Total Population"|
           subgroup == "American Indian or Alaska Native"|
           subgroup == "Asian"|
           subgroup == "Black or African American"|
           subgroup == "Hispanic/Latino"|
           subgroup == "Native Hawaiian/Other Pacific Islander"|
           subgroup == "White"|
           subgroup == "Two or more races") %>% 
  select(level:school,grade, subgroup, pass)

math <- math_race %>% 
  filter(subgroup == "Total Population") %>% 
  select(-subgroup)

# Load enrollment data from 2021-2022 school year to match grade-level enrollments
grade_enrl <- read_excel("raw data/LA_feb-2022-multi-stats-(total-by-site-and-school-system).xlsx",
                   sheet = "Total by Site")
levels(math$subgroup)
names(grade_enrl) <- tolower(grade_enrl[2,])
names(grade_enrl)[22:36] <- tolower(grade_enrl[3,c(22:36)])
grade_enrl <- grade_enrl[-c(1:5),c(1:5,25:30)]
names(grade_enrl)[5] <- "school"
grade_enrl <- grade_enrl %>% 
  filter(school == "Bricolage Academy"|
           school == "Homer Plessy Community School"|
           str_detect(school,"Edward Hynes")|
           school == "Kenner Discovery Health Sciences Academy"|
           school == "Lycee Francais de la Nouvelle-Orleans"|
           school == "Morris Jeff Community School")
grade_enrl <- grade_enrl %>% 
  gather(grade, n, `grade 3`:`grade 8`) %>% 
  mutate(n = as.numeric(n),
         grade = gsub("grade ","0",as.character(grade))) %>% 
  select(school, grade, n)

math <- merge(math, grade_enrl) %>% 
  mutate(n_pass = n*pass)%>%
  select(school, n, n_pass) %>% 
  replace(is.na(.),0) %>% 
  group_by(school) %>% 
  summarize(n = sum(n),
            n_pass = sum(n_pass))%>%
  mutate(pass_math = n_pass/n) %>% 
  select(school, n, pass_math)

# ---- ELA ----
names(acad)
ela <- acad[,c(1:12)]
names(ela)[8:12] <- tolower(ela[1,8:12])
ela <- ela[-1,]
ela_race <- ela %>% 
  mutate(level = as.factor(`summary level`),
         sys_code = as.factor(`school system code`),
         sys_name = as.factor(`school system name`),
         school_code = as.factor(`school code`),
         school = as.factor(`school name`),
         grade = as.factor(grade),
         subgroup = as.factor(subgroup),
         p_adv = as.numeric(gsub("[[:punct:]]", "", `% advanced`)),
         p_mas = as.numeric(`% mastery`),
         p_bas = as.numeric(`% basic`),
         p_appr = as.numeric(`% approaching basic`),
         p_unsat = as.numeric(gsub("[[:punct:]]", "", `% unsatisfactory`)),
         pass = (p_adv+p_mas)*.01) %>% 
  filter(school == "Bricolage Academy"|
           school == "Homer Plessy Community School"|
           str_detect(school,"Edward Hynes")|
           school == "Kenner Discovery Health Sciences Academy"|
           school == "Lycee Francais de la Nouvelle-Orleans"|
           school == "Morris Jeff Community School",
         subgroup == "Total Population"|
           subgroup == "American Indian or Alaska Native"|
           subgroup == "Asian"|
           subgroup == "Black or African American"|
           subgroup == "Hispanic/Latino"|
           subgroup == "Native Hawaiian/Other Pacific Islander"|
           subgroup == "White"|
           subgroup == "Two or more races") %>% 
  select(level:school,grade, subgroup, pass)

ela <- ela_race %>% 
  filter(subgroup == "Total Population") %>% 
  select(-subgroup)

ela <- merge(ela, grade_enrl) %>% 
  mutate(n_pass = n*pass)%>%
  select(school, n, n_pass) %>% 
  replace(is.na(.),0) %>% 
  group_by(school) %>% 
  summarize(n = sum(n),
            n_pass = sum(n_pass))%>%
  mutate(pass_ela = n_pass/n) %>% 
  select(school, pass_ela)

# merge ELA and math pass rages by total population
memb_acad <- merge(math, ela) %>% arrange(school)
memb_acad$sys_name <- c("Orleans Parish","Orleans Parish","Orleans Parish",
                        "Jefferson Parish", "Orleans Parish","Orleans Parish")

# ---- Comparison districts' 3-8 assessment results -----
dist_math <- acad[,c(1:7,13:17)]
names(dist_math)[8:12] <- tolower(dist_math[1,8:12])
dist_math <- dist_math[-1,]
dist_math <- dist_math %>% 
  mutate(level = as.factor(`summary level`),
         sys_code = as.factor(`school system code`),
         sys_name = as.factor(`school system name`),
         grade = as.factor(grade),
         subgroup = as.factor(subgroup),
         p_adv = as.numeric(gsub("[[:punct:]]", "", `% advanced`)),
         p_mas = as.numeric(`% mastery`),
         p_bas = as.numeric(`% basic`),
         p_appr = as.numeric(`% approaching basic`),
         p_unsat = as.numeric(gsub("[[:punct:]]", "", `% unsatisfactory`)),
         pass = (p_adv+p_mas)*.01) %>% 
  filter(subgroup == "Total Population"|
           subgroup == "American Indian or Alaska Native"|
           subgroup == "Asian"|
           subgroup == "Black or African American"|
           subgroup == "Hispanic/Latino"|
           subgroup == "Native Hawaiian/Other Pacific Islander"|
           subgroup == "White"|
           subgroup == "Two or more races",
         level == "School System") %>% 
  select(level:sys_name,grade, subgroup, pass)
levels(dist_math$sys_name)

z <- dist_math %>% 
  filter(sys_name == "Jefferson Parish"|
           sys_name == "Orleans Parish",
         subgroup == "Total Population")

parish_enrl <- read_excel("raw data/LA_feb-2022-multi-stats-(total-by-site-and-school-system).xlsx", sheet = "Total by School System")

names(parish_enrl) <- tolower(parish_enrl[5,])
parish_enrl <- parish_enrl[-c(1:5),c(2,23:28)]
parish_enrl <- parish_enrl %>% 
  filter(`school system name` == "Jefferson Parish"|
           `school system name` == "Orleans Parish") %>% 
  gather(grade, n, grade3:grade8) %>% 
  mutate(n = as.numeric(n),
         grade = gsub("grade","0",as.character(grade)),
         sys_name = `school system name`) %>% 
  select(sys_name, grade, n)

dist_acad <- merge(parish_enrl, z) %>% 
  select(sys_name, grade, n, pass) %>% 
  mutate(n_pass = n*pass) %>% 
  select(sys_name, n, n_pass) %>% 
  group_by(sys_name) %>% 
  summarize(n = sum(n),
            n_pass = sum(n_pass))%>%
  mutate(dist_pass_math = n_pass/n)

dist_ela <- acad[,c(1:12)]
names(dist_ela)[8:12] <- tolower(dist_ela[1,8:12])
dist_ela <- dist_ela[-1,]
dist_ela <- dist_ela %>% 
  mutate(level = as.factor(`summary level`),
         sys_code = as.factor(`school system code`),
         sys_name = as.factor(`school system name`),
         grade = as.factor(grade),
         subgroup = as.factor(subgroup),
         p_adv = as.numeric(gsub("[[:punct:]]", "", `% advanced`)),
         p_mas = as.numeric(`% mastery`),
         p_bas = as.numeric(`% basic`),
         p_appr = as.numeric(`% approaching basic`),
         p_unsat = as.numeric(gsub("[[:punct:]]", "", `% unsatisfactory`)),
         pass = (p_adv+p_mas)*.01) %>% 
  filter(subgroup == "Total Population"|
           subgroup == "American Indian or Alaska Native"|
           subgroup == "Asian"|
           subgroup == "Black or African American"|
           subgroup == "Hispanic/Latino"|
           subgroup == "Native Hawaiian/Other Pacific Islander"|
           subgroup == "White"|
           subgroup == "Two or more races",
         level == "School System") %>% 
  select(level:sys_name,grade, subgroup, pass)

z <- dist_ela %>% 
  filter(sys_name == "Jefferson Parish"|
           sys_name == "Orleans Parish",
         subgroup == "Total Population")

dist_acad <- merge(parish_enrl, z) %>% 
  select(sys_name, grade, n, pass) %>% 
  mutate(n_pass = n*pass) %>% 
  select(sys_name, n, n_pass) %>% 
  group_by(sys_name) %>% 
  summarize(n = sum(n),
            n_pass = sum(n_pass))%>%
  mutate(dist_pass_ela = n_pass/n) %>% 
  select(sys_name, dist_pass_ela) %>% 
  merge(dist_acad) %>% 
  select(-n_pass)
  
memb_acad <- merge(memb_acad,dist_acad, by = "sys_name")
write.csv(memb_acad, file = file.path('output data/la_acad.csv'), row.names = FALSE)
#------ END -----
