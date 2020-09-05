rm(list = ls()) # clear working environment
setwd("~/Downloads/States/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)

# ----- Enrollment ------
# import data from https://reportcard.msde.maryland.gov/DataDownloads/FileDownload/376
md <- read.csv("raw_data/MD_Enrollment_2019.csv")
summary(md)
str(md)
names(md)
md<- md %>% 
  filter(LSS.Name == "Baltimore City",
         Grade == "Total Enrollment") %>% 
  select(LSS.Name, School.Name, Enrolled.Count) %>% 
  arrange(School.Name)
md$c_amind = NA
md$c_asian = NA
md$c_black = NA
md$c_hispanic = NA
md$c_white = NA
md$c_multiple = NA
md$p_frpm = NA
md$p_lep = NA
md$p_swd = NA
# input info from Baltimore City schools list https://reportcard.msde.maryland.gov/SchoolsList/Index?l=30
md[1,4:12] <- c(0,0,294,16,0,0,.7,.05,.182)
md[2,4:12] <- c(0,0,477,37,17,0,.588,.082,.282)
md[3,4:12] <- c(0,0,391,16, 0,0,.626,.05,.285)
md[4,4:12] <- c(0,0,337,13,0,0,.579,0,.316)
md[5,4:12] <- c(0,0,176,0,0,0,.718,0,.109)
md <- md[-6,]
md[6,4:12] <- c(0,0,455,80,0,0,.638,.148,.151)
md[7,4:12] <- c(0,14,76,489,191,16,.473,.259,.135)
md[8,4:12] <- c(0,0,461,14,0,0,.73,0,.105)
md[9,4:12] <- c(0,0,404,0,0,0,.714,0,.336)
md[10,4:12] <- c(0,17,989,112,231,0,.276,0,.05)
md[11,4:12] <- c(0,0,463,10,0,0,.417,0,.183)
md[12,4:12] <- c(0,0,480,15,17,0,.547,0,.147)
md[13,4:12] <- c(0,0,628,31,26,19,.386,0,.05)
md[14,4:12] <- c(0,0,524,0,0,0,.503,0,.067)
md[15,4:12] <- c(0,0,266,13,138,22,.349,0,.139)
md[16,4:12] <- c(0,107,1044,150,262,20,.233,0,.05)
md[17,4:12] <- c(0,13,213,24,173,14,.121,0,.05)
md[18,4:12] <- c(0,0,195,0,0,0,.703,0,.217)
md[19,4:12] <- c(0,10,416,42,0,0,.606,.082,.128)
md[20,4:12] <- c(0,0,404,15,57,0,.355,0,.05)
md[21,4:12] <- c(0,0,272,100,20,0,.657,.165,.125)
md[22,4:12] <- c(0,0,544,25,14,0,.662,.05,.146)
md[23,4:12] <- c(0,0,260,0,0,0,.764,0,.151)
md[24,4:12] <- c(0,0,229,175,115,0,.438, .269,.223)
md[25,4:12] <- c(0,0,224,0,0,0,.725,0,.405)
md[26,4:12] <- c(0,0,223,0,0,0,.828,0,.31)
md[27,4:12] <- c(0,0,677,10,0,0,.72,0,.158)
md[28,4:12] <- c(0,0,298,0,0,0,.654,0,.235)
md[29,4:12] <- c(0,0,416,17,0,0,.781,0,.203)
md[30,4:12] <- c(0,0,669,25,0,0,.423,.1,.117)
md[31,4:12] <- c(0,0,853,11,0,0,.579,0,.106)
md[32,4:12] <- c(0,0,330,0,0,0,.734,0,.153)
md[33,4:12] <- c(0,14,139,125,56,0,.584,.385,.092)
md[34,4:12] <- c(0,0,631,22,10,0,.811,0,.181)
md[35,4:12] <- c(0,0,139,0,86,0,.223,0,.214)
md[36,4:12] <- c(0,0,115,12,90,14,.235,0,.248)
md[37,4:12] <- c(0,0,374,14,22,0,.408,0,.365)
md[38,4:12] <- c(0,0,676,11,13,0,.788,.05,.153)
md[39,4:12] <- c(0,0,54,0,0,0,.618,0,.95)
md[40,4:12] <- c(0,0,341,0,0,0,.799,0,.151)
md[41,4:12] <- c(0,0,511,312,51,0,.553,.287,.102)
md[42,4:12] <- c(0,0,504,0,0,0,.657,0,.285)
md[43,4:12] <- c(0,0,346,0,0,0,.489,0,.173)
md[44,4:12] <- c(0,0,272,0,37,12,.554,0,.185)
md[45,4:12] <- c(0,0,594,89,16,0,.462,.058,.112)
md[46,4:12] <- c(0,0,257,109,176,29,.608,.144,.148)
md[47,4:12] <- c(0,0,210,15,0,0,.636,.095,.190)
md[48,4:12] <- c(0,0,382,10,0,0,.511,.05,.103)
md[49,4:12] <- c(0,18,775,307,88,0,.477,.224,.186)
md[50,4:12] <- c(0,0,324,0,0,0,.721,0,.129)
md[51,4:12] <- c(0,0,309,0,0,0,.802,0,.151)
md[52,4:12] <- c(0,0,279,0,0,0,.76,0,.187)
md[53,4:12] <- c(0,0,358,0,0,0,.636,0,.179)
md[54,4:12] <- c(0,0,32,0,0,0,.543,0,.283)
md[55,4:12] <- c(0,0,199,0,0,0,.746,0,.264)
md[56,4:12] <- c(0,0,183,0,0,0,.761,0,.244)
md[57,4:12] <- c(0,0,860,10,0,0,.609,0,.17)
md[58,4:12] <- c(0,0,522,12,0,0,.557,0,.116)
md[59,4:12] <- c(0,0,253,0,0,0,.496,0,.117)
md[60,4:12] <- c(0,0,248,0,0,0,.839,0,.179)
md[61,4:12] <- c(0,0,555,0,0,0,.677, 0, .262)
md[62,4:12] <- c(0,0,235,214,0,0,.363,.392,.099)
md[63,4:12] <- c(0,11,183,23,95,0,.399,0,.201)
md[64,4:12] <- c(0,0,493,87,0,0,.496,.148,.206)
md[65,4:12] <- c(0,0,700,22,11,0,.744,.05,.135)
md[66,4:12] <- c(0,0,237,41,206,16,.342,.05,.179)
md[67,4:12] <- c(0,0,446,0,0,0,.806,0,.17)
md[68,4:12] <- c(0,0,835,10,0,0,.673,0,.283)
md[69,4:12] <- c(0,0,323,10,40,0,.843,0,.127)
md[70,4:12] <- c(0,0,393,17,17,0,.688,.076,.176)
md[71,4:12] <- c(0,0,454,0,0,0,.8,0,.155)
md[72,4:12] <- c(0,0,277,0,0,0,.629,0,.179)
md[73,4:12] <- c(0,0,280,0,32,0,.497,0,.121)
md[74,4:12] <- c(0,0,39,0,0,0,.479,0,.95)
md[75,4:12] <- c(0,0,183,10,20,0,.69,0,.103)
md[76,4:12] <- c(0,0,253,0,0,0,.847,0,.157)
md[77,4:12] <- c(0,0,652,27,16,0,.515,.05,.179)
md[78,4:12] <- c(0,0,394,0,0,0,.651,0,.131)
md[79,4:12] <- c(0,0,113,322,31,0,.428,.416,.117)
md[80,4:12] <- c(0,0,808,17,22,0,.549,0,.18)
md[81,4:12] <- c(0,0,263,0,0,0,.648,0,.117)
md[82,4:12] <- c(0,0,308,0,0,0,.601,0,.193)
md[83,4:12] <- c(0,0,680,61,84,27,.348,.05,.103)
md[84,4:12] <- c(0,13,164,30,226,16,.371,.051,.169)
md[85,4:12] <- c(0,10,144,326,297,34,.254,.167,.054)
md[86,4:12] <- c(0,0,398,0,0,0,.778,0,.156)
md[87,4:12] <- c(0,0,299,0,0,0,.82,0,.132)
md[88,4:12] <- c(0,0,430,18,0,0,.593,0,.201)
md[89,4:12] <- c(0,0,86,322,0,0,.403,.541,.063)
md[90,4:12] <- c(0,0,150,595,25,0,.392,.567,.085)
md[91,4:12] <- c(0,0,333,0,14,0,.605,0,.192)
md[92,4:12] <- c(0,0,189,188,65,0,.483,.301,.112)
md[93,4:12] <- c(0,0,132,0,13,0,.533,0,.352)
md[94,4:12] <- c(0,0,478,11,17,0,.854,0,.154)
md[95,4:12] <- c(0,0,275,0,0,0,.759,0,.237)
md[96,4:12] <- c(0,0,40,688,76,0,.323,.634,.084)
md[97,4:12] <- c(0,0,273,0,0,0,.798,0,.182)
md[98,4:12] <- c(0,0,70,0,0,0,.645,0,.95)
md[99,4:12] <- c(0,0,1478,24,0,0,.547,0,.095)
md[100,4:12] <- c(0,0,350,511,36,0,.384,.348,.114)
md[101,4:12] <- c(0,0,96,0,0,0,.705,0,.455)
md[102,4:12] <- c(0,0,982,54,0,0,.494,.05,.116)
md[103,4:12] <- c(0,0,464,10,0,0,.612,0,.159)
md[104,4:12] <- c(0,0,265,0,0,0,.47,0,.113)
md[105,4:12] <- c(0,0,261,0,0,0,.775,0,.092)
md[106,4:12] <- c(0,0,36,0,0,0,.438,0,.95)
md[107,4:12] <- c(0,18,246,298,167,13,.465,.225,.133)
md[108,4:12] <- c(0,11,205,77,31,0,.538,.182,.108)
md[109,4:12] <- c(0,0,238,0,0,0,.756,0,.12)
md[110,4:12] <- c(0,0,260,0,0,0,.751,0,.114)
md[111,4:12] <- c(0,0,348,0,0,0,.774,0,.125)
md[112,4:12] <- c(0,40,151,20,154,24,.247,.094,.166)
md[113,4:12] <- c(0,0,1571,81,43,0,.476,.05,.105)
md[114,4:12] <- c(0,0,157,10,19,0,.333,0,.174)
md[115,4:12] <- c(0,0,949,22,0,0,.634,0,.124)
md[116,4:12] <- c(0,0,538,11,0,0,.696,0,.162)
md[117,4:12] <- c(0,40,677,17,21,0,.509,.208,.131)
md[118,4:12] <- c(0,23,117,84,200,19,.597,.145,.184)
md[119,4:12] <- c(0,0,657,28,65,0,.519,0,.121)
md[120,4:12] <- c(0,0,325,0,0,0,.531,0,.282)
md[121,4:12] <- c(0,0,571,203,15,0,.472,.253,.214)
md[122,4:12] <- c(0,0,220,53,14,0,.51,.228,.189)
md[123,4:12] <- c(0,0,162,0,0,0,.606,0,.106)
md[124,4:12] <- c(0,0,432,20,0,0,.55,0,.134)
md[125,4:12] <- c(0,0,162,0,0,0,.486,0,.137)
md[126,4:12] <- c(0,0,591,14,11,0,.531,0,.15)
md[127,4:12] <- c(0,0,576,371,83,0,.397,.417,.161)
md[128,4:12] <- c(0,0,332,263,104,17,.414,.187,.145)
md[129,4:12] <- c(0,0,812,49,0,0,.475,0,.106)
md[130,4:12] <- c(0,0,420,14,0,0,.71,0,.234)
md[131,4:12] <- c(0,0,452,69,10,0,.503,.145,.237)
md[132,4:12] <- c(0,0,239,0,0,0,.657,0,.326)
md[133,4:12] <- c(0,0,290,0,0,0,.766,0,.152)
md[134,4:12] <- c(0,146,552,78,602,59,.141,.05,.059)
md[135,4:12] <- c(0,0,138,0,0,0,.745,0,.164)
md[136,4:12] <- c(0,0,327,0,0,0,.722,0,.11)
md[137,4:12] <- c(0,0,206,0,0,0,.702,0,.123)
md[138,4:12] <- c(0,0,24,0,0,0,.625,0,.95)
md[139,4:12] <- c(0,0,295,0,0,0,.708,0,.105)
md[140,4:12] <- c(0,0,368,15,32,0,.639,0,.195)
md[141,4:12] <- c(0,0,336,11,0,0,.682,0,.253)
md[142,4:12] <- c(0,0,222,0,19,0,.841,0,.181)
md[143,4:12] <- c(0,0,354,32,0,0,.764,.086,.134)
md[144,4:12] <- c(0,0,117,35,10,0,.522,.075,.137)
md[145,4:12] <- c(0,0,71,13,60,16,.19,0,.114)
md[146,4:12] <- c(0,0,255,0,0,0,.813,0,.164)
md[147,4:12] <- c(0,15,385,24,158,29,.17,0,.091)
md[148,4:12] <- c(0,0,538,0,0,0,.653,0,.277)
md[149,4:12] <- c(0,0,465,12,0,0,.574,0,.106)
md[150,4:12] <- c(0,0,86,38,385,27,.202,.05,.128)
md[151,4:12] <- c(0,0,379,0,71,10,.256,0,.141)
md[152,4:12] <- c(0,0,332,13,15,0,.648,.187,.215)
md[153,4:12] <- c(0,19,192,55,121,12,.52,.112,.101)
md[154,4:12] <- c(0,0,343,12,16,0,.664,0,.132)
md[155,4:12] <- c(0,0,321,11,0,0,.718,0,.285)
md[156,4:12] <- c(0,0,570,24,14,0,.605,.05,.151)
md[157,4:12] <- c(0,10,970,46,53,0,.416,.05,.05)
md[158,4:12] <- c(0,0,286,11,0,0,.773,0,.329)
md[159,4:12] <- c(0,0,768,14,0,0,.672,0,.141)
md[160,4:12] <- c(0,0,287,144,0,0,.63,.274,.109)
md[161,4:12] <- c(0,0,222,0,0,0,.789,0,.136)
md[162,4:12] <- c(0,0,135,24,12,0,.577,0,.897)
md[163,4:12] <- c(0,0,259,0,0,0,.669,0,.302)
md[164,4:12] <- c(0,0,22,200,12,0,.437,.637,.106)
md[165,4:12] <- c(0,0,311,22,39,0,.363,.05,.096)
md[166,4:12] <- c(0,0,398,12,0,0,.441,.05,.144)
names(md)[3] <- "total"
md$total <- as.numeric(as.character(md$total))

md <- md %>% 
  mutate(amind = c_amind/total,
         asian = c_asian/total,
         black = c_black/total,
         hispanic = c_hispanic/total,
         white = c_white/total,
         multiple = c_multiple/total) %>% 
  select(School.Name,total,amind, asian, black, hispanic,white,
         multiple, p_frpm, p_lep, p_swd)
summary(md$black)
md_stan <- md %>% 
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
            m_frpm = mean(p_frpm),
            sd_frpm = sd(p_frpm),
            m_lep = mean(p_lep),
            sd_lep = sd(p_lep),
            m_swd = mean(p_swd),
            sd_swd = sd(p_swd))
md_stan$link = "A"
md_memb <- md %>% 
  filter(str_detect(School.Name,"City Neighbors")==TRUE|
         School.Name == "Baltimore Montessori Public Charter School")
md_memb$link = "A"
md_stan <- merge(md_memb, md_stan, by="link")
md_stan<- md_stan %>% 
  mutate(comp_amind = (amind - m_amind)/sd_amind,
         comp_asian = (asian - m_asian)/sd_asian,
         comp_black = (black - m_black)/sd_black,
         comp_hispanic = (hispanic - m_hispanic)/sd_hispanic,
         comp_white = (white - m_white)/sd_white,
         comp_multiple = (multiple - m_multiple)/sd_multiple,
         comp_frpm = (p_frpm - m_frpm)/sd_frpm,
         comp_lep = (p_lep - m_lep)/sd_lep,
         comp_swd = (p_swd - m_swd)/sd_swd) %>% 
  select(School.Name, total, amind, m_amind, comp_amind,
         asian, m_asian, comp_asian, black, m_black, comp_black,
         hispanic, m_hispanic, comp_hispanic, white, m_white, comp_white,
         multiple, m_multiple, comp_multiple, p_frpm, m_frpm, comp_frpm,
         p_lep, m_lep, comp_lep,p_swd,m_swd,comp_swd)
write.csv(md_stan, file = file.path("output_data/md_stan_enrl.csv"),row.names = FALSE)

#---- Math and ELA ----
# imported data from https://reportcard.msde.maryland.gov/DataDownloads/FileDownload/375
md_test <- read.csv("raw_data/md_MCAP_ELA_MATH_2019.csv")
str(md_test)
md_test<-md_test %>% 
  select(LSS.Name,School.Name,Assessment,Tested.Count,Proficient.Count,Proficient.Pct) %>% 
  filter(LSS.Name == "Baltimore City")
names(md_test) <- c("district","school","test","n_tested","n_prof","p_prof")
md_test$test <- as.factor(md_test$test)
levels(md_test$test) <- gsub("English/Language Arts","ela",levels(md_test$test))
levels(md_test$test) <- gsub("Mathematics","math",levels(md_test$test))
levels(md_test$test)[1] <- "math"
levels(md_test$test) <- gsub("Grade", "--",levels(md_test$test))

md_test <- md_test %>% 
  separate(test,c("subject","grade"),sep=" -- ",extra = "merge") %>% 
  mutate(n_tested = as.numeric(as.character(n_tested)),
         n_prof = as.numeric(gsub("[^0-9.-]", 0, as.character(n_prof)))) %>% 
  select(district, school, subject, n_tested, n_prof)
DT <- data.table(md_test)
md_test <- DT[, lapply(.SD, sum), by=list(school, district, subject)]
md_test <- md_test %>% 
  mutate(p_prof = n_prof/n_tested)

md_ela <- md_test %>% 
  filter(subject == "ela",
         str_detect(school,"City Neighbors")==TRUE|
         school =="Baltimore Montessori Public Charter School")
md_ela_stan <- md_test %>% 
  filter(subject == "ela",
         school != "All Baltimore City Schools") %>% 
  summarize(mean_ela = mean(p_prof),
            sd_ela = sd(p_prof))
md_ela_stan$link = "A"
md_ela$link = "A"
md_ela <- merge(md_ela, md_ela_stan, by="link")
md_ela <- md_ela %>% 
  mutate(comp_ela = (p_prof - mean_ela)/sd_ela) %>% 
  select(school, subject, n_tested, p_prof, mean_ela, comp_ela)

md_math <- md_test %>% 
  filter(subject == "math",
         str_detect(school,"City Neighbors")==TRUE|
           school =="Baltimore Montessori Public Charter School") %>% 
  mutate_if(is.numeric, replace_na, replace = 0)
md_math_stan <- md_test %>% 
  filter(subject == "math",
         school != "All Baltimore City Schools") %>% 
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  summarize(mean_math = mean(p_prof),
            sd_math = sd(p_prof))
md_math_stan$link = "A"
md_math$link = "A"
md_math <- merge(md_math, md_math_stan, by="link")
md_math <- md_math %>% 
  mutate(comp_math = (p_prof - mean_math)/sd_math) %>% 
  select(school, subject, n_tested, p_prof, mean_math, comp_math)

write.csv(md_ela, file = file.path("output_data/md_stan_ela.csv"), row.names = FALSE)
write.csv(md_math, file = file.path("output_data/md_stan_math.csv"), row.names = FALSE)
