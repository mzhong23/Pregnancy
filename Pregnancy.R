pregvisit <- read.csv("//ad.monash.edu/home/User070/mzho0033/Desktop/data/Pregnancy/Visit_20201208-050500.csv")

pregpt <- read.csv("//ad.monash.edu/home/User070/mzho0033/Desktop/data/Pregnancy/Patient_20201208-050500.csv")
pregpt <- pregpt %>% filter(!(pregpt$MC_DONALD_CLASIFF=="NOT_MS"))
pregpt <- pregpt %>% filter(!(pregpt$NMO_CLASSIFICATION=="DEFINITE_NMO"))
pregpt <- pregpt %>% select("PATIENT_ID",
                            "BIRTH_DATE",
                            "DEATH_DATE",
                            "COUNTRY",
                            "ETHNIC_ORIGIN",
                            "SYMPTOMS_DATE",
                            "MSCOURSE_1","MSCOURSE_2","MSCOURSE_3",
                            "CURRENT_DMT") 
pregpt <- pregpt %>% mutate(clinic = substr(pregpt$PATIENT_ID, start = 1, stop = 6))
AH_pregpt <- pregpt %>% filter(clinic=="AU-025")
EH_pregpt <- pregpt %>% filter(clinic=="AU-017")
JHH_pregpt <- pregpt %>% filter(clinic=="AU-011")
pregpt <- full_join(AH_pregpt, EH_pregpt)
pregpt <- full_join(pregpt, JHH_pregpt)

pregdata <- read.csv("//ad.monash.edu/home/User070/mzho0033/Desktop/data/Pregnancy/Pregnancy_20201208-050500.csv")
pregdata <- pregdata %>% select("PatientCode",
                                "DateLastMenstruation",
                                "ChildrenBornFromPregnancy",
                                "EstDeliveryDate")
pregdata <- pregdata %>% rename(PATIENT_ID = "PatientCode") 

library(readxl)
AH_pregcap <- read_excel("//ad.monash.edu/home/User070/mzho0033/Desktop/data/Pregnancy/TheGeneticsOfMS_gender_list_18Dec2020.xlsx")
AH_pregcap <- AH_pregcap %>% filter(Sex=="Female") %>% select("Record ID",
                                                        "Affection Status",
                                                        "Date of Blood Collection",
                                                        "Enter the total number of children",
                                                        "Date of birth of child 1",
                                                        "Date of birth of child 2",
                                                        "Date of birth of child 3",
                                                        "Date of birth of child 4",
                                                        "Date of birth of child 5",
                                                        "Date of birth of child 6",
                                                        "Date of birth of child 7",
                                                        "Date of birth of child 8",
                                                        "Ever had a pregnancy",
                                                        "Age at first menstural period",
                                                        "Ever smoker",
                                                        "Average number cigarettes smoked per day",
                                                        "Year started smoking",
                                                        "Total years smoking",
                                                        "Pack Years",
                                                        "BMI",
                                                        "Mild/ Incidental physical activity in the last 12 months-duration", 
                                                        "Mild/Incidental physical activity in the last 12 months- frequency",
                                                        "Moderate physical activity in the last 12 months-duration",         
                                                        "Moderate physical activity in the last 12 months-frequency",     
                                                        "Complete?")
AH_pregcap <- AH_pregcap %>% rename(PATIENT_ID = "Record ID") 
AH_pregcap <- mutate(AH_pregcap, PATIENT_ID=replace(PATIENT_ID, PATIENT_ID=="AU-0251745", "AU-025-1745"))
AH_pregcap <- mutate(AH_pregcap, PATIENT_ID=replace(PATIENT_ID, PATIENT_ID=="Au-025-0036", "AU-025-0036"))
AH_pregcap <- mutate(AH_pregcap, PATIENT_ID=replace(PATIENT_ID, PATIENT_ID=="Au-025-0273", "AU-025-0273"))

AH_pregcap_control <- AH_pregcap %>% filter(`Affection Status`=="CONTROL")
AH_pregcap <- AH_pregcap %>% filter(!(PATIENT_ID %in% AH_pregcap_control$PATIENT_ID))
AH_pregcap <- AH_pregcap %>% mutate(clinic = substr(pregcap$PATIENT_ID, start = 1, stop = 6))

AH_torecruit <- AH_pregpt %>% filter(!(PATIENT_ID %in% AH_pregcap$PATIENT_ID))

survey <- read.csv("//ad.monash.edu/home/User070/mzho0033/Desktop/data/Pregnancy/Pregnancy, hormones and smoking.csv")
survey <- survey %>% rename(PATIENT_ID = "MSBase.ID") 

AH_nosurvey_nogeno <- AH_torecruit %>% filter(!(PATIENT_ID %in% survey$PATIENT_ID))
AH_survey_nogeno <- AH_torecruit %>% filter(PATIENT_ID %in% survey$PATIENT_ID)



# Counting n with male pregnancies
male <- survey %>% filter(PATIENT_ID %in% AH_survey_geno$PATIENT_ID)
male$Sex.of.baby <- as.character(a$Sex.of.baby)
b <- male %>% filter(Sex.of.baby =="M")
c <- male %>% filter(Sex.of.baby =="F, M")
male <- full_join(b,c)
rm(b,c)
table(duplicated(male$PATIENT_ID))


# smoker
smoker <- AH_survey_geno %>% filter(PATIENT_ID %in% survey$PATIENT_ID)
table(smoker$`Ever smoker`)
rm(smoker)

#demographics
a <- AH_survey_geno %>% select(PATIENT_ID, `Ever had a pregnancy`, `Ever smoker`, `Date of Blood Collection`)
b <- pregpt %>% select(PATIENT_ID, BIRTH_DATE, SYMPTOMS_DATE, DEATH_DATE, clinic, CURRENT_DMT)
a <- left_join(a,b)
rm(b)
a$BIRTH_DATE <- as.Date(a$BIRTH_DATE,"%d.%m.%Y")
a$`Date of Blood Collection` <- as.Date(a$`Date of Blood Collection`, format= "%Y/%m/%d")
a$SYMPTOMS_DATE <- as.Date(a$SYMPTOMS_DATE, "%d.%m.%Y")
a$age_bleed <- (a$`Date of Blood Collection` - as.Date(a$BIRTH_DATE))/365.25
mean(a$age_bleed, na.rm=TRUE)
range(a$age_bleed, na.rm=TRUE)

a$age_Dx <- (a$SYMPTOMS_DATE - a$BIRTH_DATE)/365.25
mean(a$age_Dx, na.rm=TRUE)
range(a$age_Dx, na.rm=TRUE)
summary(as.numeric(a$age_Dx), na.rm=TRUE)
a <- a %>% select(!SYMPTOMS_DATE)

parous1 <- a %>% filter(PATIENT_ID %in% parous$PATIENT_ID)
summary(as.numeric(parous1$age_Dx), na.rm=TRUE)

parous <- left_join(parous, a)
mean(parous$age_bleed, na.rm=TRUE)
range(parous$age_bleed, na.rm=TRUE)

demo <- a %>% mutate(preg_bin = ifelse(PATIENT_ID %in% parous$PATIENT_ID, 1, 0))
nullip <- demo %>% filter(!(PATIENT_ID %in% parous$PATIENT_ID))


# Any Preg pre-Dx #
# First preg before symptoms date 
preg_preDx <- pregdata %>% filter(PATIENT_ID %in% temp$PATIENT_ID)
preg_preDx <- preg_preDx %>% filter(ChildrenBornFromPregnancy>0)
g <- parous %>% select(PATIENT_ID, SYMPTOMS_DATE)
preg_preDx <- left_join(preg_preDx, g)
rm(g)
preg_preDx <- preg_preDx %>% filter(DateLastMenstruation < SYMPTOMS_DATE)
ls(preg_preDx)

# age at last pregnancy
atlastpreg <- pregdata %>% filter(PATIENT_ID %in% AH_survey_geno$PATIENT_ID)
atlastpreg <- atlastpreg %>% group_by(PATIENT_ID) %>% arrange(desc(DateLastMenstruation)) %>% slice(1L)
atlastpreg <- atlastpreg %>% filter(!is.na(ChildrenBornFromPregnancy))
b <- pregpt %>% select(PATIENT_ID, BIRTH_DATE)
atlastpreg <- left_join(atlastpreg,b)
atlastpreg$BIRTH_DATE <- as.Date(atlastpreg$BIRTH_DATE,"%d.%m.%Y")
atlastpreg$DateLastMenstruation <- as.Date(atlastpreg$DateLastMenstruation, format = "%d.%m.%Y")
atlastpreg$age_lastpreg <- (atlastpreg$DateLastMenstruation - atlastpreg$BIRTH_DATE)/365.25
mean(atlastpreg$age_lastpreg)
range(atlastpreg$age_lastpreg)
rm(b)
summary(as.numeric(atlastpreg$age_lastpreg))

# edss at pregnancy
a <- pregdata %>% filter(PATIENT_ID %in% AH_survey_geno$PATIENT_ID)
a <- a %>% group_by(PATIENT_ID) %>% arrange(desc(DateLastMenstruation)) %>% slice(1L)
a <- a %>% filter(!is.na(ChildrenBornFromPregnancy))
b <- pregvisit %>% select(PATIENT_ID, DATE_OF_VISIT, EDSS, MSCOURSE_AT_VISIT)
b <- b %>% rename(MSCOURSE_preg = MSCOURSE_AT_VISIT) 
a <- left_join(a, b)
a <- a %>% filter(!is.na(EDSS))
a$DateLastMenstruation <- as.Date(a$DateLastMenstruation, format = "%d.%m.%Y")
a$DATE_OF_VISIT <- as.Date(a$DATE_OF_VISIT, format= "%d.%m.%Y")
b <- a %>% filter(as.Date(DATE_OF_VISIT) > (as.Date(DateLastMenstruation)-30.5))
b <- b %>% group_by(PATIENT_ID) %>% arrange(DATE_OF_VISIT) %>% slice(1L)
c <- a %>% filter(!(PATIENT_ID %in% b$PATIENT_ID))
c <- c %>% group_by(PATIENT_ID) %>% arrange(desc(DATE_OF_VISIT)) %>% slice(1L)
parous <- full_join(b,c)
median(parous$EDSS)
range(parous$EDSS)
mean(parous$EDSS)

parous <- left_join(parous,pregpt)
parous$BIRTH_DATE <- as.Date(parous$BIRTH_DATE,"%d.%m.%Y")
parous$age_lastpreg <- (parous$DateLastMenstruation - parous$BIRTH_DATE)/365.25
mean(parous$age_lastpreg)
range(parous$age_lastpreg)


# rm (a,b,c,parous, temp)


# edss at bleed 
a <- AH_survey_geno %>% select(PATIENT_ID, `Date of Blood Collection`)
b <- pregvisit %>% select(PATIENT_ID, DATE_OF_VISIT, EDSS, MSCOURSE_AT_VISIT)
a <- left_join(a, b)
a <- a %>% filter(!is.na(EDSS))
a$`Date of Blood Collection` <- as.Date(a$`Date of Blood Collection`, format= "%Y/%m/%d")
a$DATE_OF_VISIT <- as.Date(a$DATE_OF_VISIT, format= "%d.%m.%Y")

b <- a %>% filter(as.Date(DATE_OF_VISIT) > (as.Date(`Date of Blood Collection`)-30.5))
b <- b %>% group_by(PATIENT_ID) %>% arrange(DATE_OF_VISIT) %>% slice(1L)
c <- a %>% filter(!(PATIENT_ID %in% b$PATIENT_ID))
c <- c %>% group_by(PATIENT_ID) %>% arrange(desc(DATE_OF_VISIT)) %>% slice(1L)
d <- full_join(b,c)
d <- d %>% rename(MSCOURSE_bleed = MSCOURSE_AT_VISIT) 
median(d$EDSS)
range(d$EDSS)
mean(d$EDSS)
#parous subgroup
e <- d %>% filter(PATIENT_ID %in% parous$PATIENT_ID)
median(e$EDSS)
range(e$EDSS)
mean(e$EDSS)
rm (a,b,c,d)
# nullip
nullip <- left_join(nullip, d)




#### EASTERN ####

genotyped <- read_excel("//ad.monash.edu/home/User070/mzho0033/Desktop/All_Genotyped_MSGP.xlsx")
genotyped$clinic_code <- substr(genotyped$PATIENT_ID,1,6)
EH_survey <- read.csv("//ad.monash.edu/home/User070/mzho0033/Desktop/data/Pregnancy/Eastern Health Data/survey IDs.csv", sep="")
EH_survey_nogeno <- EH_survey %>% filter(!(PATIENT_ID %in% genotyped$PATIENT_ID))
EH_nosurvey_geno <- genotyped %>% filter(!(PATIENT_ID %in% EH_survey$PATIENT_ID)) %>% filter(clinic_code=="AU-017")
EH_survey_geno <- EH_survey %>% filter(PATIENT_ID %in% genotyped$PATIENT_ID) %>% filter(PATIENT_ID %in% EH_pregpt)

EH_nosurvey_nogeno <- EH_pregpt %>% filter(!(PATIENT_ID %in% EH_survey$PATIENT_ID))
EH_nosurvey_nogeno <- EH_nosurvey_nogeno %>% filter(!(PATIENT_ID %in% genotyped$PATIENT_ID))

a<- EH_survey_nogeno %>% filter(PATIENT_ID %in% EH_pregpt$PATIENT_ID)


#### JHH ####
JHH_survey_geno <- JHH_pregpt %>% filter((PATIENT_ID %in% genotyped$PATIENT_ID))
JHH_pregpt <- JHH_pregpt %>% filter(PATIENT_ID %in% pregpt$PATIENT_ID)







#### ARMSS ####
install.packages("ms.sev")
library(ms.sev)
help(ms.sev)
armss <- left_join(d, demo)
armss <- armss %>% select(PATIENT_ID, EDSS, age_bleed)
armss <- armss %>% rename(edss = EDSS) 
armss <- armss %>% rename(ageatedss = age_bleed) 
armss <- as.data.frame(global_armss(armss))
armss <- armss %>% rename(PATIENT_ID = data.PATIENT_ID) 
armss <- armss %>% rename(bleed_ARMSS = data.gARMSS) 
armss <- armss %>% rename(EDSS = data.edss) 
armss <- armss %>% select(PATIENT_ID, bleed_ARMSS, EDSS)
armss <- left_join(demo, armss)
summary(lm(armss$bleed_ARMSS~armss$preg_bin))
mean(armss$bleed_ARMSS, na.rm=TRUE)

armss_p <- parous %>% select(PATIENT_ID, EDSS, age_lastpreg)
armss_p <- armss_p %>% rename(edss = EDSS) 
armss_p <- armss_p %>% rename(ageatedss = age_lastpreg) 
armss_p <- as.data.frame(global_armss(armss_p))
armss_p <- armss_p %>% rename(PATIENT_ID = data.PATIENT_ID) 
armss_p <- armss_p %>% rename(preg_ARMSS = data.gARMSS) 
armss_p <- left_join(armss_p, demo)
armss_p <- armss_p %>% rename(EDSS = data.edss) 

summary(armss_p$EDSS)
summary(armss_p$preg_ARMSS)

ls(demo)


#### MSSS ####
library(ms.sev)
msss <- left_join(d, demo)
msss$dd <- (msss$`Date of Blood Collection` - msss$SYMPTOMS_DATE)/365.25
msss <- msss %>% select(PATIENT_ID, EDSS, age_bleed, dd)
msss <- msss %>% rename(edss = EDSS) 
msss <- as.data.frame(global_msss(msss))
msss <- msss %>% rename(PATIENT_ID = data.PATIENT_ID) 
msss <- msss %>% rename(bleed_MSSS = data.uGMSSS) 
msss <- msss %>% rename(EDSS = data.edss) 
msss <- msss %>% select(PATIENT_ID, bleed_MSSS, EDSS)
msss <- left_join(demo, msss) %>% select(!SYMPTOMS_DATE)
summary(lm(msss$bleed_ARMSS~msss$preg_bin))
mean(msss$bleed_msss, na.rm=TRUE)

msss_p <- parous 
msss_p$dd <- (msss_p$DateLastMenstruation - msss_p$BIRTH_DATE)/365.25
msss_p <- msss_p %>% select(PATIENT_ID, EDSS, dd)
msss_p <- msss_p %>% rename(edss = EDSS) 
msss_p <- as.data.frame(global_msss(msss_p))
msss_p <- msss_p %>% rename(PATIENT_ID = data.PATIENT_ID) 
msss_p <- msss_p %>% rename(preg_msss = data.uGMSSS) 
msss_p <- left_join(msss_p, demo)
msss_p <- msss_p %>% rename(EDSS = data.edss) 
median(msss_p$preg_msss)

ls(parous)
summary(msss_p$EDSS)
summary(msss_p$preg_msss)


#### POWER ####
install.packages("WebPower")
library(WebPower)
# effect size d = delta(X)/sd
# sd for EDSS 2.2214
wp.regression(n = NULL, p1 = 4, f2 = 0.22508, alpha = 0.05, power = 0.8)

# f2 = R^2/(1-R^2) .... Where R2 is the squared multiple correlation
summary(lm(AH_pregYN$EDSS~AH_pregYN$preg_bin))
# adjusted R2 from lm = 0.01301 (EDSS ~ parity)
summary(lm(temp$EDSS~temp$preg_bin)) #0.06945 
0.06945 /(1-0.06945 )


#R
cor(temp$EDSS,temp$preg_bin)
#R2 = 0.09331381
#f2
0.003/(1-0.003)
sqrt(0.003))
0.012/(1-0.012)
pwr.f2.test(u = 1, v = NULL, f2 = 0.01214575, sig.level = 0.05, power = 0.8)

# calculate f2 (R^2/(1-R^2)) where R = cor(y, x)

install.packages("pwr")
library(pwr)
pwr.f2.test(u = 1, v = NULL, f2 = 0.1029174, sig.level = 0.05, power = 0.8)
pwr.f2.test(u = 4, v = 200, f2 = 0.1029174, sig.level = 0.05, power = NULL)
pwr.f2.test(u = 1, v = 2000, f2 = 0.003009027, sig.level = 0.05, power = NULL)

# WHAT IS F2 - EFFECT SIZE 
# NOT THE SAME AS COHEN'S D - effect size = delta(X)/sd
# f2 = R^2/(1-R^2) .... Where R2 is the squared multiple correlation
# TRIED PARITY VS EDSS, R2 
# adjusted R2 from lm = 0.01301  
# CAN"T REALLY CALCULATE FROM DATA ALONE

# install.packages("remotes")
# remotes::install_github("statapps/power.ctepd")
# library(remotes)
# power.rq.test(x, n = 200, sig.level = 0.05, power = NULL, 
#               tau = 0.5, beta = 1, sd = 1, dist = "Norm", kernel.smooth = "norm", 
#               bw = NULL, alternative = c("two.sided", "one.sided"))
# 
# install.packages("devtools")
# library(devtools)
# install_github("statapps/power.ctepd")

summary(lm(bleed_ARMSS~preg_bin+age_Dx+age_bleed, data=temp_np))

ls(temp_p)
temp_p <- left_join(temp_p, demo)
temp_n <- left_join(temp_n, demo)
temp_np <- full_join(temp_n, temp_p)
temp_np <- left_join(temp_np, data)


0.003
r2 0.0027
cor 0.050
