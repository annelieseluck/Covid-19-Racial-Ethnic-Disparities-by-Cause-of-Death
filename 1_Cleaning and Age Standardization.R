
# packages
library(plyr)
library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)
library(haven)
library(data.table)
library(readxl)
library(stringr)

#-----------------------------------------------------------------------------------------------------------------
# load data
## cause of death data, 2019-2020, single race coded
cod_20 <- read.csv("Raw Data/AH_Monthly_Provisional_Counts_of_Deaths_for_Select_Causes_of_Death_by_Sex__Age__and_Race_and_Hispanic_Origin_NEW.csv")

## population data, 2019-2020, single race coded
pop_19 <- read.csv("Raw Data/Census_July_Dec_2019_Monthly_Pop.csv") %>% subset(MONTH == 7 & UNIVERSE == "R" & AGE != 999)
pop_20 <- read.csv("Raw Data/Census_July_Dec_2020_Monthly_Pop.csv") %>% subset(MONTH == 7 & UNIVERSE == "R" & AGE != 999)

## bind population years together and select race/ethnicities of interes
pops <- rbind(pop_19, pop_20) %>% select(c(MONTH:TOT_FEMALE, NHWA_MALE:NHBA_FEMALE, H_MALE:H_FEMALE)) %>% gather(TOT_MALE:H_FEMALE, key="identifier", value="pop") %>% select(-c(TOT_POP))

#-----------------------------------------------------------------------------------------------------------------
# CLEANING POPULATION DATA
## create variable for 10 year age groups, race/ethnicity, and sex that align with NCHS files
midyrpop_clean <- pops %>% dplyr::mutate(
  Sex = ifelse(identifier == "TOT_MALE" | identifier == "NHWA_MALE" |  identifier == "NHBA_MALE" |  identifier ==  "H_MALE", "Male", "Female"),
  AgeGroup = ifelse(AGE < 5 , "0-4 years",
                    ifelse(AGE >= 5 & AGE < 15 , "05-14 years",
                           ifelse(AGE >= 15 & AGE < 25, "15-24 years",
                                  ifelse(AGE >= 25 & AGE < 35, "25-34 years",
                                         ifelse(AGE >= 35 & AGE < 45, "35-44 years",
                                                ifelse(AGE >= 45 & AGE < 55,  "45-54 years",
                                                       ifelse(AGE >= 55 & AGE < 65, "55-64 years",
                                                              ifelse(AGE >= 65 & AGE < 75, "65-74 years",
                                                                     ifelse(AGE >= 75 & AGE < 85, "75-84 years",
                                                                            ifelse(AGE >= 85, "85 years and over", NA)))))))))),
  Race.Ethnicity = 
    ifelse(identifier=="NHWA_MALE" | identifier == "NHWA_FEMALE", "Non-Hispanic White",
           ifelse(identifier=="NHBA_MALE" | identifier == "NHBA_FEMALE", "Non-Hispanic Black",
                  ifelse(identifier=="H_MALE" | identifier == "H_FEMALE", "Hispanic", "All")))) 

## aggregate single-year age groups to ten-year age groups
pop_race_sums <- midyrpop_clean %>% 
  group_by(YEAR, AgeGroup, Race.Ethnicity, Sex) %>% 
  dplyr::summarise(pop = sum(pop))

## aggregate across sexes 
pop_race_bothsexes <- midyrpop_clean %>%  
  group_by(YEAR, AgeGroup, Race.Ethnicity,) %>% 
  dplyr::summarize(pop = sum(pop, na.rm = T)) %>% 
  mutate(Sex = "Both") %>% 
  select(c(YEAR, AgeGroup, Sex, Race.Ethnicity, pop))
pop_clean <- rbind(pop_race_sums, pop_race_bothsexes) 

# Derive midyear population age distribuitons in 2016 for standardization
distributions <- pop_clean %>% subset(YEAR == 2020 & Race.Ethnicity == "All" & Sex == "Both")

## 25+ distribution
pop_25plus <- distributions %>% subset(AgeGroup != "0-4 years" & AgeGroup != "05-14 years" & AgeGroup != "15-24 years") 
total_25plus <- sum(pop_25plus$pop)
distr_25plus <- pop_25plus %>% 
  mutate(total_25plus = total_25plus,
         prop_25plus = pop/total_25plus) %>% 
  ungroup() %>% 
  select(c(AgeGroup, prop_25plus))
distr_25plus
sum(distr_25plus$prop_25plus)

## 25-64 distribution
pop_25_64 <- distributions %>% subset(AgeGroup != "0-4 years" & AgeGroup != "05-14 years" & AgeGroup != "15-24 years" & 
                                        AgeGroup != "65-74 years" & AgeGroup != "75-84 years" & AgeGroup != "85 years and over") 
total_25_64 <- sum(pop_25_64$pop)
distr_25_64 <- pop_25_64 %>% 
  mutate(total_25_64 = total_25_64,
         prop_25_64 = pop/total_25_64) %>% 
  ungroup() %>% 
  select(c(AgeGroup, prop_25_64))
distr_25_64
sum(distr_25_64$prop_25_64)

## 65+ distribution
pop_65plus <- distributions %>% subset(AgeGroup == "65-74 years" | AgeGroup == "75-84 years" | AgeGroup == "85 years and over") 
total_65plus <- sum(pop_65plus$pop)
distr_65plus <- pop_65plus %>% 
  mutate(total_65plus = total_65plus,
         prop_65plus = pop/total_65plus) %>% 
  ungroup() %>% 
  select(c(AgeGroup, prop_65plus))
distr_65plus
sum(distr_65plus$prop_65plus)

## compile age distributions for varying standardization groups
distributions <- merge(distr_25plus, distr_25_64, by=c("AgeGroup"), all.x=T)
distributions <- merge(distribs, distr_65plus, by=c("AgeGroup"), all.x=T)
distributions

#-----------------------------------------------------------------------------------------------------------------
# CLEANING MORTALITY DATA
## recode age group and sex to align with population, created external cause category (All Cause - Natural Cause)
cod_clean <- cod_20 %>% subset(Date.Of.Death.Year == 2020 | Date.Of.Death.Year == 2019) %>% 
  dplyr::mutate(AgeGroup = recode(AgeGroup, "5-14 years" = "05-14 years"),
                ExternalCause = AllCause - NaturalCause,
                Sex = ifelse(Sex == "F", "Female", ifelse(Sex == "M", "Male", Sex))) %>% 
  dplyr::rename(., YEAR = Date.Of.Death.Year)

## aggregate monthly death counts to annual death counts 
### for male/female
cod_race <- cod_clean %>%  
  gather(., "AllCause":"ExternalCause", key = "condition", value="condition_count") %>% 
  group_by(YEAR, AgeGroup, Sex, Race.Ethnicity, condition) %>% 
  dplyr::summarize(deaths = sum(condition_count, na.rm = T))
### across sex
cod_race_bothsexes <- cod_clean %>%  
  gather(., "AllCause":"ExternalCause", key = "condition", value="condition_count") %>% 
  group_by(YEAR, AgeGroup, Race.Ethnicity, condition) %>% 
  dplyr::summarize(deaths = sum(condition_count, na.rm = T)) %>% 
  mutate(Sex = "Both") %>% 
  select(c(YEAR, AgeGroup, Sex, Race.Ethnicity, condition, deaths))
cod_race_total <- rbind(cod_race, cod_race_bothsexes)

## aggregate across race.ethnicity to total popualtion counts
### for male/female
cod_all <- cod_clean %>%  
  gather(., "AllCause":"ExternalCause", key = "condition", value="condition_count") %>% 
  group_by(YEAR, AgeGroup, Sex, condition) %>% 
  dplyr::summarize(deaths = sum(condition_count, na.rm = T)) %>% 
  mutate(Race.Ethnicity = "All")%>% 
  select(c(YEAR, AgeGroup, Sex, Race.Ethnicity, condition, deaths))
### across sex
cod_all_bothsexes <- cod_clean %>%  
  gather(., "AllCause":"ExternalCause", key = "condition", value="condition_count") %>% 
  group_by(YEAR, AgeGroup, condition) %>% 
  dplyr::summarize(deaths = sum(condition_count, na.rm = T)) %>% 
  mutate(Sex = "Both", Race.Ethnicity = "All") %>% 
  select(c(YEAR, AgeGroup, Sex, Race.Ethnicity, condition, deaths))
cod_all_total <- rbind(cod_all, cod_all_bothsexes)

## merge together
cod_total <- rbind(cod_race_total, cod_all_total)


## create a residual death category (All Cause - All Non Covid)
allcause <- cod_total %>% subset(condition == "AllCause") %>% select(-c(condition)) ## all causes
othercause <- cod_total %>%  ## identify all other noncovid causes
  subset(condition != "AllCause" & condition != "COVID.19..U071..Multiple.Cause.of.Death." & condition != "NaturalCause" & condition != "<NA>") %>% 
  group_by(YEAR, AgeGroup, Race.Ethnicity, Sex) %>% 
  dplyr::summarise(othercause_deaths = sum(deaths))
residual <- merge(othercause, allcause, by=c("YEAR", "AgeGroup", "Race.Ethnicity", "Sex")) %>% ## calculate residual (AllCause - All Other NonCovid Causes)
  mutate(Unknown = deaths - othercause_deaths) %>% 
  select(-c(othercause_deaths:deaths)) %>% 
  gather(., "Unknown", key = "condition", value="deaths") 
cod_total <- rbind(cod_total, residual) # append to death file

## create an all non-Covid deaths (All Cause - Covid)
covidcause <- cod_total %>% subset(condition == "COVID.19..U071..Underlying.Cause.of.Death.") %>% select(-c(condition)) %>% rename(., coviddeaths = deaths) ## covid 
allcause <- cod_total %>% subset(condition == "AllCause") %>% select(-c(condition)) ##all cause
noncovidcause <- merge(covidcause, allcause, by=c("YEAR", "AgeGroup", "Race.Ethnicity", "Sex"), all=T) %>% ## calculate noncovid cause
  mutate_at(vars("coviddeaths"), ~replace(., is.na(.), 0)) %>% 
  mutate(nonCovid = deaths - coviddeaths) %>% 
  select(-c(coviddeaths:deaths)) %>% 
  gather(., "nonCovid", key = "condition", value="deaths") 
cod_total <- rbind(cod_total, noncovidcause)# append to death file

#-----------------------------------------------------------------------------------------------------------------
# MERGE POPULATIONS AND DEATHS
data_merged <- merge(cod_total, pop_clean, by=c("YEAR", "AgeGroup", "Sex", "Race.Ethnicity"), all = T)

## restrict to  working age  (25-64) and race.ethnicities of interest 
#merge stratified cod and population data together and restrict data to what is needed for analysis (exclude <25, Covid multiple case of deaths, other conditions, natural cause, cods race/ethnicity, and any not state rate/ethnicity or sex)
data_merged <- data_merged %>% 
  subset(AgeGroup != "0-4 years" & AgeGroup != "05-14 years" & AgeGroup != "15-24 years" & !is.na(AgeGroup) &  ## working age
           !is.na(Race.Ethnicity) & Race.Ethnicity != "Other" & Race.Ethnicity != "Non-Hispanic American Indian or Alaska Native" &Race.Ethnicity != "Non-Hispanic Asian" & ## race.ethncity == all, white, black, and hispanic
           !is.na(Sex) &  ## sex = both, male, and female
           condition != "COVID.19..U071..Multiple.Cause.of.Death." & condition != "NaturalCause" & condition != "<NA>")  ## causes =  13 causes of interest, includign Covid as underlying 

#-----------------------------------------------------------------------------------------------------------------
# CRUDE DEATH RATES
## calculate age-specific death rates by age
CDR <- data_merged %>% 
  mutate(CDR = 100000*(deaths/pop))

## calculate CDR change 
CDRchange <- CDR %>% 
  select(c(YEAR, AgeGroup, Race.Ethnicity, Sex, condition, CDR)) %>%  
  spread(YEAR, CDR) %>% 
  mutate_at(vars(`2019`:`2020`), ~replace(., is.na(.), 0)) %>% 
  mutate(CDR_netchange = `2020` - `2019`,
         CDR_pctchange = (`2020`-`2019`)/`2019`) %>% 
  mutate(YEAR = 2020) %>% 
  select(-c(`2019`, `2020`))

## append CDR change to CDRs
CDR_yoy <- merge(CDR, CDRchange, by=c("YEAR", "AgeGroup", "Race.Ethnicity", "Sex", "condition"), all=T) 

#-----------------------------------------------------------------------------------------------------------------
# AGE STANDARDIZED DEATH RATES
## merge in mid year age distributions for standardization
ASDR <- merge(CDR, distributions, by=c("AgeGroup"), all.x=TRUE)

##  calculate age-standardized rates 
ASDR <- ASDR %>%   ## multiply age specific CDR by age proportion
  mutate(ASDR_25plus = CDR * prop_25plus,
         ASDR_25_64 = CDR * prop_25_64,
         ASDR_65plus = CDR * prop_65plus)
ASDR_clean <- ASDR %>%  ## aggregate across age groups to ASDR
  group_by(YEAR, Sex, Race.Ethnicity, condition) %>% 
  dplyr::summarize(deaths = sum(deaths),
                   pop = sum(pop),
                   ASDR_25plus = sum(ASDR_25plus, na.rm = T),
                   ASDR_25_64 = sum(ASDR_25_64, na.rm = T),
                   ASDR_65plus = sum(ASDR_65plus, na.rm = T)) 

## make ASDR data file long format and restrict to race.ethnicity of interest
ASDR_merged <- ASDR_clean %>% select(-c(deaths, pop)) %>% distinct() %>% 
  gather(., "ASDR_25plus":"ASDR_65plus", key = "AgeGroup", value="ASDR") %>% 
  mutate(AgeGroup = ifelse(AgeGroup == "ASDR_25plus", "25+",
                           ifelse(AgeGroup == "ASDR_25_64", "25-64", "65+"))) %>% 
  subset(Race.Ethnicity != "Non-Hispanic American Indian or Alaska Native" &Race.Ethnicity != "Non-Hispanic Asian") # subset to race.ethnicity = All, white, black, and hispanic


#-----------------------------------------------------------------------------------------------------------------
# TABLE 1 
## format death, pop, and ASDR counts and merge into table
counts_summary <- data_merged %>% 
  group_by(YEAR, Race.Ethnicity, Sex, condition) %>% 
  dplyr::summarise(
    deaths = sum(deaths),
    pop = sum(pop))
## death counts
deaths_summary <- counts_summary %>% subset(condition=="AllCause" ) %>% select(-c(pop)) %>% spread(YEAR, deaths) %>% 
  dplyr::mutate(Change_Absolute = `2020` - `2019`,
                Change_Percent = Change_Absolute / `2019`) %>% 
  gather(key="measure", value="value", `2019`:Change_Percent) %>% 
  spread(Sex, value) %>% select(c(Race.Ethnicity, measure, Male, Female, Both)) %>% 
  dplyr::rename(Male_deaths = Male, 
                Female_deaths = Female,
                Both_deaths = Both)
## pop counts
pop_summary <- counts_summary %>% subset(condition=="AllCause") %>% select(-c(deaths)) %>% spread(YEAR, pop) %>% 
  dplyr::mutate(Change_Absolute = `2020` - `2019`,
                Change_Percent = Change_Absolute / `2019`) %>% 
  gather(key="measure", value="value", `2019`:Change_Percent) %>% 
  spread(Sex, value) %>% select(c(Race.Ethnicity, measure, Male, Female, Both)) %>% 
  dplyr::rename(Male_pop = Male, 
                Female_pop = Female,
                Both_pop = Both)
## asdr 
asdr_summmary <- ASDR_merged %>% subset(condition=="AllCause" & AgeGroup=="25+") %>% spread(YEAR, ASDR)  %>% 
  dplyr::mutate(Change_Absolute = `2020` - `2019`,
                Change_Percent = Change_Absolute / `2019`) %>% 
  gather(key="measure", value="value", `2019`:Change_Percent) %>% 
  spread(Sex, value) %>% select(c(Race.Ethnicity, measure, Male, Female, Both)) %>% 
  dplyr::rename(Male_ASDR = Male, 
                Female_ASDR = Female,
                Both_ASDR = Both)
## merge into table 1
table1 <- merge(deaths_summary, pop_summary, by=c("Race.Ethnicity", "measure"))
table1 <- merge(table1, asdr_summmary, by=c("Race.Ethnicity", "measure"))
table1$Race.Ethnicity <- factor(table1$Race.Ethnicity, levels=c("All", "Non-Hispanic White", "Non-Hispanic Black", "Hispanic"))
table1$measure <- factor(table1$measure, levels=c("2020", "2019", "Change_Absolute", "Change_Percent"))
table1

## write out table 1
write.csv(table1, "Clean Data/Table1_SummaryCounts.csv")


#-----------------------------------------------------------------------------------------------------------------

# ASDR TABLES (TABLE 2 + ALL APPENDIX TABLES)
## format all race.ethnicity ASDRs and merge into table
table_working <- ASDR_merged %>% spread(YEAR, ASDR)  %>% 
  dplyr::mutate(Change_Absolute = `2020` - `2019`,
                Change_Percent = Change_Absolute / `2019`) 
## total pop
all <- table_working %>% subset(Race.Ethnicity == "All") %>%   
  dplyr::rename(All_2019 = `2019`,  
                All_2020 = `2020`,
                All_AbsChange = Change_Absolute,
                All_PctChange = Change_Percent) %>% 
  ungroup() %>% select(-c(Race.Ethnicity))
## white pop
white <- table_working %>% subset(Race.Ethnicity == "Non-Hispanic White") %>%   
  dplyr::rename(white_2019 = `2019`,  
                white_2020 = `2020`,
                white_AbsChange = Change_Absolute,
                white_PctChange = Change_Percent) %>% 
  ungroup() %>% select(-c(Race.Ethnicity))
## black pop
black <- table_working %>% subset(Race.Ethnicity == "Non-Hispanic Black") %>%   
  dplyr::rename(black_2019 = `2019`,  
                black_2020 = `2020`,
                black_AbsChange = Change_Absolute,
                black_PctChange = Change_Percent) %>% 
  ungroup() %>% select(-c(Race.Ethnicity))
## hispanic pop
hisp <- table_working %>% subset(Race.Ethnicity == "Hispanic") %>%   
  dplyr::rename(hisp_2019 = `2019`,  
                hisp_2020 = `2020`,
                hisp_AbsChange = Change_Absolute,
                hisp_PctChange = Change_Percent) %>% 
  ungroup() %>% select(-c(Race.Ethnicity))

ASDR_tables <- merge(all, white, by=c("Sex", "AgeGroup", "condition"))
ASDR_tables <- merge(ASDR_tables, black, by=c("Sex", "AgeGroup", "condition"))
ASDR_tables <- merge(ASDR_tables, hisp, by=c("Sex", "AgeGroup", "condition")) %>% ungroup() %>% 
  dplyr::mutate(condition = ifelse(condition=="AllCause", "1.AllCause",
                                   ifelse(condition=="COVID.19..U071..Underlying.Cause.of.Death.", "2.Covid",
                                          ifelse(condition=="nonCovid", "3.NonCovid", condition))))
ASDR_tables <- ASDR_tables %>% arrange(Sex, AgeGroup, condition)

write.csv(ASDR_tables, "Clean Data/All_ASDR_Tables.csv")


## can filter for table of interest
# for example: TABLE 2: 
table2 <- ASDR_tables %>% subset(Sex=="Both" & AgeGroup=="25+")
write.csv(table2, "Clean Data/Table2_ASDRchange_25plusBothSexes.csv")

#-----------------------------------------------------------------------------------------------------------------
# TABLE 3: DECOMPOSITION OF MORTALITY CHANGES BY CAUSE
## calculate change from 2019-2020
ASDR_YOY <- ASDR_merged %>% 
  subset(Race.Ethnicity!="Non-Hispanic American Indian or Alaska Native" & Race.Ethnicity != "Non-Hispanic Asian") %>% 
  spread(YEAR, ASDR)  %>% 
  dplyr::mutate(Change_Absolute = `2020` - `2019`) %>% select(c(Sex:AgeGroup, Change_Absolute))
allcausechange <- ASDR_YOY %>% subset(condition == "AllCause") %>% dplyr::rename(allcause_change = Change_Absolute) %>% select(-c(condition))
noncovidchange <- ASDR_YOY %>%  subset(condition == "nonCovid")  %>% dplyr::rename(noncovid_change = Change_Absolute)%>% select(-c(condition))
netchanges <- merge(ASDR_YOY, allcausechange, by=c("AgeGroup", "Race.Ethnicity", "Sex"), all=T) 
decomposition <- merge(netchanges, noncovidchange, by=c("AgeGroup", "Race.Ethnicity", "Sex")) %>% 
  mutate(AllCause_pct = 100*Change_Absolute / allcause_change,
         NonCovid_pct = ifelse(condition!="AllCause" & condition!="COVID.19..U071..Underlying.Cause.of.Death.",  100*Change_Absolute / noncovid_change, NA))  
decomposition_long <- decomposition %>% select(-c(Change_Absolute:noncovid_change)) %>% 
  dplyr::mutate(condition = ifelse(condition=="AllCause", "1.AllCause",
                                   ifelse(condition=="COVID.19..U071..Underlying.Cause.of.Death.", "2.Covid",
                                          ifelse(condition=="nonCovid", "3.NonCovid", condition)))) %>% arrange(AgeGroup, Race.Ethnicity, Sex, condition) %>% 
  gather(key="measure", value="value", AllCause_pct:NonCovid_pct)
write.csv(decomposition, "Decomposition_unformatted.csv")

## format all cause and non covid decomposition and merge into table
allcause <- decomposition %>% subset(measure =="AllCause_pct") %>%  spread(Race.Ethnicity, value) %>% select(-c(measure)) %>% 
  dplyr::rename(All_allcause = All,
                White_allcause =`Non-Hispanic White`,
                Black_allcause =`Non-Hispanic Black`,
                Hisp_allcause =`Hispanic`)
noncovid <- decomposition %>% subset(measure =="NonCovid_pct") %>%  spread(Race.Ethnicity, value) %>% select(-c(measure)) %>% 
  dplyr::rename(All_noncovid = All,
                White_noncovid =`Non-Hispanic White`,
                Black_noncovid =`Non-Hispanic Black`,
                Hisp_noncovid =`Hispanic`)
decompositon <- merge(allcause, noncovid, by=c("AgeGroup", "Sex", "condition")) %>% 
  select(c(AgeGroup:All_allcause, All_noncovid, 
           White_allcause, White_noncovid,
           Black_allcause, Black_noncovid,
           Hisp_allcause, Hisp_noncovid)) 

## can filter for table of interest
# for example: TABLE 3: 
table3 <- decompositon %>% subset(AgeGroup=="25+" & Sex=="Both")
write.csv(table3, "Clean Data/Table3_Decomposition_25plusBothSexes.csv")















