
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
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(ggpubr)

# load data
## Cause of death data
ASDR <- read.csv("Tables/NEWTABLE_ASDR.csv") %>% select(-c("X"))
decomposition <- read.csv("Tables/Decomposition_unformatted.csv") %>% select(-c("X"))

#-----------------------------------------------------------------------------------------------------------------
# FIGURE 1
## formatting
baseline <- ASDR %>% subset(YEAR==2019 & condition=="AllCause") %>% 
  dplyr::mutate(condition="ASDR Baseline, 2019") %>% 
  select(c(Sex, Race.Ethnicity, condition, AgeGroup, ASDR)) %>% dplyr::mutate(YEAR=2019)
baseline2020 <- baseline %>% dplyr::mutate(YEAR=2020)
increases <- ASDR %>% 
  spread(YEAR, ASDR)  %>% 
  dplyr::mutate(Change_Absolute = `2020` - `2019`) %>% select(c(Sex:AgeGroup, Change_Absolute)) %>% 
  subset(condition == "COVID.19..U071..Underlying.Cause.of.Death." | condition=="nonCovid") %>% 
  dplyr::mutate(condition = ifelse(condition == "COVID.19..U071..Underlying.Cause.of.Death.", "Covid-19 Increase", "Non-Covid-19 Increase")) %>% 
  dplyr::rename(ASDR=Change_Absolute) %>% dplyr::mutate(YEAR=2020)
ASDR_visual <- rbind(baseline,baseline2020, increases)
ASDR_visual <- ASDR_visual %>% dplyr::mutate(Race.Ethnicity = ifelse(Race.Ethnicity == "Non-Hispanic White", "NH White", ifelse(Race.Ethnicity == "Non-Hispanic Black", "NH Black", "Hispanic")))
ASDR_visual$YEAR <- as.character(ASDR_visual$YEAR)
ASDR_visual$Race.Ethnicity <- factor(ASDR_visual$Race.Ethnicity, levels=c("NH White", "NH Black", "Hispanic"))
ASDR_visual$condition <- factor(ASDR_visual$condition, levels=c("Covid-19 Increase", "Non-Covid-19 Increase","ASDR Baseline, 2019"))

## bar graph visuals
### top row: 25+
ASDR_2019 <- ASDR_visual %>% subset(condition=="ASDR Baseline, 2019" & YEAR =="2019" & AgeGroup=="25+" & Sex=="Both")
Age_25PLUS <- ASDR_visual %>%  subset(AgeGroup=="25+" & Sex=="Both") %>% 
  ggplot(., aes(fill=condition, y=ASDR, x=YEAR)) +
  geom_bar(position='stack',stat='identity', color="black", width=0.7) +
  facet_wrap(~Race.Ethnicity, ncol=3)+
  scale_colour_grey()+
  scale_fill_manual(values = c("gray10", "gray60", "gray90")) +
  theme_minimal() +
  scale_y_continuous(limits=c(0,2000), expand=c(0,0), sec.axis = sec_axis(~.))+
  theme(strip.text = element_text(size = 20,face="bold", family="serif"),
        axis.text.x  = element_blank(),
        axis.text.y = element_text(size = 14, family="serif"),
        axis.title.y = element_text(size = 20, face="bold", family="serif"),
        axis.title.x =element_blank(),
        legend.title = element_blank(),
        legend.position="top",
        legend.text = element_text(size=18, family="serif"),
        panel.spacing = unit(0.8, "lines"),
        panel.border = element_rect(color="grey50", fill=NA),
        strip.background = element_rect(color="grey10"),
        plot.title = element_text(size=18, family = "serif")) +
  labs(y=expression(atop(bold("PANEL A: Ages 25+"), paste("ASDR"))))+
  geom_hline(data=ASDR_2019, aes(yintercept=ASDR), linetype="dashed")
### middle row: 25-64
ASDR_2019 <- ASDR_visual %>% subset(condition=="ASDR Baseline, 2019" & YEAR =="2019" & AgeGroup=="25-64" & Sex=="Both")
Age_25to64 <- ASDR_visual %>%  subset(AgeGroup=="25-64" & Sex=="Both") %>% 
  ggplot(., aes(fill=condition, y=ASDR, x=YEAR)) +
  geom_bar(position='stack',stat='identity', color="black", width=0.7) + 
  facet_wrap(~Race.Ethnicity, ncol=3)+
  scale_fill_manual(values = c("gray10", "gray60", "gray90")) +
  theme_minimal() +
  scale_y_continuous(limits=c(0,800), expand=c(0,0), sec.axis = sec_axis(~.))+
  theme(strip.text = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y = element_text(size = 14, family="serif"),
        axis.title.y = element_text(size = 20, face="bold", family="serif"),
        axis.title.x =element_blank(),
        legend.title = element_blank(),
        legend.position="none",
        legend.text = element_text(size=14, family="serif"),
        panel.spacing = unit(0.8, "lines"),
        panel.border = element_rect(color="grey50", fill=NA),
        strip.background = element_rect(color="grey10"),
        plot.title = element_text(size=18, family = "serif")) +
  labs(y=expression(atop(bold("PANEL B: Ages 25-64"), paste("ASDR"))))+
  geom_hline(data=ASDR_2019, aes(yintercept=ASDR), linetype="dashed")
### bottom row: 65+
ASDR_2019 <- ASDR_visual %>% subset(condition=="ASDR Baseline, 2019" & YEAR =="2019" & AgeGroup=="65+" & Sex=="Both")
Age_65PLUS <- ASDR_visual %>%  subset(AgeGroup=="65+" & Sex=="Both") %>% 
  ggplot(., aes(fill=condition, y=ASDR, x=YEAR)) +
  geom_bar(position='stack',stat='identity', color="black", width=0.7) + 
  facet_wrap(~Race.Ethnicity, ncol=3)+
  scale_fill_manual(values = c("gray10", "gray60", "gray90")) +
  theme_minimal() +
  scale_y_continuous(limits=c(0,6000),  expand=c(0,0), sec.axis = sec_axis(~.))+
  theme(strip.text = element_blank(),
        axis.text.x  = element_text(size = 16, face="bold", family="serif"),
        axis.text.y = element_text(size = 14, family="serif"),
        axis.title.y = element_text(size = 20, face="bold", family="serif"),
        axis.title.x =element_blank(),
        legend.title = element_blank(),
        legend.position="none",
        legend.text = element_text(size=14, family="serif"),
        panel.spacing = unit(0.8, "lines"),
        panel.border = element_rect(color="grey50", fill=NA),
        strip.background = element_rect(color="grey10"),
        plot.title = element_text(size=18, family = "serif")) +
  labs(y=expression(atop(bold("PANEL C: Ages 65+"), paste("ASDR"))))+
  geom_hline(data=ASDR_2019, aes(yintercept=ASDR), linetype="dashed")
## append panels together
ggarrange(Age_25PLUS, Age_25to64, Age_65PLUS, ncol=1, heights=c(8,6,7), align="v")
ggsave("visuals/FIGURE1.jpg", width = 10, height = 12)



#-----------------------------------------------------------------------------------------------------------------
# FIGURE 2
## formatting
ASDR_changes  <- decomposition %>% subset(Sex == "Both" & (condition != "AllCause" & condition !="nonCovid" & condition != "COVID.19..U071..Underlying.Cause.of.Death.") &
                                            (Race.Ethnicity == "Hispanic" |  Race.Ethnicity == "Non-Hispanic White" |  Race.Ethnicity == "Non-Hispanic Black")) %>% 
  mutate(condition = ifelse(condition == "Alzheimer.disease..G30.", "Alzheimer's",
                            ifelse(condition == "Chronic.lower.respiratory.diseases..J40.J47.", "CLRD",
                                   ifelse(condition == "Cerebrovascular.diseases..I60.I69.", "Cerebrovascular",
                                          ifelse(condition == "Influenza.and.pneumonia..J09.J18.", "Influenza and pneum.",
                                                 ifelse(condition == "Diabetes.mellitus..E10.E14.", "Diabetes",
                                                        ifelse(condition == "Malignant.neoplasms..C00.C97.", "Cancer",
                                                               ifelse(condition == "Other.diseases.of.respiratory.system..J00.J06.J30.J39.J67.J70.J98.", "Other respiratory",
                                                                      ifelse(condition == "Symptoms..signs.and.abnormal.clinical.and.laboratory.findings..not.elsewhere.classified..R00.R99.", "Signs Not Classified",
                                                                             ifelse(condition == "Nephritis..nephrotic.syndrome.and.nephrosis..N00.N07.N17.N19.N25.N27.", "Nephritis",
                                                                                    ifelse(condition == "Septicemia..A40.A41.", "Septicemia",
                                                                                           ifelse(condition == "ExternalCause", "External Cause", 
                                                                                                  ifelse(condition == "Diseases.of.heart..I00.I09.I11.I13.I20.I51.", "Heart Disease", "All Other Causes")))))))))))),
         AgeGroup = ifelse(AgeGroup =="25+", "Ages 25+", 
                           ifelse(AgeGroup == "25-64", "Ages 25-64", "Ages 65+"))) 
ASDR_changes$Race.Ethnicity <- factor(ASDR_changes$Race.Ethnicity, levels=c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic"))
ASDR_changes$condition <- factor(ASDR_changes$condition, levels=c("All Other Causes", "Heart Disease", "External Cause", "Diabetes", "Alzheimer's", 
                                                                  "Cerebrovascular", "Influenza and pneum.", "Signs Not Classified",
                                                                  "Other respiratory","Septicemia", "Nephritis",  "CLRD",  "Cancer"))

## bar graph visuals
### top row: 25+
ages_25plus <- ASDR_changes %>% subset(AgeGroup=="Ages 25+") %>% 
  ggplot(., aes(fill=condition, y=Change_Absolute, x = condition, order=condition)) +
  geom_bar(stat='identity', color="black",width=0.8)+
  facet_wrap(~Race.Ethnicity,  ncol=3)+
  #coord_flip()+
  scale_y_continuous(labels = function(x) sprintf("%+d", x), sec.axis = sec_axis(~., labels = function(x) sprintf("%+d", x)))+
  theme_minimal() +
  theme(strip.text = element_text(size = 20,face="bold", family="serif"),
        axis.text.x  = element_blank(),
        axis.text.y = element_text(size = 14, family="serif"),
        axis.title.y = element_text(size = 20, family="serif"),
        axis.title.x =element_blank(),
        legend.title = element_blank(),
        legend.position="none",
        legend.text = element_text(size=14, family="serif"),
        panel.spacing = unit(0.8, "lines"),
        panel.border = element_rect(color="grey50", fill=NA),
        strip.background = element_rect(color="grey10"),
        plot.title = element_text(size=18, face="bold", family = "serif")) +
  geom_hline(yintercept=0, size=1.5) +
  labs(y=expression(atop(bold("PANEL A: Ages 25+"), paste("ASDR Change, 2019-2020"))))
### middle row: 25-64
ages_25to64 <- ASDR_changes %>% subset(AgeGroup=="Ages 25-64") %>% 
  ggplot(., aes(fill=condition, y=Change_Absolute, x = condition, order=condition)) +
  geom_bar(stat='identity', color="black",width=0.8)+
  facet_wrap(~Race.Ethnicity,  ncol=3)+
  #coord_flip()+
  scale_y_continuous(labels = function(x) sprintf("%+d", x), sec.axis = sec_axis(~., labels = function(x) sprintf("%+d", x)))+
  theme_minimal() +
  theme(strip.text = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y = element_text(size = 14, family="serif"),
        axis.title.y = element_text(size = 20, family="serif"),
        axis.title.x =element_blank(),
        legend.title = element_blank(),
        legend.position="none",
        legend.text = element_text(size=14, family="serif"),
        panel.spacing = unit(0.8, "lines"),
        panel.border = element_rect(color="grey50", fill=NA),
        strip.background = element_rect(color="grey10"),
        plot.title = element_text(size=18, face="bold", family = "serif")) +
  geom_hline(yintercept=0, size=1.5) +
  labs(y=expression(atop(bold("PANEL B: Ages 25-64"), paste("ASDR Change, 2019-2020"))))
### bottom row: 65+
ages_65plus <- ASDR_changes %>% subset(AgeGroup=="Ages 65+") %>% 
  ggplot(., aes(fill=condition, y=Change_Absolute, x = condition, order=condition)) +
  geom_bar(stat='identity', color="black",width=0.8)+
  facet_wrap(~Race.Ethnicity,  ncol=3)+
  #coord_flip()+
  scale_y_continuous(labels = function(x) sprintf("%+d", x), sec.axis = sec_axis(~., labels = function(x) sprintf("%+d", x)))+
  theme_minimal() +
  theme(strip.text = element_blank(),
        axis.text.x  = element_text(size = 18, family="serif", face="bold", angle=90, hjust=0.95, vjust=0.2),
        axis.text.y = element_text(size = 14, family="serif"),
        axis.title.y = element_text(size = 20, family="serif"),
        axis.title.x =element_blank(),
        legend.title = element_blank(),
        legend.position="none",
        legend.text = element_text(size=14, family="serif"),
        panel.spacing = unit(0.8, "lines"),
        panel.border = element_rect(color="grey50", fill=NA),
        strip.background = element_rect(color="grey10"),
        plot.title = element_text(size=18, face="bold", family = "serif")) +
  geom_hline(yintercept=0, size=1.5) +
  labs(y=expression(atop(bold("PANEL C: Ages 65+"), paste("ASDR Change, 2019-2020"))))
## append panels together
ggarrange(ages_25plus,ages_25to64, ages_65plus, ncol=1, heights=c(7,6.5,10), align="v")
ggsave("visuals/FIGURE2.jpg", width = 12, height = 14)



#-----------------------------------------------------------------------------------------------------------------
# RACIAL COMPARISONS
## formatting
ASDR_change <- ASDR %>% 
  spread(YEAR, ASDR)  %>% 
  dplyr::mutate(
    netchange = (`2020` - `2019`),
    pctchange = (`2020` - `2019`) /`2019`)

## ABSOLUTE CHANGES
ASDRchange_BW <- ASDR_change %>% subset(AgeGroup == "25+" & Sex == "Both" & 
                                          condition != "AllCause" & condition != "nonCovid" & condition != "COVID.19..U071..Underlying.Cause.of.Death." & condition != "Unknown" &
                                          (Race.Ethnicity == "Non-Hispanic White" |  Race.Ethnicity == "Non-Hispanic Black")) %>% 
  dplyr::mutate(condition = recode(condition, "Alzheimer.disease..G30." = "Alzheimer disease",
                                   "Cerebrovascular.diseases..I60.I69." = "Cerebrovascular disease",
                                   "Chronic.lower.respiratory.diseases..J40.J47."= "CLRD",
                                   "Diabetes.mellitus..E10.E14." = "Diabetes",
                                   "Diseases.of.heart..I00.I09.I11.I13.I20.I51." = "Heart disease",
                                   "ExternalCause" = "External cause",
                                   "Influenza.and.pneumonia..J09.J18." = "Influenza", 
                                   "Malignant.neoplasms..C00.C97." = "Malignant Neoplasms",
                                   "Nephritis..nephrotic.syndrome.and.nephrosis..N00.N07.N17.N19.N25.N27." = "Nephritis",
                                   "Other.diseases.of.respiratory.system..J00.J06.J30.J39.J67.J70.J98." = "Other resp.", 
                                   "Septicemia..A40.A41." = "Septicemia",
                                   "Symptoms..signs.and.abnormal.clinical.and.laboratory.findings..not.elsewhere.classified..R00.R99." = "Signs not classified")) %>% 
  select(-c(`2019`, `2020`, pctchange)) %>% 
  spread(Race.Ethnicity, netchange) %>% 
  rename(., White = `Non-Hispanic White`,
         Black = `Non-Hispanic Black`)
## Black-White
trsup <- data.frame(x=c(-10,-10,45),y=c(-10,45,45))
BW_absolute <- ASDRchange_BW %>% ggplot(aes(x=White, y=Black))+ 
  scale_y_continuous(limits=c(-10,45), expand = c(0,0))+
  scale_x_continuous(limits=c(-10,45), expand = c(0,0))+
  geom_abline(intercept = 0, slope = 1, size=0.3)+
  geom_polygon(aes(x=x, y=y), data=trsup, fill="grey50", alpha=0.3)+
  geom_point(size = 3)+
  theme_bw()+
  theme(axis.text.x  = element_text(size = 12, family="serif", angle=90, vjust=0),
        axis.text.y = element_text(size = 12, family="serif"),
        axis.title.y = element_text(size = 16, face="bold", family="serif"),
        axis.title.x =  element_blank(),
        legend.title = element_blank(),
        legend.position= "none",
        legend.text = element_text(size=18, family="serif"),
        plot.title = element_text(size=18, face="bold", family="serif",hjust = 0.5),
        axis.line = element_line(size = 0.5, colour = "gray50", linetype=1))+
  ggtitle("Non-Hispanic Black vs. Non-Hispanic White") +
  labs(y=expression(atop(bold("PANEL A: ABSOLUTE CHANGE"), paste("Black ASDR Change, 2019-2020"))))+
  xlab("White ASDR Change, 2019-2020")+
  geom_text_repel(aes(x = White, y = Black, label = condition),  family="serif", size=4.5) 
## Hispanic-White
ASDRchange_HW <- ASDR_change %>% subset(AgeGroup == "25+" & Sex == "Both" & 
                                          condition != "AllCause" & condition != "nonCovid" & condition != "COVID.19..U071..Underlying.Cause.of.Death." & condition != "Unknown" &
                                          (Race.Ethnicity == "Non-Hispanic White" |  Race.Ethnicity == "Hispanic")) %>% 
  dplyr::mutate(condition = recode(condition, "Alzheimer.disease..G30." = "Alzheimer disease",
                                   "Cerebrovascular.diseases..I60.I69." = "Cerebrovascular disease",
                                   "Chronic.lower.respiratory.diseases..J40.J47."= "CLRD",
                                   "Diabetes.mellitus..E10.E14." = "Diabetes",
                                   "Diseases.of.heart..I00.I09.I11.I13.I20.I51." = "Heart disease",
                                   "ExternalCause" = "External cause",
                                   "Influenza.and.pneumonia..J09.J18." = "Influenza", 
                                   "Malignant.neoplasms..C00.C97." = "Malignant Neoplasms",
                                   "Nephritis..nephrotic.syndrome.and.nephrosis..N00.N07.N17.N19.N25.N27." = "Nephritis",
                                   "Other.diseases.of.respiratory.system..J00.J06.J30.J39.J67.J70.J98." = "Other resp.", 
                                   "Septicemia..A40.A41." = "Septicemia",
                                   "Symptoms..signs.and.abnormal.clinical.and.laboratory.findings..not.elsewhere.classified..R00.R99." = "Signs not classified")) %>% 
  select(-c(`2019`, `2020`, pctchange)) %>% 
  spread(Race.Ethnicity, netchange) %>% 
  rename(., White = `Non-Hispanic White`,
         Hispanic = `Hispanic`)
trsup <- data.frame(x=c(-10,-10,45),y=c(-10,45,45))
HW_absolute <- ASDRchange_HW %>% ggplot(aes(x=White, y=Hispanic))+ 
  scale_y_continuous(limits=c(-10,45), expand = c(0,0),position="right")+
  scale_x_continuous(limits=c(-10,45), expand = c(0,0))+
  geom_abline(intercept = 0, slope = 1, size=0.3 )+
  geom_polygon(aes(x=x, y=y), data=trsup, fill="grey50", alpha=0.3)+
  geom_point(size = 3)+
  theme_bw()+
  theme(axis.text.x  = element_text(size = 12, family="serif", angle=90),
        axis.text.y = element_text(size = 12, family="serif"),
        axis.title.y = element_text(size = 16, family="serif"),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position= "none",
        legend.text = element_text(size=18, family="serif"),
        plot.title = element_text(size=18, face="bold", family="serif",hjust = 0.5),
        axis.line = element_line(size = 0.5, colour = "gray50", linetype=1))+
  ggtitle("Hispanic vs. Non-Hispanic White") +
  labs(y=expression(atop(bold("PANEL A: ABSOLUTE CHANGE"), paste("Black ASDR Change, 2019-2020"))))+
  xlab("White ASDR Change, 2019-2020")+
  geom_text_repel(aes(x = White, y = Hispanic, label = condition),  family="serif", size=4.5) 


## PERCENT CHANGES
## Black-White
ASDRchange_BW <- ASDR_change %>% subset(AgeGroup == "25+" & Sex == "Both" & 
                                          condition != "AllCause" & condition != "nonCovid" & condition != "COVID.19..U071..Underlying.Cause.of.Death." & condition != "Unknown" &
                                          (Race.Ethnicity == "Non-Hispanic White" |  Race.Ethnicity == "Non-Hispanic Black")) %>% 
  dplyr::mutate(condition = recode(condition, "Alzheimer.disease..G30." = "Alzheimer disease",
                                   "Cerebrovascular.diseases..I60.I69." = "Cerebrovascular disease",
                                   "Chronic.lower.respiratory.diseases..J40.J47."= "CLRD",
                                   "Diabetes.mellitus..E10.E14." = "Diabetes",
                                   "Diseases.of.heart..I00.I09.I11.I13.I20.I51." = "Heart disease",
                                   "ExternalCause" = "External cause",
                                   "Influenza.and.pneumonia..J09.J18." = "Influenza", 
                                   "Malignant.neoplasms..C00.C97." = "Malignant Neoplasms",
                                   "Nephritis..nephrotic.syndrome.and.nephrosis..N00.N07.N17.N19.N25.N27." = "Nephritis",
                                   "Other.diseases.of.respiratory.system..J00.J06.J30.J39.J67.J70.J98." = "Other resp.", 
                                   "Septicemia..A40.A41." = "Septicemia",
                                   "Symptoms..signs.and.abnormal.clinical.and.laboratory.findings..not.elsewhere.classified..R00.R99." = "Signs not classified")) %>% 
  select(-c(`2019`, `2020`, netchange)) %>% 
  spread(Race.Ethnicity, pctchange) %>% 
  rename(., White = `Non-Hispanic White`,
         Black = `Non-Hispanic Black`)
trsup <- data.frame(x=c(-0.1,-0.1,0.30),y=c(-0.1,0.30,0.30))
BW_percent <- ASDRchange_BW %>% ggplot(aes(x=White, y=Black))+ 
  scale_y_continuous(labels =scales::percent_format(accuracy = 1L), breaks=c(-0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.25), expand = c(0,0))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1L),breaks=c(-0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.25), expand = c(0,0))+
  geom_abline(intercept = 0, slope = 1, size=0.3)+
  geom_polygon(aes(x=x, y=y), data=trsup, fill="grey50", alpha=0.3)+
  geom_point(size = 3)+
  theme_bw()+
  theme(axis.text.x  = element_text(size = 12, family="serif", angle=90, vjust=0),
        axis.text.y = element_text(size = 12, family="serif"),
        axis.title.y = element_text(size = 16, face="bold", family="serif"),
        axis.title.x = element_text(size = 16, family="serif"),
        legend.title = element_blank(),
        legend.position= "none",
        legend.text = element_text(size=18, family="serif"),
        plot.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "gray50", linetype=1))+
  ggtitle("Non-Hispanic Black vs. Non-Hispanic White") +
  labs(y=expression(atop(bold("PANEL B: PERCENT CHANGE"), paste("Black ASDR Change, 2019-2020"))))+
  xlab("White ASDR Change, 2019-2020")+
  geom_text_repel(aes(x = White, y = Black, label = condition),  family="serif", size=4.5) 
## Hispanic-White
ASDRchange_HW <- ASDR_change %>% subset(AgeGroup == "25+" & Sex == "Both" & 
                                          condition != "AllCause" & condition != "nonCovid" & condition != "COVID.19..U071..Underlying.Cause.of.Death." & condition != "Unknown" &
                                          (Race.Ethnicity == "Non-Hispanic White" |  Race.Ethnicity == "Hispanic")) %>% 
  dplyr::mutate(condition = recode(condition, "Alzheimer.disease..G30." = "Alzheimer disease",
                                   "Cerebrovascular.diseases..I60.I69." = "Cerebrovascular disease",
                                   "Chronic.lower.respiratory.diseases..J40.J47."= "CLRD",
                                   "Diabetes.mellitus..E10.E14." = "Diabetes",
                                   "Diseases.of.heart..I00.I09.I11.I13.I20.I51." = "Heart disease",
                                   "ExternalCause" = "External cause",
                                   "Influenza.and.pneumonia..J09.J18." = "Influenza", 
                                   "Malignant.neoplasms..C00.C97." = "Malignant Neoplasms",
                                   "Nephritis..nephrotic.syndrome.and.nephrosis..N00.N07.N17.N19.N25.N27." = "Nephritis",
                                   "Other.diseases.of.respiratory.system..J00.J06.J30.J39.J67.J70.J98." = "Other resp.", 
                                   "Septicemia..A40.A41." = "Septicemia",
                                   "Symptoms..signs.and.abnormal.clinical.and.laboratory.findings..not.elsewhere.classified..R00.R99." = "Signs not classified")) %>% 
  select(-c(`2019`, `2020`, netchange)) %>% 
  spread(Race.Ethnicity, pctchange) %>% 
  rename(., White = `Non-Hispanic White`,
         Hispanic = `Hispanic`)
trsup <- data.frame(x=c(-0.1,-0.1,0.30),y=c(-0.1,0.30,0.30))
HW_percent <- ASDRchange_HW %>% ggplot(aes(x=White, y=Hispanic))+ 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),breaks=c(-0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.25), expand = c(0,0), position="right")+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1L), breaks=c(-0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.25), expand = c(0,0))+
  geom_abline(intercept = 0, slope = 1, size=0.3)+
  geom_polygon(aes(x=x, y=y), data=trsup, fill="grey50", alpha=0.3)+
  geom_point(size = 3)+
  theme_bw()+
  theme(axis.text.x  = element_text(size = 12, family="serif", angle=90),
        axis.text.y = element_text(size = 12, family="serif"),
        axis.title.y = element_text(size = 16, family="serif"),
        axis.title.x = element_text(size = 16, family="serif"),
        legend.title = element_blank(),
        legend.position= "none",
        legend.text = element_text(size=18, family="serif"),
        plot.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "gray50", linetype=1))+
  ggtitle("Hispanic vs. Non-Hispanic White") +
  labs(y=expression(atop(bold("PANEL B: PERCENT CHANGE"), paste("Black ASDR Change, 2019-2020"))))+
  xlab("White ASDR Change, 2019-2020")+
  geom_text_repel(aes(x = White, y = Hispanic, label = condition),  family="serif", size=4.5) 


## append panels together
bw <- ggarrange(BW_absolute, NULL, BW_percent, ncol=1, align="v", heights=c(20,0,20))
hw <- ggarrange(HW_absolute, NULL, HW_percent, ncol=1, align="v", heights=c(20,0,20))
ggarrange(bw, hw, ncol=2, nrow=1, align="v")
ggsave("visuals/FIGURE3.jpg", width = 12, height = 10)






