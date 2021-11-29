update.packages(ask = FALSE, dependencies = TRUE)
getwd()
setwd("/Users/Ian Davis/Desktop/Social Lab")
df_ext <- read.csv(file.choose())
describe(df_ext)

#library#####
library(dplyr)
library(tidyr)
library(psych)
library(effects)
library(ggplot2)
library(car)
library(pastecs)

#Composite Variables

###### EIS composite variable #########


#EIS df
EIS <- df_ext[c(22:38)]

#EIS reverse score items: 1, 2, 7, 9, 10, 13, 16
EIS$EIS1_R = recode(EIS$EIS1, '1=4; 2=3; 3=2; 4=1')
EIS$EIS2_R = recode(EIS$EIS2, '1=4; 2=3; 3=2; 4=1')
EIS$EIS7_R = recode(EIS$EIS7, '1=4; 2=3; 3=2; 4=1')
EIS$EIS9_R = recode(EIS$EIS9, '1=4; 2=3; 3=2; 4=1')
EIS$EIS10_R = recode(EIS$EIS10, '1=4; 2=3; 3=2; 4=1')
EIS$EIS13_R = recode(EIS$EIS13, '1=4; 2=3; 3=2; 4=1')
EIS$EIS16_R = recode(EIS$EIS16, '1=4; 2=3; 3=2; 4=1')

#df for EIS reverse-scored only
EIS_R <- EIS[, c(18:19,3:6,20,8,21:22,11:12,23,14:15,24,17)]

#row means
EIS_R <- sapply(EIS_R, function(x) as.numeric(as.character(x)))
rowMeans(EIS_R,na.rm = TRUE)
EIS_comp <- rowMeans(EIS_R)
df_ext$EIS_comp <- rowMeans(EIS_R)


###### SDO comp variable #######

#SDO df
SDO <- df_ext[c(39:53)]

#SDO reverse score items: 9-16
SDO$SDO9_R = recode(SDO$SDO9, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
SDO$SD11_R = recode(SDO$SDO11, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
SDO$SDO12_R = recode(SDO$SDO12, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
SDO$SDO13_R = recode(SDO$SDO13, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
SDO$SDO14_R = recode(SDO$SDO14, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
SDO$SDO15_R = recode(SDO$SDO15, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
SDO$SDO16_R = recode(SDO$SDO16, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')

#df for SDO reverse-scored only
SDO_R <- SDO[, c(1:7,9,16:22)]


#row means
SDO_R <- sapply(SDO_R, function(x) as.numeric(as.character(x)))
rowMeans(SDO_R,na.rm = TRUE)
SDO_comp <- rowMeans(SDO_R)
df_ext$SDO_comp <- rowMeans(SDO_R)



###### RWA comp variable #######

#RWA df
RWA <- df_ext[c(54:67)]


#RWA reverse score items: 3,5,7,9,11,12,14
RWA$RWA3_R = recode(RWA$RWA3, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
RWA$RWA5_R = recode(RWA$RWA5, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
RWA$RWA7_R = recode(RWA$RWA7, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
RWA$RWA9_R = recode(RWA$RWA9, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
RWA$RWA11_R = recode(RWA$RWA11, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
RWA$RWA12_R = recode(RWA$RWA12, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
RWA$RWA14_R = recode(RWA$RWA14, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')

#df for RWA reverse-scored only
RWA_R <- RWA[, c(1,15,3,16,5,17,7,18,9,19:20,12,21,14)]


#row means
RWA_R <- sapply(RWA_R, function(x) as.numeric(as.character(x)))
rowMeans(RWA_R,na.rm = TRUE)
RWA_comp <- rowMeans(RWA_R)
df_ext$RWA_comp <- rowMeans(RWA_R)



###### CBRA comp variable #######

#CBRA df
CBRA <- df_ext[c(68:87)]


#CBRA reverse score items: 2, 4, 5, 6, 8, 11, 12, 15, 17, 20
CBRA$CBRA2_R = recode(CBRA$CBRA2, '1=5; 2=4; 3=3; 4=2; 5=1')
CBRA$CBRA4_R = recode(CBRA$CBRA4, '1=5; 2=4; 3=3; 4=2; 5=1')
CBRA$CBRA5_R = recode(CBRA$CBRA5, '1=5; 2=4; 3=3; 4=2; 5=1')
CBRA$CBRA6_R = recode(CBRA$CBRA6, '1=5; 2=4; 3=3; 4=2; 5=1')
CBRA$CBRA8_R = recode(CBRA$CBRA8, '1=5; 2=4; 3=3; 4=2; 5=1')
CBRA$CBRA11_R = recode(CBRA$CBRA11, '1=5; 2=4; 3=3; 4=2; 5=1')
CBRA$CBRA12_R = recode(CBRA$CBRA12, '1=5; 2=4; 3=3; 4=2; 5=1')
CBRA$CBRA15_R = recode(CBRA$CBRA15, '1=5; 2=4; 3=3; 4=2; 5=1')
CBRA$CBRA17_R = recode(CBRA$CBRA17, '1=5; 2=4; 3=3; 4=2; 5=1')
CBRA$CBRA20_R = recode(CBRA$CBRA20, '1=5; 2=4; 3=3; 4=2; 5=1')

#df for RWA reverse-scored only
CBRA_R <- CBRA[, c(1,3,7,9,10,13,14,16,18,19,21:30)]


#row means
CBRA_R <- sapply(CBRA_R, function(x) as.numeric(as.character(x)))
rowMeans(CBRA_R,na.rm = TRUE)
CBRA_comp <- rowMeans(CBRA_R)
df_ext$CBRA_comp <- rowMeans(CBRA_R)



###### BWAS comp variable #######

#BWAS df
BWAS <- df_ext[c(88:98, 100:112)]


#BWAS reverse score items: 2, 8, 16, 21, 22
BWAS$BWAS2_R = recode(BWAS$BWAS2, '1=5; 2=4; 3=3; 4=2; 5=1')
BWAS$BWAS8_R = recode(BWAS$BWAS8, '1=5; 2=4; 3=3; 4=2; 5=1')
BWAS$BWAS16_R = recode(BWAS$BWAS16, '1=5; 2=4; 3=3; 4=2; 5=1')
BWAS$BWAS21_R = recode(BWAS$BWAS21, '1=5; 2=4; 3=3; 4=2; 5=1')
BWAS$BWAS22_R = recode(BWAS$BWAS22, '1=5; 2=4; 3=3; 4=2; 5=1')


#df for BWAS reverse-scored only
BWAS_R <- BWAS[, c(1,3:7,9:15, 17:20,23:29)]


#row means
BWAS_R <- sapply(BWAS_R, function(x) as.numeric(as.character(x)))
rowMeans(BWAS_R,na.rm = TRUE)
BWAS_comp <- rowMeans(BWAS_R)
df_ext$BWAS_comp <- rowMeans(BWAS_R)




# Separate by condition ########

####FL_13 is control, FL_12 is test##

names(df_ext)[names(df_ext) == 'FL_4_DO'] <- 'Cond'
df_ext$Cond = recode(df_ext$Cond, '"FL_12" = "test"; "FL_13" = "control"')
df_test <- df_ext %>% filter(df_ext$Cond == "test")
df_control <- df_ext %>% filter(df_ext$Cond == "control")


# Demographics ##########
###### Gender & Sexuality ######
###1 = Cis male, 2 = Cis female, 3 = Trans male, 4 = Trans female,###
###5 = non-binary, 6 = other, 7 = prefer not to say###
p_gen <- df_ext$Gender
p_gen <- sapply(p_gen, function(x) as.numeric(as.character(x)))

amod_gender1 <- aov(RWA_comp~p_gen, data = df_ext)
summary(amod_gender1)

lm345 <- lm(EIS_comp~p_gen, data = df_ext)
summary(lm345)

p_sexuality <- df_ext$Sexuality

amod_sexuality1 <- aov(BWAS_comp~p_sexuality, data = df_ext)

lm_sexuality <- lm(EIS_comp~p_sexuality, data = df_ext)
summary(lm_sexuality)
###### Age + Year ########
p_age <- df_ext$Age
p_age <- sapply(p_age, function(x) as.numeric(as.character(x)))

####Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's ##
##18.00   19.00   19.00   19.62   20.00   24.00       4 ##
amod_age1 <- aov(BWAS_comp~p_age, data = df_ext)

lm_age1 <- lm(EIS_comp~p_age, data = df_ext)
summary(lm_age1)

p_College.Year <- df_ext$College.Year
p_College.Year <- sapply(p_College.Year, function(x) as.numeric(as.character(x)))

amod_College.Year <- aov(BWAS_comp~p_College.Year, data = df_ext)



###### Race, Ethnicity, Nationality, & Language######

###1 = White, 2 = Native American, 3 = Black, 5 = Latinx, 6 = PI###
###7 = S. Asian, 8 = MENA, 9 = E Asian, 10 = SE Asian, 13 = Mixed, 11 = Other###
p_race <- df_ext$Race
p_race = recode(p_race, '1 = "W"; 2 = "NatA"; 3 = "B"; 5 = "L"; 6 = "PI"; 7 = "SA"; 
                8 = "MENA"; 9 = "EA"; 10 = "SEA"; 13 = "Mix"; 11 = "Oth"; 12 = "Oth"')
p_race <- sapply(p_race, function(x) (as.character(x)))
summary(data.frame(p_race))
amod_race1 <- aov(BWAS_comp~p_race, data = df_ext)

lm_race <- lm(CBRA_comp~p_race, data = df_ext)
summary(lm_race)

df_ext_White <- df_ext %>% filter(df_ext$Race == 1)
df_ext_poc <- df_ext %>% filter(df_ext$Race != 1)

p_usborn <- df_ext$US.born
p_usborn <- data.frame(table(p_usborn))
amod_usborn1 <- aov(BWAS_comp~p_usborn, data = df_ext)


p_nlanguage <- df_ext$English.language
p_nlanguage <- data.frame(table(p_nlanguage))
amod_nlanguage1 <- aov(BWAS_comp~p_nlanguage, data = df_ext)


p_friends <- df_ext$Friends.
p_friends <- data.frame(table(p_friends))
amod_friends1 <- aov(BWAS_comp~p_friends, data = df_ext)


###### SES ####

p_income <- df_ext$Income
p_income <- sapply(p_income, function(x) as.numeric(as.character(x)))
amod_income1 <- aov(BWAS_comp~p_income, data = df_ext)


p_SESidentity <- df_ext$SES.ladder
#p_SESidentity = recode(p_SESidentity, '1 = "Bottom"; 10 = "Top"')
#p_SESidentity <- sapply(p_SESidentity, function(x) as.character(x))

amod_SESidentity1 <- aov(BWAS_comp~p_SESidentity, data = df_ext)




# Models ###############
###### effects of condition ######
### condition on BWAS ##
lmod_cond <- lm(BWAS_comp~Cond, data = df_ext_poc)
summary(lmod_cond)
lmod_cond_W <- lm(BWAS_comp~Cond, data = df_ext_White)
summary(lmod_cond_W)
##Condtest b = 0.12478, p ¿ .275###
##White sample, Condtest b = 0.2048, p ¿ .184###


### condition on RWA ##
lmod_cond_RWA <- lm(RWA_comp~Cond, data = df_ext_poc)
summary(lmod_cond_RWA)
lmod_cond_RWA_W <- lm(RWA_comp~Cond, data = df_ext_White)
summary(lmod_cond_RWA_W)
##Condtest b = 0.###
##White sample, Condtest b = 0.###

### condition on SDO ##
lmod_cond_SDO <- lm(SDO_comp~Cond, data = df_ext_poc)
summary(lmod_cond_SDO)
lmod_cond_SDO_W <- lm(SDO_comp~Cond, data = df_ext_White)
summary(lmod_cond_SDO_W)
##Condtest b = 0.###
##White sample, Condtest b = 0.###

### condition on CBRA ##
lmod_cond_CBRA <- lm(CBRA_comp~Cond, data = df_ext_poc)
summary(lmod_cond_CBRA)
lmod_cond_CBRA_W <- lm(CBRA_comp~Cond, data = df_ext_White)
summary(lmod_cond_CBRA_W)
##Condtest b = 0.###
##White sample, Condtest b = 0.###

### condition on EIS ##
lmod_cond_EIS <- lm(EIS_comp~Cond, data = df_ext)
summary(lmod_cond_EIS)
lmod_cond_EIS_W <- lm(EIS_comp~Cond, data = df_ext_White)
summary(lmod_cond_EIS_W)
##Condtest b = 0.###
##White sample, Condtest b = 0.###

###### relationships between SDO, RWA, CBRA, and BWAS ######

## RWA & SDO on CBRA ###
lmod_SDO_RWA_onCBRA <- lm(CBRA_comp~RWA_comp + SDO_comp, data = df_ext_poc)
summary(lmod_SDO_RWA_onCBRA)
lmod_SDO_RWA_onCBRA_W <- lm(CBRA_comp~RWA_comp + SDO_comp, data = df_ext_White)
summary(lmod_SDO_RWA_onCBRA_W)
#RWA_comp b = 0.188 p = .009 SDO_comp b = 0.242, p = .006##
#White sample, RWA_comp b = 0.148 p = .245 SDO_comp b = 0..223, p = .280##

## CBRA alone on BWAS ##
lmod_CBRA <- lm(BWAS_comp~CBRA_comp, data = df_ext_poc)
summary(lmod_CBRA)
lmod_CBRA_W <- lm(BWAS_comp~CBRA_comp, data = df_ext_White)
summary(lmod_CBRA_W)
#CBRA_comp b = 0.573 p = .00008##
#White sample, CBRA_comp b = 0.624 p = .0051 ##


##RWA on BWAS##
lmod_RWA1 <- lm(BWAS_comp~RWA_comp, data = df_ext_poc)
summary(lmod_RWA1)
lmod_RWA1_W <- lm(BWAS_comp~RWA_comp, data = df_ext_White)
summary(lmod_RWA1_W)

##SDO on BWAS##
lmod_SDO1 <- lm(BWAS_comp~SDO_comp, data = df_ext)
summary(lmod_SDO1)
lmod_SDO1_W <- lm(BWAS_comp~SDO_comp, data = df_ext_White)
summary(lmod_SDO1_W)

##RWA & SDO on BWAS##
lmod_SDO_RWA <- lm(BWAS_comp~RWA_comp + SDO_comp, data = df_ext_poc)
summary(lmod_SDO_RWA)
lmod_SDO_RWA_W <- lm(BWAS_comp~RWA_comp + SDO_comp, data = df_ext_White)
summary(lmod_SDO_RWA_W)
#RWA_comp b = 0.     SDO_comp b = 0.##

##RWA, SDO, & CBRA on BWAS###
lmod_SDO_RWA_CBRA <- lm(BWAS_comp~RWA_comp + SDO_comp + CBRA_comp, data = df_ext)
summary(lmod_SDO_RWA_CBRA)
lmod_SDO_RWA_CBRA_W <- lm(BWAS_comp~RWA_comp + SDO_comp + CBRA_comp, data = df_ext_White)
summary(lmod_SDO_RWA_CBRA_W)
#RWA_comp b = 0. SDO_comp b = 0.   CBRA_comp b = 0. ##

##RWA on CBRA##
lmod_RWA3 <- lm(CBRA_comp~RWA_comp, data = df_ext)
summary(lmod_RWA3)
lmod_RWA3_W <- lm(CBRA_comp~RWA_comp, data = df_ext_White)
summary(lmod_RWA3_W)

##SDO on CBRA##
lmod_SDO3 <- lm(CBRA_comp~SDO_comp, data = df_ext)
summary(lmod_SDO3)
lmod_SDO3_W <- lm(CBRA_comp~SDO_comp, data = df_ext_White)
summary(lmod_SDO3_W)

##RWA on SDO##
lmod_RWA2 <- lm(SDO_comp~RWA_comp, data = df_ext)
summary(lmod_RWA2)

##SDO on RWA##
lmod_SDO2 <- lm(RWA_comp~SDO_comp, data = df_ext_White)
summary(lmod_SDO2)
lmod_SDO8 <- lm(RWA_comp~SDO_comp, data = df_ext_poc)
summary(lmod_SDO8)

# CBRA as moderator #
lmod_SDO4 <- lm(BWAS_comp~SDO_comp + SDO_comp*CBRA_comp, data = df_ext_poc)
summary(lmod_SDO4)
lmod_SDO4_W <- lm(BWAS_comp~SDO_comp + SDO_comp*CBRA_comp, data = df_ext_White)
summary(lmod_SDO4_W)
lmod_RWA4 <- lm(BWAS_comp~RWA_comp + RWA_comp*CBRA_comp, data = df_ext_poc)
summary(lmod_RWA4)
lmod_RWA4_W <- lm(BWAS_comp~RWA_comp + RWA_comp*CBRA_comp, data = df_ext_White)
summary(lmod_RWA4_W)

###### ANOVAs & t-tests #########

# effect of condition #
amod_Cond <- aov(BWAS_comp~Cond, data = df_ext)
summary(amod_Cond)
amod_Cond_W <- aov(BWAS_comp~Cond, data = df_ext_White)
summary(amod_Cond_W)

# RWA & SDO on BWAS #
amod_RWA_SDO <- aov(BWAS_comp~RWA_comp + SDO_comp, data = df_ext)
summary(amod_RWA_SDO)
amod_RWA_SDO_W <- aov(BWAS_comp~RWA_comp + SDO_comp, data = df_ext)
summary(amod_RWA_SDO_W)

# CBRA on BWAS #
amod_CBRA <- aov(BWAS_comp~CBRA_comp, data = df_ext)
summary(amod_CBRA)
amod_CBRA_W <- aov(BWAS_comp~CBRA_comp, data = df_ext_White)
summary(amod_CBRA_W)

#RWA SDO#
amod_42 <- aov(SDO_comp~RWA_comp, data = df_ext_poc)
summary(amod_42)
amod_43 <- aov(SDO_comp~RWA_comp, data = df_ext_White)
summary(amod_43)

#t-test comparing BWAS by condition#
t.test(BWAS_comp~Cond, data = df_ext)
t.test(BWAS_comp~Cond, data = df_ext_White)
#p-value = 0.

#t-test comparing CBRA across conditions##
t.test(CBRA_comp~Cond, data = df_ext)
t.test(CBRA_comp~Cond, data = df_ext_White)
#p-value = 0.



# Visualization ####
###### BWAS distribution ####
plot_BWAS_dist <- ggplot(data = df_ext) +
                    geom_density(aes(BWAS_comp), kernel = "gaussian")+
                    geom_freqpoly(aes(BWAS_comp))
plot_BWAS_dist

###### SDO Distribution ######
plot_SDO_dist <- ggplot(data = df_ext) +
  geom_density(aes(SDO_comp), kernel = "gaussian")+
  geom_freqpoly(aes(SDO_comp))
plot_SDO_dist

###### RWA Distribution ######
plot_RWA_dist <- ggplot(data = df_ext) +
  geom_density(aes(RWA_comp), kernel = "gaussian")+
  geom_freqpoly(aes(RWA_comp))
plot_RWA_dist

###### boxplots #####

bp_EIS <- ggplot(data = df_ext) +
    geom_boxplot(aes(y = EIS_comp), width = 1, xmin = -.05, xmax = .05)+
  coord_cartesian(ylim = c(1,4)) 
bp_EIS

bp_BWAS <- ggplot(data = df_ext) +
  geom_boxplot(aes(y = BWAS_comp), width = 1, xmin = -.05, xmax = .05) +
  coord_cartesian(ylim = c(0,7))
bp_BWAS

bp_BWAS_gen <- ggplot(data = df_ext) +
  geom_boxplot(aes(y = BWAS_comp), width = 1, xmin = -.05, xmax = .05) +
  coord_cartesian(ylim = c(0,7)) +
  facet_grid(cols = vars(Gender))
bp_BWAS_gen

bp_SDO <- ggplot(data = df_ext) +
  geom_boxplot(aes(y = SDO_comp), width = 1, xmin = -.05, xmax = .05) +
  coord_cartesian(ylim = c(1,7))
bp_SDO

bp_SDO_gen <- ggplot(data = df_ext) +
  geom_boxplot(aes(y = SDO_comp), width = 1, xmin = -.05, xmax = .05) +
  coord_cartesian(ylim = c(1,7))+
  facet_grid(cols = vars(Gender))
bp_SDO_gen

bp_RWA <- ggplot(data = df_ext) +
  geom_boxplot(aes(y = RWA_comp), width = 1, xmin = -.05, xmax = .05) +
  coord_cartesian(ylim = c(1,9))
bp_RWA

bp_CBRA <- ggplot(data = df_ext) +
  geom_boxplot(aes(y = CBRA_comp), width = 1, xmin = -.05, xmax = .05) +
  coord_cartesian(ylim = c(1,5))
bp_CBRA


###### CBRA & BWAS Scatterplot####

plot_CBRA_BWAS <- ggplot(data = df_ext) +
                    geom_jitter(aes(x = CBRA_comp, y = BWAS_comp)) +
                    geom_smooth(aes(x = CBRA_comp, y = BWAS_comp))
plot_CBRA_BWAS

plot_CBRA_BWAS_W <- ggplot(data = df_ext_White) +
  geom_jitter(aes(x = CBRA_comp, y = BWAS_comp)) +
  geom_smooth(aes(x = CBRA_comp, y = BWAS_comp))
plot_CBRA_BWAS_W

###### SDO & CBRA Scatterplot####

plot_SDO_CBRA <- ggplot(data = df_ext) +
  geom_jitter(aes(x = SDO_comp, y = CBRA_comp)) +
  geom_smooth(aes(x = SDO_comp, y = CBRA_comp))
plot_SDO_CBRA




###### RWA & CBRA Scatterplot####

plot_RWA_CBRA <- ggplot(data = df_ext) +
  geom_jitter(aes(x = RWA_comp, y = CBRA_comp)) +
  geom_smooth(aes(x = RWA_comp, y = CBRA_comp))
plot_RWA_CBRA


###### RWA & SD0 Scatterplot####

plot_SDO_CBRA2 <- ggplot(data = df_ext) +
  geom_jitter(aes(x = SDO_comp, y = RWA_comp)) +
  geom_smooth(aes(x = SDO_comp, y = RWA_comp))
plot_SDO_CBRA2

#Mod1<-lm(MRS~RWA + SDO, data=data)
#Mod2<-lm(MRS~RWA+ SDO + CB, data=data)
#compare the two models using ANOVA
#aov(mod1, mod2)
#Mod1<-lm(MRS~RWA + SDO, data=data)
#Mod2<-lm(MRS~RWA+ SDO + CB, data=data)
#compare the two models using ANOVA
#aov(mod1, mod2)
