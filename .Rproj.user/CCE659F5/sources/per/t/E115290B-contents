library(dplyr)
library(lme4)
library(arm)

setwd("G:/OneDrive - american.edu/(C) American/수업/2024 Fall/Conduct 1/replication/Replication_Park&Favero(2023)")

# 1. Dataset
# 1.1 IAT data

# omitted 
load(file='Race_IAT_0316.rda')
load(file='Race_IAT_1216.rda')

load(file='ACS_White_0316.rda')
load(file='ACS_White_1216.rda')
load(file='ACS_All_0716.rda')

# Instead of importing data above and walking through the MRP process below,
# this file can be used as final data on county-level racial biases.
load(file='IAT_EXP_county.rda') 

# 1.2 CRDC + CCD + EDGE data
load(file='CRDC_CCD_Gift.rda')
load(file='CRDC_CCD_Sus.rda')

# 1.3 Teacher data
load(file='Teacher_State.rda')


# 2. Multilevel regressions
# 2.1 Implicit bias
formula_i_white = Imp_IAT ~ (1|age_sex_cat) + (1|FIPS) + (1|SFIP) + Trumpvote + Black_pop
formula_i_all = Imp_IAT ~ (1|race_gender_cat) + (1|age_cat) + (1|FIPS) + (1|SFIP) + Trumpvote + Black_pop

Race_IAT_0316_White <- Race_IAT_0316 %>% subset(race==1)
model_i_white <- lmer(formula = formula_i_white, 
                      data=Race_IAT_0316_White, REML = TRUE)
summary(model_i_white) 
#display(model_i_white, digits=5)
ranef(model_i_white)$age_sex_cat
se.ranef(model_i_white)$age_sex_cat


Race_IAT_0716 <- Race_IAT_0316 %>% subset(year>=2007)
model_i_all <- lmer(formula = formula_i_all, 
                    data=Race_IAT_0716, REML = TRUE)
summary(model_i_all) 
#display(model_i_all, digits=5)
ranef(model_i_all)$age_cat
ranef(model_i_all)$race_gender_cat
se.ranef(model_i_all)$age_cat
se.ranef(model_i_all)$race_gender_cat


Race_IAT_1216_white <- Race_IAT_1216 %>% subset(race==1)
model_i_1216 <- lmer(formula = formula_i_white,
                     data=Race_IAT_1216_white, REML = TRUE)
summary(model_i_1216) 
#display(model_i_1216, digits=5)
ranef(model_i_1216)$age_sex_cat
se.ranef(model_i_1216)$age_sex_cat


# 2.2 Explicit bias
formula_e_white = Exp_IAT ~ (1|age_sex_cat) + (1|FIPS) + (1|SFIP) + Trumpvote + Black_pop
formula_e_all = Exp_IAT ~ (1|race_gender_cat) + (1|age_cat) + (1|FIPS) + (1|SFIP) + Trumpvote + Black_pop


model_e_white <- lmer(formula = formula_e_white, 
                      data=Race_IAT_0316_White, REML = TRUE)
summary(model_e_white)
#display(model_e_white, digits=5)
ranef(model_e_white)$age_sex_cat
se.ranef(model_e_white)$age_sex_cat


model_e_all <- lmer(formula = formula_e_all, 
                    data=Race_IAT_0716, REML = TRUE)
summary(model_e_all)
#display(model_e_all, digits=5)
ranef(model_e_all)$race_gender_cat
ranef(model_e_all)$age_cat
se.ranef(model_e_all)$race_gender_cat
se.ranef(model_e_all)$age_cat


model_e_1216 <- lmer(formula = formula_e_white, 
                     data=Race_IAT_1216_white, REML = TRUE)
summary(model_e_1216) 
#display(model_e_1216, digits=5)
ranef(model_e_1216)$age_sex_cat
se.ranef(model_e_1216)$age_sex_cat



# 3. Poststratification
# 3.1 Implicit bias
ranefi_white <- ranef(model_i_white)
fixefi_white <- fixef(model_i_white)
ranefi_white_d <- data.frame(ranefi_white)
fixefi_white_d <- data.frame(fixefi_white)

ranefi_all <- ranef(model_i_all)
fixefi_all <- fixef(model_i_all)
ranefi_all_d <- data.frame(ranefi_all)
fixefi_all_d <- data.frame(fixefi_all)

ranefi_1216 <- ranef(model_i_1216)
fixefi_1216 <- fixef(model_i_1216)
ranefi_1216_d <- data.frame(ranefi_1216)
fixefi_1216_d <- data.frame(fixefi_1216)


ranefi_white_fips <- ranefi_white_d %>%
  subset(grpvar=="FIPS") %>%
  rename(FIPS=grp, fipsef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
ranefi_white_sfip <- ranefi_white_d %>%
  subset(grpvar=="SFIP") %>%
  rename(SFIP=grp, sfipef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
ranefi_white_age_sex <- ranefi_white_d %>%
  subset(grpvar=="age_sex_cat") %>%
  rename(age_sex_cat=grp, agesexef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
fixefi_white_do <- fixefi_white_d %>%
  mutate(obs=row_number())
fixefi_white_int <- fixefi_white_do %>%
  subset(obs==1) %>%
  rename(int=fixefi_white) %>% dplyr::select(-obs)
fixefi_white_trump <- fixefi_white_do %>%
  subset(obs==2) %>%
  rename(trumpef=fixefi_white) %>% dplyr::select(-obs)
fixefi_white_blackpop <- fixefi_white_do %>%
  subset(obs==3) %>%
  rename(blackpopef=fixefi_white) %>% dplyr::select(-obs)

IAT_county_white_weight1 <- merge(x=ACS_White_0316,
                            y=ranefi_white_fips,
                            by='FIPS', all.x=TRUE)
IAT_county_white_weight2 <- merge(x=IAT_county_white_weight1,
                            y=ranefi_white_sfip,
                            by='SFIP', all.x=TRUE)
IAT_county_white_weight3 <- merge(x=IAT_county_white_weight2,
                            y=ranefi_white_age_sex,
                            by='age_sex_cat', all.x=TRUE)
IAT_county_white_weight4 <- merge(x=IAT_county_white_weight3,
                            y=fixefi_white_int,
                            by=NULL, all.x=TRUE)
IAT_county_white_weight5 <- merge(x=IAT_county_white_weight4,
                            y=fixefi_white_trump,
                            by=NULL, all.x=TRUE)
IAT_county_white_weight6 <- merge(x=IAT_county_white_weight5,
                            y=fixefi_white_blackpop,
                            by=NULL, all.x=TRUE)
colSums(is.na(IAT_county_white_weight6))

IAT_county_white_weight <- IAT_county_white_weight6 %>%
  mutate(coef=int+agesexef+fipsef+sfipef+trumpef*Trumpvote+blackpopef*Black_pop) %>%
  mutate(county_weight=weight*coef)
IAT_county_white <- IAT_county_white_weight %>%
  group_by(FIPS) %>% summarise(wIAT_white=sum(county_weight))
colSums(is.na(IAT_county_white))


ranefi_all_fips <- ranefi_all_d %>%
  subset(grpvar=="FIPS") %>%
  rename(FIPS=grp, fipsef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
ranefi_all_sfip <- ranefi_all_d %>%
  subset(grpvar=="SFIP") %>%
  rename(SFIP=grp, sfipef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
ranefi_all_race_gender <- ranefi_all_d %>%
  subset(grpvar=="race_gender_cat") %>%
  rename(race_gender_cat=grp, racegenderef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
ranefi_all_age <- ranefi_all_d %>%
  subset(grpvar=="age_cat") %>%
  rename(age_cat=grp, ageef=condval) %>% dplyr::select(-grpvar, -term)
fixefi_all_do <- fixefi_all_d %>%
  mutate(obs=row_number())
fixefi_all_int <- fixefi_all_do %>%
  subset(obs==1) %>%
  rename(int=fixefi_all) %>% dplyr::select(-obs)
fixefi_all_trump <- fixefi_all_do %>%
  subset(obs==2) %>%
  rename(trumpef=fixefi_all) %>% dplyr::select(-obs)
fixefi_all_blackpop <- fixefi_all_do %>%
  subset(obs==3) %>%
  rename(blackpopef=fixefi_all) %>% dplyr::select(-obs)

IAT_county_all_weight1 <- merge(x=ACS_All_0716,
                                y=ranefi_all_fips,
                                by='FIPS', all.x=TRUE)
IAT_county_all_weight2 <- merge(x=IAT_county_all_weight1,
                                y=ranefi_all_sfip,
                                by='SFIP', all.x=TRUE)
IAT_county_all_weight3 <- merge(x=IAT_county_all_weight2,
                                y=ranefi_all_race_gender,
                                by='race_gender_cat', all.x=TRUE)
IAT_county_all_weight31 <- merge(x=IAT_county_all_weight3,
                                 y=ranefi_all_age,
                                 by='age_cat', all.x=TRUE)
IAT_county_all_weight4 <- merge(x=IAT_county_all_weight31,
                                y=fixefi_all_int,
                                by=NULL, all.x=TRUE)
IAT_county_all_weight5 <- merge(x=IAT_county_all_weight4,
                                y=fixefi_all_trump,
                                by=NULL, all.x=TRUE)
IAT_county_all_weight6 <- merge(x=IAT_county_all_weight5,
                                y=fixefi_all_blackpop,
                                by=NULL, all.x=TRUE)
colSums(is.na(IAT_county_all_weight6))

IAT_county_all_weight <- IAT_county_all_weight6 %>%
  mutate(ceof=int+racegenderef+ageef+fipsef+sfipef+trumpef*Trumpvote+blackpopef*Black_pop) %>%
  mutate(county_weight=weight*ceof)
IAT_county_all <- IAT_county_all_weight %>%
  group_by(FIPS) %>% summarise(wIAT_all=sum(county_weight))
colSums(is.na(IAT_county_all))


ranefi_1216_fips <- ranefi_1216_d %>%
  subset(grpvar=="FIPS") %>%
  rename(FIPS=grp, fipsef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
ranefi_1216_sfip <- ranefi_1216_d %>%
  subset(grpvar=="SFIP") %>%
  rename(SFIP=grp, sfipef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
ranefi_1216_age_sex <- ranefi_1216_d %>%
  subset(grpvar=="age_sex_cat") %>%
  rename(age_sex_cat=grp, agesexef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
fixefi_1216_do <- fixefi_1216_d %>%
  mutate(obs=row_number())
fixefi_1216_int <- fixefi_1216_do %>%
  subset(obs==1) %>%
  rename(int=fixefi_1216) %>% dplyr::select(-obs)
fixefi_1216_trump <- fixefi_1216_do %>%
  subset(obs==2) %>%
  rename(trumpef=fixefi_1216) %>% dplyr::select(-obs)
fixefi_1216_blackpop <- fixefi_1216_do %>%
  subset(obs==3) %>%
  rename(blackpopef=fixefi_1216) %>% dplyr::select(-obs)

IAT_county_1216_weight1 <- merge(x=ACS_White_1216,
                                 y=ranefi_1216_fips,
                                 by='FIPS', all.x=TRUE)
IAT_county_1216_weight2 <- merge(x=IAT_county_1216_weight1,
                                 y=ranefi_1216_sfip,
                                 by='SFIP', all.x=TRUE)
IAT_county_1216_weight3 <- merge(x=IAT_county_1216_weight2,
                                 y=ranefi_1216_age_sex,
                                 by='age_sex_cat', all.x=TRUE)
IAT_county_1216_weight4 <- merge(x=IAT_county_1216_weight3,
                                 y=fixefi_1216_int,
                                 by=NULL, all.x=TRUE)
IAT_county_1216_weight5 <- merge(x=IAT_county_1216_weight4,
                                 y=fixefi_1216_trump,
                                 by=NULL, all.x=TRUE)
IAT_county_1216_weight6 <- merge(x=IAT_county_1216_weight5,
                                 y=fixefi_1216_blackpop,
                                 by=NULL, all.x=TRUE)
colSums(is.na(IAT_county_1216_weight6))

IAT_county_1216_weight <- IAT_county_1216_weight6 %>%
  mutate(coef=int+agesexef+fipsef+sfipef+trumpef*Trumpvote+blackpopef*Black_pop) %>%
  mutate(county_weight=weight*coef)
IAT_county_1216 <- IAT_county_1216_weight %>%
  group_by(FIPS) %>% summarise(wIAT_1216=sum(county_weight))
colSums(is.na(IAT_county_1216))


IAT_county1 <- merge(x=IAT_county_white, y=IAT_county_all,
                     by='FIPS', all=TRUE)
IAT_county2 <-  merge(x=IAT_county1, y=IAT_county_1216,
                      by='FIPS', all=TRUE)
colSums(is.na(IAT_county2))


# 3.2 Explicit bias
ranefe_white <- ranef(model_e_white)
fixefe_white <- fixef(model_e_white)
ranefe_white_d <- data.frame(ranefe_white)
fixefe_white_d <- data.frame(fixefe_white)

ranefe_all <- ranef(model_e_all)
fixefe_all <- fixef(model_e_all)
ranefe_all_d <- data.frame(ranefe_all)
fixefe_all_d <- data.frame(fixefe_all)

ranefe_1216 <- ranef(model_e_1216)
fixefe_1216 <- fixef(model_e_1216)
ranefe_1216_d <- data.frame(ranefe_1216)
fixefe_1216_d <- data.frame(fixefe_1216)


ranefe_white_fips <- ranefe_white_d %>%
  subset(grpvar=="FIPS") %>%
  rename(FIPS=grp, fipsef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
ranefe_white_sfip <- ranefe_white_d %>%
  subset(grpvar=="SFIP") %>%
  rename(SFIP=grp, sfipef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
ranefe_white_age_sex <- ranefe_white_d %>%
  subset(grpvar=="age_sex_cat") %>%
  rename(age_sex_cat=grp, agesexef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
fixefe_white_do <- fixefe_white_d %>%
  mutate(obs=row_number())
fixefe_white_int <- fixefe_white_do %>%
  subset(obs==1) %>%
  rename(int=fixefe_white) %>% dplyr::select(-obs)
fixefe_white_trump <- fixefe_white_do %>%
  subset(obs==2) %>%
  rename(trumpef=fixefe_white) %>% dplyr::select(-obs)
fixefe_white_blackpop <- fixefe_white_do %>%
  subset(obs==3) %>%
  rename(blackpopef=fixefe_white) %>% dplyr::select(-obs)

EXP_county_white_weight1 <- merge(x=ACS_White_0316,
                                  y=ranefe_white_fips,
                                  by='FIPS', all.x=TRUE)
EXP_county_white_weight2 <- merge(x=EXP_county_white_weight1,
                                  y=ranefe_white_sfip,
                                  by='SFIP', all.x=TRUE)
EXP_county_white_weight3 <- merge(x=EXP_county_white_weight2,
                                  y=ranefe_white_age_sex,
                                  by='age_sex_cat', all.x=TRUE)
EXP_county_white_weight4 <- merge(x=EXP_county_white_weight3,
                                  y=fixefe_white_int,
                                  by=NULL, all.x=TRUE)
EXP_county_white_weight5 <- merge(x=EXP_county_white_weight4,
                                  y=fixefe_white_trump,
                                  by=NULL, all.x=TRUE)
EXP_county_white_weight6 <- merge(x=EXP_county_white_weight5,
                                  y=fixefe_white_blackpop,
                                  by=NULL, all.x=TRUE)
colSums(is.na(EXP_county_white_weight6))

EXP_county_white_weight <- EXP_county_white_weight6 %>%
  mutate(coef=int+agesexef+fipsef+sfipef+trumpef*Trumpvote+blackpopef*Black_pop) %>%
  mutate(county_weight=weight*coef)
EXP_county_white <- EXP_county_white_weight %>%
  group_by(FIPS) %>% summarise(wEXP_white=sum(county_weight))
colSums(is.na(EXP_county_white))


ranefe_all_fips <- ranefe_all_d %>%
  subset(grpvar=="FIPS") %>%
  rename(FIPS=grp, fipsef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
ranefe_all_sfip <- ranefe_all_d %>%
  subset(grpvar=="SFIP") %>%
  rename(SFIP=grp, sfipef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
ranefe_all_race_gender <- ranefe_all_d %>%
  subset(grpvar=="race_gender_cat") %>%
  rename(race_gender_cat=grp, racegenderef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
ranefe_all_age <- ranefe_all_d %>%
  subset(grpvar=="age_cat") %>%
  rename(age_cat=grp, ageef=condval) %>% dplyr::select(-grpvar, -term)
fixefe_all_do <- fixefe_all_d %>%
  mutate(obs=row_number())
fixefe_all_int <- fixefe_all_do %>%
  subset(obs==1) %>%
  rename(int=fixefe_all) %>% dplyr::select(-obs)
fixefe_all_trump <- fixefe_all_do %>%
  subset(obs==2) %>%
  rename(trumpef=fixefe_all) %>% dplyr::select(-obs)
fixefe_all_blackpop <- fixefe_all_do %>%
  subset(obs==3) %>%
  rename(blackpopef=fixefe_all) %>% dplyr::select(-obs)

EXP_county_all_weight1 <- merge(x=ACS_All_0716,
                                y=ranefe_all_fips,
                                by='FIPS', all.x=TRUE)
EXP_county_all_weight2 <- merge(x=EXP_county_all_weight1,
                                y=ranefe_all_sfip,
                                by='SFIP', all.x=TRUE)
EXP_county_all_weight3 <- merge(x=EXP_county_all_weight2,
                                y=ranefe_all_race_gender,
                                by='race_gender_cat', all.x=TRUE)
EXP_county_all_weight31 <- merge(x=EXP_county_all_weight3,
                                 y=ranefe_all_age,
                                 by='age_cat', all.x=TRUE)
EXP_county_all_weight4 <- merge(x=EXP_county_all_weight31,
                                y=fixefe_all_int,
                                by=NULL, all.x=TRUE)
EXP_county_all_weight5 <- merge(x=EXP_county_all_weight4,
                                y=fixefe_all_trump,
                                by=NULL, all.x=TRUE)
EXP_county_all_weight6 <- merge(x=EXP_county_all_weight5,
                                y=fixefe_all_blackpop,
                                by=NULL, all.x=TRUE)
colSums(is.na(EXP_county_all_weight6))

EXP_county_all_weight <- EXP_county_all_weight6 %>%
  mutate(ceof=int+racegenderef+ageef+fipsef+sfipef+trumpef*Trumpvote+blackpopef*Black_pop) %>%
  mutate(county_weight=weight*ceof)
EXP_county_all <- EXP_county_all_weight %>%
  group_by(FIPS) %>% summarise(wEXP_all=sum(county_weight))
colSums(is.na(EXP_county_all))


ranefe_1216_fips <- ranefe_1216_d %>%
  subset(grpvar=="FIPS") %>%
  rename(FIPS=grp, fipsef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
ranefe_1216_sfip <- ranefe_1216_d %>%
  subset(grpvar=="SFIP") %>%
  rename(SFIP=grp, sfipef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
ranefe_1216_age_sex <- ranefe_1216_d %>%
  subset(grpvar=="age_sex_cat") %>%
  rename(age_sex_cat=grp, agesexef=condval) %>% dplyr::select(-grpvar,-term,-condsd)
fixefe_1216_do <- fixefe_1216_d %>%
  mutate(obs=row_number())
fixefe_1216_int <- fixefe_1216_do %>%
  subset(obs==1) %>%
  rename(int=fixefe_1216) %>% dplyr::select(-obs)
fixefe_1216_trump <- fixefe_1216_do %>%
  subset(obs==2) %>%
  rename(trumpef=fixefe_1216) %>% dplyr::select(-obs)
fixefe_1216_blackpop <- fixefe_1216_do %>%
  subset(obs==3) %>%
  rename(blackpopef=fixefe_1216) %>% dplyr::select(-obs)

EXP_county_1216_weight1 <- merge(x=ACS_White_1216,
                                 y=ranefe_1216_fips,
                                 by='FIPS', all.x=TRUE)
EXP_county_1216_weight2 <- merge(x=EXP_county_1216_weight1,
                                 y=ranefe_1216_sfip,
                                 by='SFIP', all.x=TRUE)
EXP_county_1216_weight3 <- merge(x=EXP_county_1216_weight2,
                                 y=ranefe_1216_age_sex,
                                 by='age_sex_cat', all.x=TRUE)
EXP_county_1216_weight4 <- merge(x=EXP_county_1216_weight3,
                                 y=fixefe_1216_int,
                                 by=NULL, all.x=TRUE)
EXP_county_1216_weight5 <- merge(x=EXP_county_1216_weight4,
                                 y=fixefe_1216_trump,
                                 by=NULL, all.x=TRUE)
EXP_county_1216_weight6 <- merge(x=EXP_county_1216_weight5,
                                 y=fixefe_1216_blackpop,
                                 by=NULL, all.x=TRUE)
colSums(is.na(EXP_county_1216_weight6))

EXP_county_1216_weight <- EXP_county_1216_weight6 %>%
  mutate(coef=int+agesexef+fipsef+sfipef+trumpef*Trumpvote+blackpopef*Black_pop) %>%
  mutate(county_weight=weight*coef)
EXP_county_1216 <- EXP_county_1216_weight %>%
  group_by(FIPS) %>% summarise(wEXP_1216=sum(county_weight))
colSums(is.na(EXP_county_1216))


EXP_county1 <- merge(x=EXP_county_white, y=EXP_county_all,
                     by='FIPS', all=TRUE)
EXP_county2 <-  merge(x=EXP_county1, y=EXP_county_1216,
                      by='FIPS', all=TRUE)
colSums(is.na(EXP_county2))


# 3.3 County-level Implicit & Exmplicit bias
IAT_EXP_county <- merge(x=IAT_county2, y=EXP_county2,
                        by='FIPS', all=TRUE)
colSums(is.na(IAT_EXP_county))

#save(IAT_EXP_county, file='IAT_EXP_county.rda')
#load(file='IAT_EXP_county.rda')

# 4. Merge
# 4.1 Gifted Program
Gift_IAT <- merge(x=CRDC_CCD_Gift, y=IAT_EXP_county,
                  by='FIPS', all.x=TRUE)
Gift_IAT_Teacher <- merge(x=Gift_IAT, y=Teacher_State,
                          by='NCESSCH', all.x=TRUE)
colSums(is.na(Gift_IAT_Teacher))

# 4.2 Suspension
Sus_IAT <- merge(x=CRDC_CCD_Sus, y=IAT_EXP_county,
                 by='FIPS', all.x=TRUE)
Sus_IAT_Teacher <- merge(x=Sus_IAT, y=Teacher_State,
                         by='NCESSCH', all.x=TRUE)
colSums(is.na(Sus_IAT_Teacher))




