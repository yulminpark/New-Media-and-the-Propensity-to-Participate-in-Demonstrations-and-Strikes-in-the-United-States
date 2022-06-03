
################################################################################
########    R Coding Script    #################################################
################################################################################
########    New Media and the Propensity to Participate                 ########
########           in Demonstrations and Strikes in the United States   ########

########    Data:  World Values Survey (WVS), Wave 7                    ########
########           (publicly available at:                              ########
########    https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp)  ########
################################################################################
################################################################################

### Get data: WVS Wave 7
install.packages("dplyr")
install.packages("tidyverse")
install.packages("haven")

library(dplyr)
library(tidyverse)
library(haven)

DATA <- read_dta("WVS Wave 7 (2017)_United_States_Stata.dta")


### Select variables
data1 <- select(DATA, N_REGION_ISO, Q211, Q212,
               Q275, Q199, Q200, Q204, Q206, Q207,
               Q152, Q154, Q155,
               Q262, Q260, Q287, Q240) # n = 2596

data2 <- filter(data1, N_REGION_ISO != 840051) # remove Washington DC
# now 2,592 respondents in 50 states


################################################################################
### Analysis 1. 
###      Dependent variable = Potential for attending peaceful demonstrations
################################################################################

### Re-code dependent variable (DV) and independent variables (IVs)

## DV1: Potential for participation--attending peaceful demonstrations
table(data2$Q211) #   -2    1    2    3
                  #   29  504 1404  655
data2 <- filter(data2, Q211 != 1) # remove "Have done"
data2$Q211[data2$Q211 == -2] <- NA
data2$Q211 <- -(data2$Q211 - 3)

table(data2$Q211) # 0 ("Would never do"): 655 
                  # 1 ("Might do"): 1404

# N (attending peaceful demonstrations) = 2,088 observations 


## IV1: Education
table(data2$Q275)
data2$Q275[data2$Q275 == -2] <- NA
data2$Q275[data2$Q275 == -1] <- NA
data2$Q275[data2$Q275 <= 2] <- 1
data2$Q275[data2$Q275 == 3] <- 2
data2$Q275[data2$Q275 == 4] <- 3
data2$Q275[data2$Q275 == 5] <- 3
data2$Q275[data2$Q275 == 6] <- 4
data2$Q275[data2$Q275 == 7] <- 5
data2$Q275[data2$Q275 == 8] <- 5
# 1 = Below high school; 2 = High school graduate; 3 = Some college; 
# 4 = Bachelor or equivalent; 5 = Master or above

table(data2$Q275) #    1    2    3    4    5
                  #   40  522  758  462  273


## IV2: Political interest
table(data2$Q199)
data2$Q199[data2$Q199 == -2] <- NA
data2$Q199 <- -(data2$Q199 - 5)

table(data2$Q199) #   1   2   3   4
                  # 249 541 913 365


## IV3: Interpersonal communication
table(data2$Q200)
data2$Q200[data2$Q200 == -2] <- NA
data2$Q200[data2$Q200 == -1] <- NA
data2$Q200 <- -(data2$Q200 - 4)

table(data2$Q200) #    1    2    3
                  #  205  480 1374


## IV4-(1): Media use (mobile phone)
table(data2$Q204)
data2$Q204[data2$Q204 == -2] <- NA
data2$Q204[data2$Q204 == -1] <- NA
data2$Q204 <- -(data2$Q204 - 6)
# 1 = Never; 2 = Less than monthly; 3 = Monthly; 4 = Weekly; 5 = Daily

table(data2$Q204) #    1    2    3    4    5
                  #  340  160  112  305 1131


## IV4-(2): Media use (Internet)
table(data2$Q206)
data2$Q206[data2$Q206 == -2] <- NA
data2$Q206[data2$Q206 == -1] <- NA
data2$Q206 <- -(data2$Q206 - 6)
# 1 = Never; 2 = Less than monthly; 3 = Monthly; 4 = Weekly; 5 = Daily

table(data2$Q206) #    1    2    3    4    5 
                  #  136  106  110  316 1381


# IV4-(3): Media use (social media)
table(data2$Q207)
data2$Q207[data2$Q207 == -2] <- NA
data2$Q207[data2$Q207 == -1] <- NA
data2$Q207 <- -(data2$Q207 - 6)
# 1 = Never; 2 = Less than monthly; 3 = Monthly; 4 = Weekly; 5 = Daily

table(data2$Q207) #    1    2    3    4    5
                  #  470  139  126  312 1008


## IV5: Post-materialism 
table(data2$Q154)
table(data2$Q155)

data2 <- data2 %>%
  add_column(postmat = NA)

data2$postmat[data2$Q154 == 1 & data2$Q155 == 3] <- 1
data2$postmat[data2$Q154 == 3 & data2$Q155 == 1] <- 1
data2$postmat[data2$Q154 == 2 & data2$Q155 == 4] <- 3
data2$postmat[data2$Q154 == 4 & data2$Q155 == 2] <- 3

data2$postmat[data2$Q154 == 1 & data2$Q155 == 2] <- 2
data2$postmat[data2$Q154 == 1 & data2$Q155 == 4] <- 2
data2$postmat[data2$Q154 == 2 & data2$Q155 == 1] <- 2
data2$postmat[data2$Q154 == 2 & data2$Q155 == 3] <- 2
data2$postmat[data2$Q154 == 3 & data2$Q155 == 2] <- 2
data2$postmat[data2$Q154 == 3 & data2$Q155 == 4] <- 2
data2$postmat[data2$Q154 == 4 & data2$Q155 == 1] <- 2
data2$postmat[data2$Q154 == 4 & data2$Q155 == 3] <- 2
# 1 = Materialist; 2 = Mixed; 3 = Post-materialist

table(data2$postmat) #    1    2    3
                     #  322 1230  450


## Control1: Age
table(data2$Q262) # range: 18 to 90
data2$Q262[data2$Q262 <= 20] <- 1
data2$Q262[data2$Q262 >= 21 & data2$Q262 <= 24] <- 2
data2$Q262[data2$Q262 >= 25 & data2$Q262 <= 29] <- 3
data2$Q262[data2$Q262 >= 30 & data2$Q262 <= 34] <- 4
data2$Q262[data2$Q262 >= 35 & data2$Q262 <= 39] <- 5
data2$Q262[data2$Q262 >= 40 & data2$Q262 <= 44] <- 6
data2$Q262[data2$Q262 >= 45 & data2$Q262 <= 49] <- 7
data2$Q262[data2$Q262 >= 50 & data2$Q262 <= 54] <- 8
data2$Q262[data2$Q262 >= 55 & data2$Q262 <= 59] <- 9
data2$Q262[data2$Q262 >= 60 & data2$Q262 <= 64] <- 10
data2$Q262[data2$Q262 >= 65] <- 11

table(data2$Q262)


## Control2: Gender
table(data2$Q260) # 1 = male; 2 = female
                  #    1    2
                  # 1079 1009


## Control3: Income
table(data2$Q287)
data2$Q287[data2$Q287 == -2] <- NA
data2$Q287[data2$Q287 == -1] <- NA
data2$Q287 <- -(data2$Q287 - 6)
# 1 = Lower class; 2 = Working class; 3 = Lower middle class;
# 4 = Upper middle class; 5 = Upper class

table(data2$Q287) #    1   2   3   4   5
                  #  162 508 769 589  27


## Control4: Ideology
table(data2$Q240)
data2$Q240[data2$Q240 == -2] <- NA
data2$Q240[data2$Q240 == -1] <- NA
# 1 = left ---> 10 = right

table(data2$Q240) #   1   2   3   4   5   6   7   8   9  10 
                  # 152  74 208 158 544 226 218 203  91 160


### Rename variables
colnames(data2)
names(data2)[1]  <- "state"
names(data2)[2]  <- "demons"
names(data2)[3]  <- "strike"
names(data2)[4]  <- "edu"
names(data2)[5]  <- "interest"
names(data2)[6]  <- "discuss"
names(data2)[7]  <- "mobile"
names(data2)[8]  <- "internet"
names(data2)[9]  <- "socmed"
names(data2)[10] <- "post1"
names(data2)[11] <- "post2"
names(data2)[12] <- "post2_1"
names(data2)[13] <- "age"
names(data2)[14] <- "gender"
names(data2)[15] <- "income"
names(data2)[16] <- "ideology"
names(data2)[17] <- "post"
colnames(data2)


### Factor dummy variables (IVs, Control, DVs)
install.packages("stats")
install.packages("base")

library(stats)
library(base)

library(dplyr)
library(haven)

## Factor IV1: Education ("edu")
data2 <- data2 %>%
  mutate(edu_f = factor(edu, levels = c(1, 2, 3, 4, 5), 
                        labels = c("below high", "high",
                                   "some college", "bachelor",
                                   "master or above")))
select(data2, edu, edu_f)


## Factor IV5: Postmaterialism ("post")
data2 <- data2 %>%
  mutate(post_f = factor(post, levels = c(1, 2, 3), 
                          labels = c("materialist", "mixed", "postmaterialist")))
select(data2, post, post_f)


## Factor Control2: Gender ("gender")
data2 <- data2 %>%
mutate(gender_f = factor(gender, levels = c(1,2), 
                         labels = c("male", "female")))
select(data2, gender, gender_f)


## Factor DV1: Potential for participation--attending peaceful demonstrations
data2 <- data2 %>%
  mutate(demons_f = factor(demons, levels = c(0, 1), 
                         labels = c("Would never do", "Might do")))
select(data2, demons, demons_f)

# Histogram of DV1
install.packages("ggplot")

library(ggplot2)

ggplot(data = na.omit(data2), 
       aes(x = factor(demons_f))) +
  labs(title = "              ",
       x = "willingness for attending peaceful demonstrations", 
       y = "number of respondents") +
  geom_bar(fill = "brown", alpha = 0.4, stat = "count", width = 0.6) +
  theme_classic()

table(data2$demons_f)


### Run logit regression models

model1 <- glm(demons_f ~ socmed, 
              data = data2, family = "binomial")
summary(model1) # just social media

model2 <- glm(demons_f ~ socmed + post_f, 
              data = data2, family = "binomial")
summary(model2) # just the main predictors

model3 <- glm(demons_f ~ socmed + post_f 
              + edu_f + interest + discuss,
              data = data2, family = "binomial")
summary(model3) # just the explanatory variables

model4 <- glm(demons_f ~ socmed + post_f 
              + edu_f + interest + discuss
              + age + gender_f + income + ideology,
              data = data2, family = "binomial")
summary(model4) # included control variables

model5 <- glm(demons_f ~ mobile + internet + socmed + post_f
              + edu_f + interest + discuss,
              data = data2, family = "binomial")
summary(model5) # included mobile use, internet use


model6 <- glm(demons_f ~ mobile + internet + socmed + post_f
             + edu_f + interest + discuss
             + age + gender_f + income + ideology,
             data = data2, family = "binomial")
summary(model6) # included mobile use, internet use
                # and control variables


### Create confidence interval (CI) plots
install.packages("dotwhisker")

library(dotwhisker)
library(dplyr)

dwplot(model4) # social media use
dwplot(model6) # social media use + mobile phone use + Internet use
 
## CIs using profiled log-likelihood
confint(model4)
confint(model6)


## CIs using standard errors
confint.default(model4)
confint.default(model6)


################################################################################
### Analysis 2. 
###      Dependent variable = Potential for joining strikes
################################################################################

### Set new dataset
data3 <- filter(data1, N_REGION_ISO != 840051) # Washington DC out
# now 2,592 respondents in 50 states


### Re-code dependent variable (DV) and independent variables (IVs)

## DV2: Potential for participation--joining strikes
table(data3$Q212) #  -2   1    2    3 
                  #  34 206 1347 1005
data3 <- filter(data3, Q212 != 1) # remove "Have done"
data3$Q212[data3$Q212 == -2] <- NA
data3$Q212 <- -(data3$Q212 - 3)
table(data3$Q212) # 0 ("Would never do"): 1005
                  # 1 ("Might do"): 1347

## N = 2386 observations (with "joining strikes")


## IV1: Education
table(data3$Q275)
data3$Q275[data3$Q275 == -2] <- NA
data3$Q275[data3$Q275 == -1] <- NA
data3$Q275[data3$Q275 <= 2] <- 1
data3$Q275[data3$Q275 == 3] <- 2
data3$Q275[data3$Q275 == 4] <- 3
data3$Q275[data3$Q275 == 5] <- 3
data3$Q275[data3$Q275 == 6] <- 4
data3$Q275[data3$Q275 == 7] <- 5
data3$Q275[data3$Q275 == 8] <- 5
# 1 = Below high school; 2 = High school graduate; 3 = Some college; 
# 4 = Bachelor or equivalent; 5 = Master or above

table(data3$Q275) #    1    2    3    4    5
                  #   40  522  814  591  386


## IV2: Political interest
table(data3$Q199)
data3$Q199[data3$Q199 == -2] <- NA
data3$Q199 <- -(data3$Q199 - 5)

table(data3$Q199) #   1   2    3   4
                  # 257 575 1018 515


## IV3: Interpersonal communication
table(data3$Q200)
data3$Q200[data3$Q200 == -2] <- NA
data3$Q200[data3$Q200 == -1] <- NA
data3$Q200 <- -(data3$Q200 - 4)

table(data3$Q200) #    1    2    3
                  #  212  482  1660


## IV4-(1): Media use (mobile phone)
table(data3$Q204)
data3$Q204[data3$Q204 == -2] <- NA
data3$Q204[data3$Q204 == -1] <- NA
data3$Q204 <- -(data3$Q204 -6)
data3$Q204 <- - data3$Q204
# 1 = Never; 2 = Less than monthly; 3 = Monthly; 4 = Weekly; 5 = Daily

table(data3$Q204) #    1    2    3    4    5
                  #  356  177  132  345 1334


## IV4-(2): Media use (Internet)
table(data3$Q206)
data3$Q206[data3$Q206 == -2] <- NA
data3$Q206[data3$Q206 == -1] <- NA
data3$Q206 <- -(data3$Q206 - 6)
# 1 = Never; 2 = Less than monthly; 3 = Monthly; 4 = Weekly; 5 = Daily

table(data3$Q206) #    1    2    3    4    5 
                  #  131  115  114  357 1633


# IV4-(3): Media use (social media)
table(data3$Q207)
data3$Q207[data3$Q207 == -2] <- NA
data3$Q207[data3$Q207 == -1] <- NA
data3$Q207 <- -(data3$Q207 - 6)
# 1 = Never; 2 = Less than monthly; 3 = Monthly; 4 = Weekly; 5 = Daily

table(data3$Q207) #    1    2    3    4    5
                  #  503  173  144  367 1168


## IV5: Post-materialism 
table(data3$Q154)
table(data3$Q155)
data3 <- data3 %>%
  add_column(postmat = NA)

data3$postmat[data3$Q154 == 1 & data3$Q155 == 3] <- 1
data3$postmat[data3$Q154 == 3 & data3$Q155 == 1] <- 1
data3$postmat[data3$Q154 == 2 & data3$Q155 == 4] <- 3
data3$postmat[data3$Q154 == 4 & data3$Q155 == 2] <- 3

data3$postmat[data3$Q154 == 1 & data3$Q155 == 2] <- 2
data3$postmat[data3$Q154 == 1 & data3$Q155 == 4] <- 2
data3$postmat[data3$Q154 == 2 & data3$Q155 == 1] <- 2
data3$postmat[data3$Q154 == 2 & data3$Q155 == 3] <- 2
data3$postmat[data3$Q154 == 3 & data3$Q155 == 2] <- 2
data3$postmat[data3$Q154 == 3 & data3$Q155 == 4] <- 2
data3$postmat[data3$Q154 == 4 & data3$Q155 == 1] <- 2
data3$postmat[data3$Q154 == 4 & data3$Q155 == 3] <- 2
# 1 = Materialist; 2 = Mixed; 3 = Post-materialist

table(data3$postmat) #    1    2    3
                     #  342 1359  597


## Control1: Age
table(data3$Q262) # range: 18 to 90
data3$Q262[data3$Q262 <= 20] <- 1
data3$Q262[data3$Q262 >= 21 & data3$Q262 <= 24] <- 2
data3$Q262[data3$Q262 >= 25 & data3$Q262 <= 29] <- 3
data3$Q262[data3$Q262 >= 30 & data3$Q262 <= 34] <- 4
data3$Q262[data3$Q262 >= 35 & data3$Q262 <= 39] <- 5
data3$Q262[data3$Q262 >= 40 & data3$Q262 <= 44] <- 6
data3$Q262[data3$Q262 >= 45 & data3$Q262 <= 49] <- 7
data3$Q262[data3$Q262 >= 50 & data3$Q262 <= 54] <- 8
data3$Q262[data3$Q262 >= 55 & data3$Q262 <= 59] <- 9
data3$Q262[data3$Q262 >= 60 & data3$Q262 <= 64] <- 10
data3$Q262[data3$Q262 >= 65] <- 11

table(data3$Q262)


## Control2: Gender
table(data3$Q260) # 1 = male; 2 = female
                  #    1    2
                  # 1258 1128


## Control3: Income
table(data3$Q287)
data3$Q287[data3$Q287 == -2] <- NA
data3$Q287[data3$Q287 == -1] <- NA
data3$Q287 <- -(data3$Q287 - 6)
# 1 = Lower class; 2 = Working class; 3 = Lower middle class;
# 4 = Upper middle class; 5 = Upper class

table(data3$Q287) #    1   2   3   4   5
                  #  174 564 880 705  29


## Control4: Ideology
table(data3$Q240)
data3$Q240[data3$Q240 == -2] <- NA
data3$Q240[data3$Q240 == -1] <- NA
# 1 = left ---> 10 = right

table(data3$Q240) #   1   2   3   4   5   6   7   8   9  10 
                  # 218 115 282 190 564 240 232 223  93 171


### Rename variables
colnames(data3)
names(data3)[1]  <- "state"
names(data3)[2]  <- "demons"
names(data3)[3]  <- "strike"
names(data3)[4]  <- "edu"
names(data3)[5]  <- "interest"
names(data3)[6]  <- "discuss"
names(data3)[7]  <- "mobile"
names(data3)[8]  <- "internet"
names(data3)[9]  <- "socmed"
names(data3)[10] <- "post1"
names(data3)[11] <- "post2"
names(data3)[12] <- "post2_1"
names(data3)[13] <- "age"
names(data3)[14] <- "gender"
names(data3)[15] <- "income"
names(data3)[16] <- "ideology"
names(data3)[17] <- "post"
colnames(data3)


### Factor dummy variables (IVs, Control, DVs)

## Factor IV1: Education ("edu")
data3 <- data3 %>%
  mutate(edu_f = factor(edu, levels = c(1, 2, 3, 4, 5), 
                        labels = c("below high", "high",
                                   "some college", "bachelor",
                                   "master or above")))
select(data3, edu, edu_f)


## Factor IV5: Postmaterialism ("post")
data3 <- data3 %>%
  mutate(post_f = factor(post, levels = c(1, 2, 3), 
                         labels = c("materialist", "mixed", "postmaterialist")))
select(data2, post, post_f)


## Factor Control2: Gender ("gender")
data3 <- data3 %>%
  mutate(gender_f = factor(gender, levels = c(1,2), 
                           labels = c("male", "female")))
select(data3, gender, gender_f)


## Factor DV2: Potential for participation-joining strikes
data3 <- data3 %>%
  mutate(strike_f = factor(strike, levels = c(0, 1), 
                           labels = c("Would never do", "Might do")))
select(data3, strike, strike_f)

# Histogram of DV2
ggplot(data = na.omit(data3), 
       aes(x = factor(strike_f))) +
  labs(title = "                                     ",
       x = "willingness for joining strikes", 
       y = "number of respondents") +
  geom_bar(fill = "brown", alpha = 0.7, stat = "count", width = 0.6) +
  theme_classic()

table(data3$strike_f)


### Run logit regression models

model7 <- glm(strike_f ~ socmed, 
              data = data3, family = "binomial")
summary(model7) # just social media

model8 <- glm(strike_f ~ socmed + post_f, 
              data = data3, family = "binomial")
summary(model8) # just the main predictors

model9 <- glm(strike_f ~ socmed + post_f 
              + edu_f + interest + discuss,
              data = data3, family = "binomial")
summary(model9) # just the explanatory variables

model10 <- glm(strike_f ~ socmed + post_f
              + edu_f + interest + discuss
              + age + gender_f + income + ideology, 
              data = data3, family = "binomial")
summary(model10) # included control variables

model11 <- glm(strike_f ~ mobile + internet + socmed + post_f + 
              + edu_f + interest + discuss,
              data = data3, family = "binomial")
summary(model11) # included mobile use, internet use


model12 <- glm(strike_f ~ mobile + internet + socmed + post_f + 
              + edu_f + interest + discuss
              + age + gender_f + income + ideology,
              data = data3, family = "binomial")
summary(model12) # included mobile use, internet use
                 # and control variables


### Create confidence interval (CI) plots
dwplot(model10) # social media use
dwplot(model12) # social media use + mobile phone use + Internet use

## CIs using profiled log-likelihood
confint(model10)
confint(model12)


## CIs using standard errors
confint.default(model10)
confint.default(model12)


## CI plots for both demonstrations and strikes in one graph

# just social media
dwplot(list(model4, model10))

dwplot(list(model4, model10),
       vline = geom_vline(
         xintercept = 0,
         colour = "grey60",
         linetype = 2
       )) +
  xlab("coefficient estimate") + 
  ylab("") +
  ggtitle("") +
  theme(
    legend.justification = c(0, 0),
    legend.background = element_rect(colour = "grey80"),
    legend.title = element_blank()
  )
  
# adding Internet use + mobile phone use
dwplot(list(model6, model12))

dwplot(list(model6, model12),
       vline = geom_vline(
         xintercept = 0,
         colour = "grey60",
         linetype = 2
       )) +
  xlab("coefficient estimate") + 
  ylab("") +
  ggtitle("") +
  theme(
    legend.justification = c(0, 0),
    legend.background = element_rect(colour = "grey80"),
    legend.title = element_blank()
  ) +
  scale_y_discrete(labels=c(
    mobile = "Mobile phone",
    internet = "Internet",
    socmed = "Social media",
    post_fmix = "Mixed type",
    post_fpostmat = "Postmaterialism",
    edu_fhigh = "High school",
    "edu_fsome college" = "Some college",
    edu_fbachelor = "Bachelor or equivalent",
    "edu_fabove master" = "Above master's degree",
    interest = "Political interest",
    discuss = "Discussion",
    age = "Age",
    gender_ffemale = "Female",
    income = "Income",
    ideology = "Ideology")
  ) +
  scale_color_discrete(labels = c("Strikes", "Demonstrations"))


############################### END OF SCRIPT ##################################
################################################################################