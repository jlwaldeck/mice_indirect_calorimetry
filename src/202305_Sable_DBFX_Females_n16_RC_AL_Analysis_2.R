################June 2023 Sable RC DAT-BMAL1fx/fx Female Analysis
###########################################Author: Nathan Waldeck
#####################################################June 8, 2023
#################################################################

rm(list = ls())

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

detachAllPackages()

#again this thoroughly removes all packages and loaded namespaces except for base packages "base" and "utils" (which is highly not recommended).

library(multcompView)
library(multcomp)
library(plyr)
library(car)
library(emmeans)
library(nlme)
library(ggplot2)
library(rstatix)
library(tidyverse)
library(broom)
library(dplyr)
library(lme4)

se <- function(x)
  sd(x, na.rm = TRUE)/sqrt(length(x))

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = se(x[[col]]))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

superpose.eb <- function (x, y, ebl, ebu = ebl, length = 0.08, ...)
  arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3,
         length = length, ...)

# setwd("~/Desktop/OneDrive - Northwestern University/202305_Sable_DATBFX_Females_n16_AL_RC/")
# data <- read.csv("./202305_DATBFX_Females_RC_AL_Sable_n16.csv")

data <- read.csv("C:/Users/nwald/OneDrive/Desktop/202305_Sable_DATBFX_Females_n16_AL_RC/202305_DATBFX_Females_RC_AL_Sable_n16.csv")

data <-
  data %>%
  mutate(Genotype = case_when(Animal == "1" ~ 'BKO',
                             Animal == "2" ~ 'Ctrl',
                             Animal == "3" ~ 'BKO',
                             Animal == "4" ~ 'Ctrl',
                             Animal == "5" ~ 'BKO',
                             Animal == "6" ~ 'Ctrl',
                             Animal == "7" ~ 'BKO',
                             Animal == "8" ~ 'Ctrl',
                             Animal == "9" ~ 'BKO',
                             Animal == "10" ~ 'Ctrl',
                             Animal == "11" ~ 'BKO',
                             Animal == "12" ~ 'Ctrl',
                             Animal == "13" ~ 'BKO',
                             Animal == "14" ~ 'Ctrl',
                             Animal == "15" ~ 'BKO',
                             Animal == "16" ~ 'Ctrl',
                              TRUE ~ 'Low')) 
data <-
  data %>%
  mutate(four_hours = case_when(Cycle >= 40 ~ 'ZT20_ZT24',
                                Cycle >= 32 ~ 'ZT16_ZT20',
                                Cycle >= 24 ~ 'ZT12_ZT16',
                                Cycle >= 16 ~ 'ZT8_ZT12',
                                Cycle >= 8 ~ 'ZT4_ZT8',
                                Cycle >= 1 ~ 'ZT0_ZT4',
                                TRUE ~ 'Low')) 



data <-
  data %>%
  mutate(two_hours = case_when(Cycle >= 44 ~ 'ZT22_ZT24',
                               Cycle >= 40 ~ 'ZT20_ZT22',
                               Cycle >= 36 ~ 'ZT18_ZT20',
                               Cycle >= 32 ~ 'ZT16_ZT18',
                               Cycle >= 28 ~ 'ZT14_ZT16',
                               Cycle >= 24 ~ 'ZT12_ZT14',
                               Cycle >= 20 ~ 'ZT10_ZT12',
                               Cycle >= 16 ~ 'ZT8_ZT10',
                               Cycle >= 12 ~ 'ZT6_ZT8',
                               Cycle >= 8 ~ 'ZT4_ZT6',
                               Cycle >= 4 ~ 'ZT2_ZT4',
                               Cycle >= 1 ~ 'ZT0_ZT2',
                               TRUE ~ 'Low')) 

data <-
  data %>%
  mutate(one_hour = case_when(Cycle >= 47 ~ 'ZT23',
                              Cycle >= 45 ~ 'ZT22',
                              Cycle >= 43 ~ 'ZT21',
                              Cycle >= 41 ~ 'ZT20',
                              Cycle >= 39 ~ 'ZT19',
                              Cycle >= 37 ~ 'ZT18',
                              Cycle >= 35 ~ 'ZT17',
                              Cycle >= 33 ~ 'ZT16',
                              Cycle >= 31 ~ 'ZT15',
                              Cycle >= 29 ~ 'ZT14',
                              Cycle >= 27 ~ 'ZT13',
                              Cycle >= 25 ~ 'ZT12',
                              Cycle >= 23 ~ 'ZT11',
                              Cycle >= 21 ~ 'ZT10',
                              Cycle >= 19 ~ 'ZT9',
                              Cycle >= 17 ~ 'ZT8',
                              Cycle >= 15 ~ 'ZT7',
                              Cycle >= 13 ~ 'ZT6',
                              Cycle >= 11 ~ 'ZT5',
                              Cycle >= 9 ~ 'ZT4',
                              Cycle >= 7 ~ 'ZT3',
                              Cycle >= 5 ~ 'ZT2',
                              Cycle >= 3 ~ 'ZT1',
                              Cycle >= 1 ~ 'ZT0',
                              TRUE ~ 'Low')) 

data <-
  data %>%
  mutate(day_night = case_when(Cycle >= 25 ~ 'Dark',
                               Cycle >= 1 ~ 'Light',
                               TRUE ~ 'Low')) 

cols_4hrs <- c("Animal","StartDate","four_hours")

data<- 
  data %>% 
  group_by(across(all_of(cols_4hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(UptakeASum_4hrs = sum(UptakeA_Sum)) %>%
  ungroup

cols_LD <- c("Animal","StartDate","day_night")

data<- 
  data %>% 
  group_by(across(all_of(cols_LD))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(UptakeASum_LD = sum(UptakeA_Sum)) %>%
  ungroup

cols_Daily <- c("Animal","StartDate")

data<- 
  data %>% 
  group_by(across(all_of(cols_Daily))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(UptakeASum_Daily = sum(UptakeA_Sum)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_4hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(AllMeters_4hrs = sum(AllMeters)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_LD))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(AllMeters_LD = sum(AllMeters)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_Daily))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(AllMeters_Daily = sum(AllMeters)) %>%
  ungroup

data<-
  data %>%
  group_by(across(all_of(cols_4hrs))) %>%
  mutate(index = cur_group_id()) %>%
  mutate(Sleep_pct_4hrs = mean(Sleep_pct)) %>%
  ungroup

data<-
  data %>%
  group_by(across(all_of(cols_LD))) %>%
  mutate(index = cur_group_id()) %>%
  mutate(Sleep_pct_LD = mean(Sleep_pct)) %>%
  ungroup

data<-
  data %>%
  group_by(across(all_of(cols_Daily))) %>%
  mutate(index = cur_group_id()) %>%
  mutate(Sleep_pct_Daily = mean(Sleep_pct)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_4hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_VO2_4hrs = mean(Avg_VO2)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_LD))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_VO2_LD = mean(Avg_VO2)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_Daily))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_VO2_Daily = mean(Avg_VO2)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_4hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_RER_4hrs = mean(Avg_RQ)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_LD))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_RER_LD = mean(Avg_RQ)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_Daily))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_RER_Daily = mean(Avg_RQ)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_4hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_EE_4hrs = mean(Avg_EE)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_LD))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_EE_LD = mean(Avg_EE)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_Daily))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_EE_Daily = mean(Avg_EE)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_4hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Sleep_hrs_4hrs = sum(Sleep_hrs)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_LD))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Sleep_hrs_LD = sum(Sleep_hrs)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_Daily))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Sleep_hrs_Daily = sum(Sleep_hrs)) %>%
  ungroup

#data2 <- data[-which(data$id_code == "8" | data$id_code == "9"),]

aggregate(UptakeA_Sum ~ Animal:Genotype, data = data, FUN = mean)
aggregate(UptakeA_Sum ~ Animal, data = data, FUN = sd)

# 4 Hour Resolution

## Exclude acclimation time (4 days)

unique(data$StartDate)

acclimation <- c("5/22/23","5/23/23","5/24/23","5/25/24","5/30/23")
#excluded <- c("2", "3", "8")

data2 <- data[-which(data$StartDate %in% acclimation),]


aggregate(UptakeA_Sum ~ Animal:Genotype:StartDate:day_night, data = data, FUN = mean)
aggregate(UptakeA_Sum ~ Animal:Genotype, data = data2, FUN = mean)

data2$Cycle <- as.factor(data2$Cycle)

aggregate(AllMeters ~ Animal, data = data, FUN = mean)
aggregate(UptakeA_Sum ~ Animal, data = data2, FUN = mean)

# var.test(InitMass_g ~ Genotype, data = data2)
# t.test(InitMass_g ~ Genotype, data = data2, var.equal = F)

# aggregate(InitMass_g ~ Genotype, data = data2, FUN = mean)
# aggregate(InitMass_g ~ Genotype, data = data2, FUN = sd)

#data2 <- data2[-which(data$id_code %in% excluded),]


# FoodIntake_4hrs <- glm(UptakeA_Sum ~ Genotype + four_hours, data=data2)
FoodIntake_4hrs <- lmer(UptakeASum_4hrs ~ Genotype*four_hours + (1|Animal), data=data2, REML = TRUE)

Anova(FoodIntake_4hrs)

Food_Intake_emmeans_4hrs <- emmeans(FoodIntake_4hrs, list(pairwise ~ Genotype:four_hours), adjust = "fdr")

Food_Intake_lsmeans_4hrs <- Food_Intake_emmeans_4hrs$`emmeans of Genotype, four_hours`

Food_Intake_lsmeans_4hrs <- as.data.frame(Food_Intake_lsmeans_4hrs)

Food_Intake_contrasts_4hrs <- as.data.frame(Food_Intake_emmeans_4hrs$`pairwise differences of Genotype, four_hours`)

print(Food_Intake_lsmeans_4hrs)

Food_Intake_lsmeans_4hrs$emmean <- Food_Intake_lsmeans_4hrs$emmean
Food_Intake_lsmeans_4hrs$SE <- Food_Intake_lsmeans_4hrs$SE
Food_Intake_lsmeans_4hrs <- Food_Intake_lsmeans_4hrs[with(Food_Intake_lsmeans_4hrs, order(four_hours, Genotype)), ]
write.table(Food_Intake_contrasts_4hrs, file = "./output/202305_DBFX_Females_Sable_RC_LD_Food_Intake_contrasts_4hrs.csv")

pdf(file = "./output/202305_DBFX_Females_RC_4hrs_Feeding_Amount.pdf", height = 4, width = 6)

foodintake_4hrs_mean_mat <- t(matrix(c(subset(Food_Intake_lsmeans_4hrs, Genotype == "Ctrl" & four_hours == "ZT0_ZT4")$emmean, subset(Food_Intake_lsmeans_4hrs, Genotype == "Ctrl" & four_hours == "ZT4_ZT8")$emmean, subset(Food_Intake_lsmeans_4hrs, Genotype == "Ctrl" & four_hours == "ZT8_ZT12")$emmean, subset(Food_Intake_lsmeans_4hrs, Genotype == "Ctrl" & four_hours == "ZT12_ZT16")$emmean, subset(Food_Intake_lsmeans_4hrs, Genotype == "Ctrl" & four_hours == "ZT16_ZT20")$emmean, subset(Food_Intake_lsmeans_4hrs, Genotype == "Ctrl" & four_hours == "ZT20_ZT24")$emmean,subset(Food_Intake_lsmeans_4hrs, Genotype == "BKO" & four_hours == "ZT0_ZT4")$emmean, subset(Food_Intake_lsmeans_4hrs, Genotype == "BKO" & four_hours == "ZT4_ZT8")$emmean, subset(Food_Intake_lsmeans_4hrs, Genotype == "BKO" & four_hours == "ZT8_ZT12")$emmean, subset(Food_Intake_lsmeans_4hrs, Genotype == "BKO" & four_hours == "ZT12_ZT16")$emmean, subset(Food_Intake_lsmeans_4hrs, Genotype == "BKO" & four_hours == "ZT16_ZT20")$emmean, subset(Food_Intake_lsmeans_4hrs, Genotype == "BKO" & four_hours == "ZT20_ZT24")$emmean), 6, 2)) # create a 4-by-2 matrix, then transpose it
colnames(foodintake_4hrs_mean_mat) <- c("0-4", "4-8", "8-12", "12-16", "16-20", "20-24")
rownames(foodintake_4hrs_mean_mat) <- c("Control","BKO")

foodintake_4hrs_se_mat <- t(matrix(c(subset(Food_Intake_lsmeans_4hrs, Genotype == "Ctrl" & four_hours == "ZT0_ZT4")$SE, subset(Food_Intake_lsmeans_4hrs, Genotype == "Ctrl" & four_hours == "ZT4_ZT8")$SE, subset(Food_Intake_lsmeans_4hrs, Genotype == "Ctrl" & four_hours == "ZT8_ZT12")$SE, subset(Food_Intake_lsmeans_4hrs, Genotype == "Ctrl" & four_hours == "ZT12_ZT16")$SE, subset(Food_Intake_lsmeans_4hrs, Genotype == "Ctrl" & four_hours == "ZT16_ZT20")$SE, subset(Food_Intake_lsmeans_4hrs, Genotype == "Ctrl" & four_hours == "ZT20_ZT24")$SE,subset(Food_Intake_lsmeans_4hrs, Genotype == "BKO" & four_hours == "ZT0_ZT4")$SE, subset(Food_Intake_lsmeans_4hrs, Genotype == "BKO" & four_hours == "ZT4_ZT8")$SE, subset(Food_Intake_lsmeans_4hrs, Genotype == "BKO" & four_hours == "ZT8_ZT12")$SE, subset(Food_Intake_lsmeans_4hrs, Genotype == "BKO" & four_hours == "ZT12_ZT16")$SE, subset(Food_Intake_lsmeans_4hrs, Genotype == "BKO" & four_hours == "ZT16_ZT20")$SE, subset(Food_Intake_lsmeans_4hrs, Genotype == "BKO" & four_hours == "ZT20_ZT24")$SE), 6, 2)) # create a 4-by-2 matrix, then transpose it
colnames(foodintake_4hrs_se_mat) <- c("0-4", "4-8", "8-12", "12-16", "16-20", "20-24")
rownames(foodintake_4hrs_se_mat) <- c("Control","BKO")

fillcolours = c("white","red")
x.abscis <- barplot(
  foodintake_4hrs_mean_mat, beside=TRUE,
  col=fillcolours,
  space=c(0.1,0.3), # spacing between bars in the same group, and then between groups
  width=0.2, # bar widths
  ylim=c(0,1.5),
  yaxp=c(0,1.5,10),
  ylab="Food Consumed (g)",
  xlim=c(0.1,3), # makes sense in the context of width and space parameters
  xlab="Time Point",
  axis.lty=1, # enable tick marks on the X axis
  main="Food Consumption (4 hours)",
  font.lab=2 # bold for axis labels
)
box(bty="L")
superpose.eb(x.abscis, foodintake_4hrs_mean_mat, ebl=0, ebu=foodintake_4hrs_se_mat) # +1 SEM, no descending error bar
legend(x=2, y=1, box.lty=0, legend=rownames(foodintake_4hrs_mean_mat), fill=fillcolours, y.intersp=1)
dev.off()

##### Food Intake Analysis (Day vs Night)

FoodIntake_LD <- lmer(UptakeASum_LD ~ Genotype*day_night + (1|Animal), data=data2, REML = TRUE)

Anova(FoodIntake_LD)

Food_Intake_emmeans_LD <- emmeans(FoodIntake_LD, list(pairwise ~ Genotype:day_night), adjust = "fdr")

Food_Intake_lsmeans_LD <- Food_Intake_emmeans_LD$`emmeans of Genotype, day_night`

Food_Intake_lsmeans_LD <- as.data.frame(Food_Intake_lsmeans_LD)

Food_Intake_contrasts_LD <- as.data.frame(Food_Intake_emmeans_LD$`pairwise differences of Genotype, day_night`)

print(Food_Intake_lsmeans_LD)

Food_Intake_lsmeans_LD$emmean <- Food_Intake_lsmeans_LD$emmean
Food_Intake_lsmeans_LD$SE <- Food_Intake_lsmeans_LD$SE
Food_Intake_lsmeans_LD <- Food_Intake_lsmeans_LD[with(Food_Intake_lsmeans_LD, order(day_night, Genotype)), ]
write.table(Food_Intake_contrasts_LD, file = "./output/202305_DBFX_Females_Sable_RC_LD_Food_Intake_contrasts_LD.csv")
###  FDR-adjusted comparisons

print(Food_Intake_lsmeans_LD)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_LD_Feeding_Amount.pdf", height = 4, width = 6)

foodintake_LD_mean_mat <- t(matrix(c(subset(Food_Intake_lsmeans_LD, Genotype == "Ctrl" & day_night == "Dark")$emmean, subset(Food_Intake_lsmeans_LD, Genotype == "Ctrl" & day_night == "Light")$emmean, subset(Food_Intake_lsmeans_LD, Genotype == "BKO" & day_night == "Dark")$emmean, subset(Food_Intake_lsmeans_LD, Genotype == "BKO" & day_night == "Light")$emmean), 2, 2)) # create a 2-by-2 matrix, then transpose it
colnames(foodintake_LD_mean_mat) <- c("Dark", "Light")
rownames(foodintake_LD_mean_mat) <- c("Control","BKO")

foodintake_LD_se_mat <- t(matrix(c(subset(Food_Intake_lsmeans_LD, Genotype == "Ctrl" & day_night == "Dark")$SE, subset(Food_Intake_lsmeans_LD, Genotype == "Ctrl" & day_night == "Light")$SE, subset(Food_Intake_lsmeans_LD, Genotype == "BKO" & day_night == "Dark")$SE, subset(Food_Intake_lsmeans_LD, Genotype == "BKO" & day_night == "Light")$SE), 2, 2)) # create a 2-by-2 matrix, then transpose it
colnames(foodintake_LD_mean_mat) <- c("Dark", "Light")
rownames(foodintake_LD_mean_mat) <- c("Control","BKO")

fillcolours = c("dark violet","gray")
x.abscis <- barplot(
  foodintake_LD_mean_mat, beside=TRUE,
  col=fillcolours,
  space=c(0.1,0.4), # spacing between bars in the same group, and then between groups
  width=0.3, # bar widths
  ylim=c(0,3),
  yaxp=c(0,3,12),
  ylab="Average Food Consumed (g)",
  xlim=c(0.1,2), # makes sense in the context of width and space parameters
  xlab="Time Point",
  axis.lty=1, # enable tick marks on the X axis
  main="Average Total Food Consumption (Day vs Night)",
  font.lab=2 # bold for axis labels
)
box(bty="L")
superpose.eb(x.abscis, foodintake_LD_mean_mat, ebl=0, ebu=foodintake_LD_se_mat) # +1 SEM, no descending error bar
legend(x=1.5, y=2, box.lty=0, legend=rownames(foodintake_LD_mean_mat), fill=fillcolours, y.intersp=1)
dev.off()

## Total Daily Food Intake

aggregate(UptakeASum_Daily ~ Animal:StartDate, data = data2, FUN = mean)

FoodIntake_Daily <- lmer(UptakeASum_Daily ~ Genotype + (1|Animal), data=data2, REML=FALSE)

Anova(FoodIntake_Daily)

var.test(UptakeASum_Daily ~ Genotype, data=data2)
t.test(UptakeASum_Daily ~ Genotype, data=data2, var.equal = F)

Food_Intake_emmeans_Daily <- emmeans(FoodIntake_Daily, list(pairwise ~ Genotype), adjust = "tukey")

Food_Intake_lsmeans_Daily <- Food_Intake_emmeans_Daily$`emmeans of Genotype`

Food_Intake_lsmeans_Daily <- as.data.frame(Food_Intake_lsmeans_Daily)

Food_Intake_contrasts_Daily <- as.data.frame(Food_Intake_emmeans_Daily$`pairwise differences of Genotype`)

print(Food_Intake_lsmeans_Daily)

Food_Intake_lsmeans_Daily$emmean <- Food_Intake_lsmeans_Daily$emmean
Food_Intake_lsmeans_Daily$SE <- Food_Intake_lsmeans_Daily$SE
Food_Intake_lsmeans_Daily <- Food_Intake_lsmeans_Daily[with(Food_Intake_lsmeans_Daily, order(Genotype)), ]
write.table(Food_Intake_contrasts_Daily, file = "./output/202305_DBFX_Females_Sable_RC_Daily_Food_Intake_contrasts_Daily.csv")
###  FDR-adjusted comparisons

pdf(file = "./output/202305_DBFX_Females_Sable_RC_Daily_Feeding_Amount.pdf", height = 4, width = 6)

foodintake_Daily_mean_mat <- t(matrix(c(subset(Food_Intake_lsmeans_Daily, Genotype == "Ctrl")$emmean, subset(Food_Intake_lsmeans_Daily, Genotype == "BKO")$emmean), 1, 2)) # create a 2-by-2 matrix, then transpose it
rownames(foodintake_Daily_mean_mat) <- c("Control","BKO")

foodintake_Daily_se_mat <- t(matrix(c(subset(Food_Intake_lsmeans_Daily, Genotype == "Ctrl")$SE, subset(Food_Intake_lsmeans_Daily, Genotype == "BKO")$SE), 1, 2)) # create a 2-by-2 matrix, then transpose it
rownames(foodintake_Daily_se_mat) <- c("Control","BKO")

fillcolours = c("dark violet","gray")
x.abscis <- barplot(
  foodintake_Daily_mean_mat, beside=TRUE,
  col=fillcolours,
  space=c(0.4,0.4), # spacing between bars in the same group, and then between groups
  width=0.2, # bar widths
  ylim=c(0,5),
  yaxp=c(0,5,10),
  ylab="Average Food Consumed (g)",
  xlim=c(0,1.5), # makes sense in the context of width and space parameters
  xlab="Treatment",
  axis.lty=1, # enable tick marks on the X axis
  main="Average Total Daily Food Consumption",
  font.lab=2 # boDaily for axis labels
)
box(bty="L")
superpose.eb(x.abscis, foodintake_Daily_mean_mat, ebl=0, ebu=foodintake_Daily_se_mat) # +1 SEM, no descending error bar
legend(x=1, y=3, box.lty=0, legend=rownames(foodintake_Daily_mean_mat), fill=fillcolours, y.intersp=1)
dev.off()

## Average Food Intake by Mouse

aggregate(UptakeASum_Daily ~ Animal:StartDate, data = data2, FUN = mean)
aggregate(UptakeASum_Daily ~ Animal:StartDate, data = data2, FUN = sd)

# Activity 4 Hour Resolution

#data_activity <- data2[-which(data2$Animal == "3"),]

aggregate(AllMeters ~ Animal:Genotype:StartDate:day_night, data = data2, FUN = mean)
aggregate(AllMeters ~ Animal, data = data2, FUN = sd)

Activity_4hrs <- lmer(AllMeters_4hrs ~ Genotype*four_hours + (1|Animal), data=data2, REML = FALSE)

Anova(Activity_4hrs)

Activity_emmeans_4hrs <- emmeans(Activity_4hrs, list(pairwise ~ Genotype:four_hours), adjust = "tukey")

Activity_lsmeans_4hrs <- Activity_emmeans_4hrs$`emmeans of Genotype, four_hours`

Activity_lsmeans_4hrs <- as.data.frame(Activity_lsmeans_4hrs)

Activity_contrasts_4hrs <- as.data.frame(Activity_emmeans_4hrs$`pairwise differences of Genotype, four_hours`)

print(Activity_lsmeans_4hrs)

Activity_lsmeans_4hrs$emmean <- Activity_lsmeans_4hrs$emmean
Activity_lsmeans_4hrs$SE <- Activity_lsmeans_4hrs$SE
Activity_lsmeans_4hrs <- Activity_lsmeans_4hrs[with(Activity_lsmeans_4hrs, order(four_hours, Genotype)), ]
write.table(Activity_contrasts_4hrs, file = "./output/202305_DBFX_Females_Sable_RC_LD_Activity_contrasts_4hrs.csv")

Activity_lsmeans_4hrs <- Activity_lsmeans_4hrs[order(factor(Activity_lsmeans_4hrs$four_hours, levels = c('ZT0_ZT4', 'ZT4_ZT8', 'ZT8_ZT12', 'ZT12_ZT16', 'ZT16_ZT20', 'ZT20_ZT24'))),]

Activity_lsmeans_4hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)

print(Activity_lsmeans_4hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_8WKRC_Activity_4hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Activity_lsmeans_4hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Activity")
theme_set(opar)  
dev.off()

##### Activity Analysis (Day vs Night)
################################################START HERE
Activity_LD <- lmer(AllMeters_LD ~ Genotype*day_night + (1|Animal), data=data2, REML = TRUE)

Anova(Activity_LD)

Activity_emmeans_LD <- emmeans(Activity_LD, list(pairwise ~ Genotype:day_night), adjust = "fdr")

Activity_lsmeans_LD <- Activity_emmeans_LD$`emmeans of Genotype, day_night`

Activity_lsmeans_LD <- as.data.frame(Activity_lsmeans_LD)

Activity_contrasts_LD <- as.data.frame(Activity_emmeans_LD$`pairwise differences of Genotype, day_night`)

print(Activity_lsmeans_LD)

Activity_lsmeans_LD$emmean <- Activity_lsmeans_LD$emmean
Activity_lsmeans_LD$SE <- Activity_lsmeans_LD$SE
Activity_lsmeans_LD <- Activity_lsmeans_LD[with(Activity_lsmeans_LD, order(day_night, Genotype)), ]
write.table(Activity_contrasts_LD, file = "./output/202305_DBFX_Females_Sable_RC_LD_Activity_contrasts_LD.csv")

pdf(file = "./output/202305_DBFX_Females_Sable_RC_LD_Activity.pdf", height = 4, width = 6)

Activity_LD_mean_mat <- t(matrix(c(subset(Activity_lsmeans_LD, Genotype == "Ctrl" & day_night == "Dark")$emmean, subset(Activity_lsmeans_LD, Genotype == "Ctrl" & day_night == "Light")$emmean, subset(Activity_lsmeans_LD, Genotype == "BKO" & day_night == "Dark")$emmean, subset(Activity_lsmeans_LD, Genotype == "BKO" & day_night == "Light")$emmean), 2, 2)) # create a 2-by-2 matrix, then transpose it
colnames(Activity_LD_mean_mat) <- c("Dark", "Light")
rownames(Activity_LD_mean_mat) <- c("Control","BKO")

Activity_LD_se_mat <- t(matrix(c(subset(Activity_lsmeans_LD, Genotype == "Ctrl" & day_night == "Dark")$SE, subset(Activity_lsmeans_LD, Genotype == "Ctrl" & day_night == "Light")$SE, subset(Activity_lsmeans_LD, Genotype == "BKO" & day_night == "Dark")$SE, subset(Activity_lsmeans_LD, Genotype == "BKO" & day_night == "Light")$SE), 2, 2)) # create a 2-by-2 matrix, then transpose it
colnames(Activity_LD_mean_mat) <- c("Dark", "Light")
rownames(Activity_LD_mean_mat) <- c("Control","BKO")

fillcolours = c("dark violet","gray")
x.abscis <- barplot(
  Activity_LD_mean_mat, beside=TRUE,
  col=fillcolours,
  space=c(0.1,0.4), # spacing between bars in the same group, and then between groups
  width=0.3, # bar widths
  ylim=c(0,400),
  yaxp=c(0,400,10),
  ylab="IR Beam Breaks",
  xlim=c(0.1,2), # makes sense in the context of width and space parameters
  xlab="Time Point",
  axis.lty=1, # enable tick marks on the X axis
  main="Activity Counts Day and Night",
  font.lab=2 # bold for axis labels
)
box(bty="L")
superpose.eb(x.abscis, Activity_LD_mean_mat, ebl=0, ebu=Activity_LD_se_mat) # +1 SEM, no descending error bar
legend(x=1.5, y=2, box.lty=0, legend=rownames(Activity_LD_mean_mat), fill=fillcolours, y.intersp=1)
dev.off()

## Total Daily Activity

Activity_Daily <- lmer(AllMeters_Daily ~ Genotype + (1|StartDate) + (1|Animal), data=data2, REML = TRUE)

Anova(Activity_Daily)

Activity_emmeans_Daily <- emmeans(Activity_Daily, list(pairwise ~ Genotype), adjust = "tukey")

Activity_lsmeans_Daily <- Activity_emmeans_Daily$`emmeans of Genotype`

Activity_lsmeans_Daily <- as.data.frame(Activity_lsmeans_Daily)

Activity_contrasts_Daily <- as.data.frame(Activity_emmeans_Daily$`pairwise differences of Genotype`)

print(Activity_lsmeans_Daily)

Activity_lsmeans_Daily$emmean <- Activity_lsmeans_Daily$emmean
Activity_lsmeans_Daily$SE <- Activity_lsmeans_Daily$SE
Activity_lsmeans_Daily <- Activity_lsmeans_Daily[with(Activity_lsmeans_Daily, order(Genotype)), ]
write.table(Activity_contrasts_Daily, file = "./output/202305_DBFX_Females_Sable_RC_Daily_Activity_contrasts_Daily.csv")

pdf(file = "./output/202305_DBFX_Females_Sable_RC_Activity_Amount.pdf", height = 4, width = 6)

Activity_Daily_mean_mat <- t(matrix(c(subset(Activity_lsmeans_Daily, Genotype == "Ctrl")$emmean, subset(Activity_lsmeans_Daily, Genotype == "BKO")$emmean), 1, 2)) # create a 2-by-2 matrix, then transpose it
rownames(Activity_Daily_mean_mat) <- c("Control","BKO")

Activity_Daily_se_mat <- t(matrix(c(subset(Activity_lsmeans_Daily, Genotype == "Ctrl")$SE, subset(Activity_lsmeans_Daily, Genotype == "BKO")$SE), 1, 2)) # create a 2-by-2 matrix, then transpose it
rownames(Activity_Daily_se_mat) <- c("Control","BKO")

fillcolours = c("dark violet","gray")
x.abscis <- barplot(
  Activity_Daily_mean_mat, beside=TRUE,
  col=fillcolours,
  space=c(0.4,0.4), # spacing between bars in the same group, and then between groups
  width=0.2, # bar widths
  ylim=c(0,800),
  yaxp=c(0,800,10),
  ylab="IR Beam Breaks",
  xlim=c(0,1.5), # makes sense in the context of width and space parameters
  xlab="Treatment",
  axis.lty=1, # enable tick marks on the X axis
  main="Average Daily Activity Counts",
  font.lab=2 # boDaily for axis labels
)
box(bty="L")
superpose.eb(x.abscis, Activity_Daily_mean_mat, ebl=0, ebu=Activity_Daily_se_mat) # +1 SEM, no descending error bar
legend(x=1, y=3, box.lty=0, legend=rownames(Activity_Daily_mean_mat), fill=fillcolours, y.intersp=1)
dev.off()

## Average Activity by Mouse


aggregate(AllMeters_Daily ~ Animal:StartDate, data = data, FUN = mean)
aggregate(AllMeters ~ Animal, data = data, FUN = sd)

###### Sleep_Percent Analysis (4 hours)

Sleep_Percent_4hrs <- lmer(Sleep_pct_4hrs ~ Genotype*four_hours + (1|Animal), data=data2, REML = TRUE)

Anova(Sleep_Percent_4hrs)

Sleep_Percent_emmeans_4hrs <- emmeans(Sleep_Percent_4hrs, list(pairwise ~ Genotype:four_hours), adjust = "fdr")

Sleep_Percent_lsmeans_4hrs <- Sleep_Percent_emmeans_4hrs$`emmeans of Genotype, four_hours`

Sleep_Percent_lsmeans_4hrs <- as.data.frame(Sleep_Percent_lsmeans_4hrs)

Sleep_Percent_contrasts_4hrs <- as.data.frame(Sleep_Percent_emmeans_4hrs$`pairwise differences of Genotype, four_hours`)

print(Sleep_Percent_lsmeans_4hrs)

Sleep_Percent_lsmeans_4hrs$emmean <- Sleep_Percent_lsmeans_4hrs$emmean
Sleep_Percent_lsmeans_4hrs$SE <- Sleep_Percent_lsmeans_4hrs$SE
Sleep_Percent_lsmeans_4hrs <- Sleep_Percent_lsmeans_4hrs[with(Sleep_Percent_lsmeans_4hrs, order(four_hours, Genotype)), ]
write.table(Sleep_Percent_contrasts_4hrs, file = "./output/202305_DBFX_Females_Sable_RC_LD_Sleep_Percent_contrasts_4hrs.csv")

Sleep_Percent_lsmeans_4hrs <- Sleep_Percent_lsmeans_4hrs[order(factor(Sleep_Percent_lsmeans_4hrs$four_hours, levels = c('ZT0_ZT4', 'ZT4_ZT8', 'ZT8_ZT12', 'ZT12_ZT16', 'ZT16_ZT20', 'ZT20_ZT24'))),]

Sleep_Percent_lsmeans_4hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)

print(Sleep_Percent_lsmeans_4hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_8WKRC_Sleep_Percent_4hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Sleep_Percent_lsmeans_4hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Sleep_Percent")
theme_set(opar)  
dev.off()

##### Sleep_Percent Analysis (Day vs Night)

Sleep_Percent_LD <- lmer(Sleep_pct_LD ~ Genotype*day_night + (1|Animal), data=data2, REML = TRUE)

Anova(Sleep_Percent_LD)

Sleep_Percent_emmeans_LD <- emmeans(Sleep_Percent_LD, list(pairwise ~ Genotype:day_night), adjust = "fdr")

Sleep_Percent_lsmeans_LD <- Sleep_Percent_emmeans_LD$`emmeans of Genotype, day_night`

Sleep_Percent_lsmeans_LD <- as.data.frame(Sleep_Percent_lsmeans_LD)

Sleep_Percent_contrasts_LD <- as.data.frame(Sleep_Percent_emmeans_LD$`pairwise differences of Genotype, day_night`)

print(Sleep_Percent_lsmeans_LD)

Sleep_Percent_lsmeans_LD$emmean <- Sleep_Percent_lsmeans_LD$emmean
Sleep_Percent_lsmeans_LD$SE <- Sleep_Percent_lsmeans_LD$SE
Sleep_Percent_lsmeans_LD <- Sleep_Percent_lsmeans_LD[with(Sleep_Percent_lsmeans_LD, order(day_night, Genotype)), ]
write.table(Sleep_Percent_contrasts_LD, file = "./output/202305_DBFX_Females_Sable_RC_LD_Sleep_Percent_contrasts_LD.csv")

pdf(file = "./output/202305_DBFX_Females_Sable_RC_LD_Sleep_Percent.pdf", height = 4, width = 6)

Sleep_Percent_LD_mean_mat <- t(matrix(c(subset(Sleep_Percent_lsmeans_LD, Genotype == "Ctrl" & day_night == "Dark")$emmean, subset(Sleep_Percent_lsmeans_LD, Genotype == "Ctrl" & day_night == "Light")$emmean, subset(Sleep_Percent_lsmeans_LD, Genotype == "BKO" & day_night == "Dark")$emmean, subset(Sleep_Percent_lsmeans_LD, Genotype == "BKO" & day_night == "Light")$emmean), 2, 2)) # create a 2-by-2 matrix, then transpose it
colnames(Sleep_Percent_LD_mean_mat) <- c("Dark", "Light")
rownames(Sleep_Percent_LD_mean_mat) <- c("Control","BKO")

Sleep_Percent_LD_se_mat <- t(matrix(c(subset(Sleep_Percent_lsmeans_LD, Genotype == "Ctrl" & day_night == "Dark")$SE, subset(Sleep_Percent_lsmeans_LD, Genotype == "Ctrl" & day_night == "Light")$SE, subset(Sleep_Percent_lsmeans_LD, Genotype == "BKO" & day_night == "Dark")$SE, subset(Sleep_Percent_lsmeans_LD, Genotype == "BKO" & day_night == "Light")$SE), 2, 2)) # create a 2-by-2 matrix, then transpose it
colnames(Sleep_Percent_LD_mean_mat) <- c("Dark", "Light")
rownames(Sleep_Percent_LD_mean_mat) <- c("Control","BKO")

fillcolours = c("dark violet","gray")
x.abscis <- barplot(
  Sleep_Percent_LD_mean_mat, beside=TRUE,
  col=fillcolours,
  space=c(0.1,0.4), # spacing between bars in the same group, and then between groups
  width=0.3, # bar widths
  ylim=c(0,100),
  yaxp=c(0,100,5),
  ylab="IR Beam Breaks",
  xlim=c(0.1,2), # makes sense in the context of width and space parameters
  xlab="Time Point",
  axis.lty=1, # enable tick marks on the X axis
  main="Sleep_Percent Counts Day and Night",
  font.lab=2 # bold for axis labels
)
box(bty="L")
superpose.eb(x.abscis, Sleep_Percent_LD_mean_mat, ebl=0, ebu=Sleep_Percent_LD_se_mat) # +1 SEM, no descending error bar
legend(x=1.5, y=2, box.lty=0, legend=rownames(Sleep_Percent_LD_mean_mat), fill=fillcolours, y.intersp=1)
dev.off()

## Total Daily Sleep_Percent

Sleep_Percent_Daily <- lmer(Sleep_pct_Daily ~ Genotype + (1|StartDate) + (1|Animal), data=data2, REML = TRUE)

Anova(Sleep_Percent_Daily)

Sleep_Percent_emmeans_Daily <- emmeans(Sleep_Percent_Daily, list(pairwise ~ Genotype), adjust = "tukey")

Sleep_Percent_lsmeans_Daily <- Sleep_Percent_emmeans_Daily$`emmeans of Genotype`

Sleep_Percent_lsmeans_Daily <- as.data.frame(Sleep_Percent_lsmeans_Daily)

Sleep_Percent_contrasts_Daily <- as.data.frame(Sleep_Percent_emmeans_Daily$`pairwise differences of Genotype`)

print(Sleep_Percent_lsmeans_Daily)

Sleep_Percent_lsmeans_Daily$emmean <- Sleep_Percent_lsmeans_Daily$emmean
Sleep_Percent_lsmeans_Daily$SE <- Sleep_Percent_lsmeans_Daily$SE
Sleep_Percent_lsmeans_Daily <- Sleep_Percent_lsmeans_Daily[with(Sleep_Percent_lsmeans_Daily, order(Genotype)), ]
write.table(Sleep_Percent_contrasts_Daily, file = "./output/202305_DBFX_Females_Sable_RC_Daily_Sleep_Percent_contrasts_Daily.csv")

pdf(file = "./output/202305_DBFX_Females_Sable_RC_Sleep_Percent_Amount.pdf", height = 4, width = 6)

Sleep_Percent_Daily_mean_mat <- t(matrix(c(subset(Sleep_Percent_lsmeans_Daily, Genotype == "Ctrl")$emmean, subset(Sleep_Percent_lsmeans_Daily, Genotype == "BKO")$emmean), 1, 2)) # create a 2-by-2 matrix, then transpose it
rownames(Sleep_Percent_Daily_mean_mat) <- c("Control","BKO")

Sleep_Percent_Daily_se_mat <- t(matrix(c(subset(Sleep_Percent_lsmeans_Daily, Genotype == "Ctrl")$SE, subset(Sleep_Percent_lsmeans_Daily, Genotype == "BKO")$SE), 1, 2)) # create a 2-by-2 matrix, then transpose it
rownames(Sleep_Percent_Daily_se_mat) <- c("Control","BKO")

fillcolours = c("dark violet","gray")
x.abscis <- barplot(
  Sleep_Percent_Daily_mean_mat, beside=TRUE,
  col=fillcolours,
  space=c(0.4,0.4), # spacing between bars in the same group, and then between groups
  width=0.2, # bar widths
  ylim=c(0,250),
  yaxp=c(0,250,10),
  ylab="IR Beam Breaks",
  xlim=c(0,1.5), # makes sense in the context of width and space parameters
  xlab="Treatment",
  axis.lty=1, # enable tick marks on the X axis
  main="Average Daily Sleep_Percent Counts",
  font.lab=2 # boDaily for axis labels
)
box(bty="L")
superpose.eb(x.abscis, Sleep_Percent_Daily_mean_mat, ebl=0, ebu=Sleep_Percent_Daily_se_mat) # +1 SEM, no descending error bar
legend(x=1, y=3, box.lty=0, legend=rownames(Sleep_Percent_Daily_mean_mat), fill=fillcolours, y.intersp=1)
dev.off()

###### VO2 Analysis (4 hours)

VO2_4hrs <- lmer(Avg_VO2_4hrs ~ Genotype*four_hours + BodyMass_g + (1|Animal), data=data2, REML = TRUE)

Anova(VO2_4hrs)

VO2_emmeans_4hrs <- emmeans(VO2_4hrs, list(pairwise ~ Genotype:four_hours), adjust = "fdr")

VO2_lsmeans_4hrs <- VO2_emmeans_4hrs$`emmeans of Genotype, four_hours`

VO2_lsmeans_4hrs <- as.data.frame(VO2_lsmeans_4hrs)

VO2_contrasts_4hrs <- as.data.frame(VO2_emmeans_4hrs$`pairwise differences of Genotype, four_hours`)

print(VO2_lsmeans_4hrs)

VO2_lsmeans_4hrs$emmean <- VO2_lsmeans_4hrs$emmean
VO2_lsmeans_4hrs$SE <- VO2_lsmeans_4hrs$SE
VO2_lsmeans_4hrs <- VO2_lsmeans_4hrs[with(VO2_lsmeans_4hrs, order(four_hours, Genotype)), ]
write.table(VO2_contrasts_4hrs, file = "./output/202305_DBFX_Females_Sable_RC_LD_VO2_contrasts_4hrs.csv")

VO2_lsmeans_4hrs <- VO2_lsmeans_4hrs[order(factor(VO2_lsmeans_4hrs$four_hours, levels = c('ZT0_ZT4', 'ZT4_ZT8', 'ZT8_ZT12', 'ZT12_ZT16', 'ZT16_ZT20', 'ZT20_ZT24'))),]

VO2_lsmeans_4hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)

print(VO2_lsmeans_4hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_8WKRC_VO2_4hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(VO2_lsmeans_4hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("VO2")
theme_set(opar)  
dev.off()

##### VO2 Analysis (Day vs Night)

VO2_LD <- lmer(Avg_VO2_LD ~ Genotype*day_night + BodyMass_g + (1|Animal) + (1|StartDate), data=data2)

Anova(VO2_LD)

VO2_emmeans_LD <- emmeans(VO2_LD, list(pairwise ~ Genotype:day_night), adjust = "fdr")

VO2_lsmeans_LD <- VO2_emmeans_LD$`emmeans of Genotype, day_night`

VO2_lsmeans_LD <- as.data.frame(VO2_lsmeans_LD)

VO2_contrasts_LD <- as.data.frame(VO2_emmeans_LD$`pairwise differences of Genotype, day_night`)

print(VO2_lsmeans_LD)

VO2_lsmeans_LD$emmean <- VO2_lsmeans_LD$emmean
VO2_lsmeans_LD$SE <- VO2_lsmeans_LD$SE
VO2_lsmeans_LD <- VO2_lsmeans_LD[with(VO2_lsmeans_LD, order(day_night, Genotype)), ]
write.table(VO2_contrasts_LD, file = "./output/202305_DBFX_Females_Sable_RC_LD_VO2_contrasts_LD.csv")

pdf(file = "./output/202305_DBFX_Females_Sable_RC_LD_VO2.pdf", height = 4, width = 6)

VO2_LD_mean_mat <- t(matrix(c(subset(VO2_lsmeans_LD, Genotype == "Ctrl" & day_night == "Dark")$emmean, subset(VO2_lsmeans_LD, Genotype == "Ctrl" & day_night == "Light")$emmean, subset(VO2_lsmeans_LD, Genotype == "BKO" & day_night == "Dark")$emmean, subset(VO2_lsmeans_LD, Genotype == "BKO" & day_night == "Light")$emmean), 2, 2)) # create a 2-by-2 matrix, then transpose it
colnames(VO2_LD_mean_mat) <- c("Dark", "Light")
rownames(VO2_LD_mean_mat) <- c("Control","BKO")

VO2_LD_se_mat <- t(matrix(c(subset(VO2_lsmeans_LD, Genotype == "Ctrl" & day_night == "Dark")$SE, subset(VO2_lsmeans_LD, Genotype == "Ctrl" & day_night == "Light")$SE, subset(VO2_lsmeans_LD, Genotype == "BKO" & day_night == "Dark")$SE, subset(VO2_lsmeans_LD, Genotype == "BKO" & day_night == "Light")$SE), 2, 2)) # create a 2-by-2 matrix, then transpose it
colnames(VO2_LD_mean_mat) <- c("Dark", "Light")
rownames(VO2_LD_mean_mat) <- c("Control","BKO")

fillcolours = c("dark violet","gray")
x.abscis <- barplot(
  VO2_LD_mean_mat, beside=TRUE,
  col=fillcolours,
  space=c(0.1,0.4), # spacing between bars in the same group, and then between groups
  width=0.3, # bar widths
  ylim=c(0,150),
  yaxp=c(0,150,10),
  ylab="IR Beam Breaks",
  xlim=c(0.1,2), # makes sense in the context of width and space parameters
  xlab="Time Point",
  axis.lty=1, # enable tick marks on the X axis
  main="VO2 Counts Day and Night",
  font.lab=2 # bold for axis labels
)
box(bty="L")
superpose.eb(x.abscis, VO2_LD_mean_mat, ebl=0, ebu=VO2_LD_se_mat) # +1 SEM, no descending error bar
legend(x=1.5, y=2, box.lty=0, legend=rownames(VO2_LD_mean_mat), fill=fillcolours, y.intersp=1)
dev.off()

## Total Daily VO2

VO2_Daily <- lmer(Avg_VO2_Daily ~ Genotype + BodyMass_g + (1|StartDate) + (1|Animal), data=data2, REML = TRUE)

Anova(VO2_Daily)

VO2_emmeans_Daily <- emmeans(VO2_Daily, list(pairwise ~ Genotype), adjust = "tukey")

VO2_lsmeans_Daily <- VO2_emmeans_Daily$`emmeans of Genotype`

VO2_lsmeans_Daily <- as.data.frame(VO2_lsmeans_Daily)

VO2_contrasts_Daily <- as.data.frame(VO2_emmeans_Daily$`pairwise differences of Genotype`)

print(VO2_lsmeans_Daily)

VO2_lsmeans_Daily$emmean <- VO2_lsmeans_Daily$emmean
VO2_lsmeans_Daily$SE <- VO2_lsmeans_Daily$SE
VO2_lsmeans_Daily <- VO2_lsmeans_Daily[with(VO2_lsmeans_Daily, order(Genotype)), ]
write.table(VO2_contrasts_Daily, file = "./output/202305_DBFX_Females_Sable_RC_Daily_VO2_contrasts_Daily.csv")

pdf(file = "./output/202305_DBFX_Females_Sable_RC_VO2_Amount.pdf", height = 4, width = 6)

VO2_Daily_mean_mat <- t(matrix(c(subset(VO2_lsmeans_Daily, Genotype == "Ctrl")$emmean, subset(VO2_lsmeans_Daily, Genotype == "BKO")$emmean), 1, 2)) # create a 2-by-2 matrix, then transpose it
rownames(VO2_Daily_mean_mat) <- c("Control","BKO")

VO2_Daily_se_mat <- t(matrix(c(subset(VO2_lsmeans_Daily, Genotype == "Ctrl")$SE, subset(VO2_lsmeans_Daily, Genotype == "BKO")$SE), 1, 2)) # create a 2-by-2 matrix, then transpose it
rownames(VO2_Daily_se_mat) <- c("Control","BKO")

fillcolours = c("dark violet","gray")
x.abscis <- barplot(
  VO2_Daily_mean_mat, beside=TRUE,
  col=fillcolours,
  space=c(0.4,0.4), # spacing between bars in the same group, and then between groups
  width=0.2, # bar widths
  ylim=c(0,250),
  yaxp=c(0,250,10),
  ylab="IR Beam Breaks",
  xlim=c(0,1.5), # makes sense in the context of width and space parameters
  xlab="Treatment",
  axis.lty=1, # enable tick marks on the X axis
  main="Average Daily VO2 Counts",
  font.lab=2 # boDaily for axis labels
)
box(bty="L")
superpose.eb(x.abscis, VO2_Daily_mean_mat, ebl=0, ebu=VO2_Daily_se_mat) # +1 SEM, no descending error bar
legend(x=1, y=3, box.lty=0, legend=rownames(VO2_Daily_mean_mat), fill=fillcolours, y.intersp=1)
dev.off()


## Body Weight Analysis

# pdf(file = "./output/202305_DBFX_Females_Chronic_Body_Weights.pdf", height = 4, width = 6)

# var.test(InitMass_g ~ Genotype, data = data2) # commenting out since no InitMass_g variable in data2
# weight_ttest <- t.test(InitMass_g ~ Genotype, data = data2, var.equal = T)

# names(weight_ttest)

# Weight_mean_mat <- t(matrix(c(weight_ttest$estimate[2], weight_ttest$estimate[1]), 1, 2)) # create a 2-by-2 matrix, then transpose it
# rownames(Weight_mean_mat) <- c("Control", "BKO")

# Weight_se_mat <- t(matrix(c(weight_ttest$stderr[2], weight_ttest$stderr[1]), 1, 2)) # create a 2-by-2 matrix, then transpose it
# rownames(Weight_se_mat) <- c("Control","BKO")

# fillcolours = c("dark violet","gray")
# x.abscis <- barplot(
#   Weight_mean_mat, beside=TRUE,
#   col=fillcolours,
#   space=c(0.4,0.4), # spacing between bars in the same group, and then between groups
#   width=0.2, # bar widths
#   ylim=c(0,50),
#   yaxp=c(0,50,10),
#   ylab="Boddy Mass (g)",
#   xlim=c(0,1.5), # makes sense in the context of width and space parameters
#   xlab="Treatment",
#   axis.lty=1, # enable tick marks on the X axis
#   main="Body Mass by Group",
#   font.lab=2 # boDaily for axis labels
# )
# box(bty="L")
# superpose.eb(x.abscis, Weight_mean_mat, ebl=0, ebu=Weight_se_mat) # +1 SEM, no descending error bar
# legend(x=1, y=40, box.lty=0, legend=rownames(Weight_mean_mat), fill=fillcolours, y.intersp=1)
# dev.off()

###### RER Analysis (4 hours)

RER_4hrs <- lmer(Avg_RER_4hrs ~ Genotype*four_hours + (1|Animal), data=data2, REML = TRUE, control = lmerControl(optimizer ="Nelder_Mead"))

Anova(RER_4hrs)

RER_emmeans_4hrs <- emmeans(RER_4hrs, list(pairwise ~ Genotype:four_hours), adjust = "fdr")

RER_lsmeans_4hrs <- RER_emmeans_4hrs$`emmeans of Genotype, four_hours`

RER_lsmeans_4hrs <- as.data.frame(RER_lsmeans_4hrs)

RER_contrasts_4hrs <- as.data.frame(RER_emmeans_4hrs$`pairwise differences of Genotype, four_hours`)

print(RER_lsmeans_4hrs)

RER_lsmeans_4hrs$emmean <- RER_lsmeans_4hrs$emmean
RER_lsmeans_4hrs$SE <- RER_lsmeans_4hrs$SE
RER_lsmeans_4hrs <- RER_lsmeans_4hrs[with(RER_lsmeans_4hrs, order(four_hours, Genotype)), ]
write.table(RER_contrasts_4hrs, file = "./output/202305_DBFX_Females_Sable_RC_LD_RER_contrasts_4hrs.csv")

RER_lsmeans_4hrs <- RER_lsmeans_4hrs[order(factor(RER_lsmeans_4hrs$four_hours, levels = c('ZT0_ZT4', 'ZT4_ZT8', 'ZT8_ZT12', 'ZT12_ZT16', 'ZT16_ZT20', 'ZT20_ZT24'))),]

RER_lsmeans_4hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)

print(RER_lsmeans_4hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_8WKRC_RER_4hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(RER_lsmeans_4hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("RER")
theme_set(opar)  
dev.off()

##### RER Analysis (Day vs Night)

RER_LD <- lmer(Avg_RER_LD ~ Genotype*day_night + (StartDate|Animal), data=data2, REML = TRUE)

RER_emmeans_LD <- emmeans(RER_LD, list(pairwise ~ Genotype:day_night), adjust = "tukey")

RER_lsmeans_LD <- RER_emmeans_LD$`emmeans of Genotype, day_night`

RER_lsmeans_LD <- as.data.frame(RER_lsmeans_LD)

RER_contrasts_LD <- as.data.frame(RER_emmeans_LD$`pairwise differences of Genotype, day_night`)

print(RER_lsmeans_LD)

RER_lsmeans_LD$emmean <- RER_lsmeans_LD$emmean
RER_lsmeans_LD$SE <- RER_lsmeans_LD$SE
RER_lsmeans_LD <- RER_lsmeans_LD[with(RER_lsmeans_LD, order(day_night, Genotype)), ]
write.table(RER_contrasts_LD, file = "./output/202305_DBFX_Females_Sable_RC_LD_RER_contrasts_LD.csv")

pdf(file = "./output/202305_DBFX_Females_Sable_RC_LD_RER.pdf", height = 4, width = 6)

RER_LD_mean_mat <- t(matrix(c(subset(RER_lsmeans_LD, Genotype == "Ctrl" & day_night == "Dark")$emmean, subset(RER_lsmeans_LD, Genotype == "Ctrl" & day_night == "Light")$emmean, subset(RER_lsmeans_LD, Genotype == "BKO" & day_night == "Dark")$emmean, subset(RER_lsmeans_LD, Genotype == "BKO" & day_night == "Light")$emmean), 2, 2)) # create a 2-by-2 matrix, then transpose it
colnames(RER_LD_mean_mat) <- c("Dark", "Light")
rownames(RER_LD_mean_mat) <- c("Control","BKO")

RER_LD_se_mat <- t(matrix(c(subset(RER_lsmeans_LD, Genotype == "Ctrl" & day_night == "Dark")$SE, subset(RER_lsmeans_LD, Genotype == "Ctrl" & day_night == "Light")$SE, subset(RER_lsmeans_LD, Genotype == "BKO" & day_night == "Dark")$SE, subset(RER_lsmeans_LD, Genotype == "BKO" & day_night == "Light")$SE), 2, 2)) # create a 2-by-2 matrix, then transpose it
colnames(RER_LD_mean_mat) <- c("Dark", "Light")
rownames(RER_LD_mean_mat) <- c("Control","BKO")

fillcolours = c("dark violet","gray")
x.abscis <- barplot(
  RER_LD_mean_mat, beside=TRUE,
  col=fillcolours,
  space=c(0.1,0.4), # spacing between bars in the same group, and then between groups
  width=0.3, # bar widths
  ylim=c(0,150),
  yaxp=c(0,150,10),
  ylab="IR Beam Breaks",
  xlim=c(0.1,2), # makes sense in the context of width and space parameters
  xlab="Time Point",
  axis.lty=1, # enable tick marks on the X axis
  main="RER Counts Day and Night",
  font.lab=2 # bold for axis labels
)
box(bty="L")
superpose.eb(x.abscis, RER_LD_mean_mat, ebl=0, ebu=RER_LD_se_mat) # +1 SEM, no descending error bar
legend(x=1.5, y=2, box.lty=0, legend=rownames(RER_LD_mean_mat), fill=fillcolours, y.intersp=1)
dev.off()

## Total Daily RER

RER_Daily <- lmer(Avg_RER_Daily ~ Genotype + (1|StartDate) + (1|Animal), data=data2, REML = TRUE)

Anova(RER_Daily)

RER_emmeans_Daily <- emmeans(RER_Daily, list(pairwise ~ Genotype), adjust = "tukey")

RER_lsmeans_Daily <- RER_emmeans_Daily$`emmeans of Genotype`

RER_lsmeans_Daily <- as.data.frame(RER_lsmeans_Daily)

RER_contrasts_Daily <- as.data.frame(RER_emmeans_Daily$`pairwise differences of Genotype`)

print(RER_lsmeans_Daily)

RER_lsmeans_Daily$emmean <- RER_lsmeans_Daily$emmean
RER_lsmeans_Daily$SE <- RER_lsmeans_Daily$SE
RER_lsmeans_Daily <- RER_lsmeans_Daily[with(RER_lsmeans_Daily, order(Genotype)), ]
write.table(RER_contrasts_Daily, file = "./output/202305_DBFX_Females_Sable_RC_Daily_RER_contrasts_Daily.csv")

pdf(file = "./output/202305_DBFX_Females_Sable_RC_RER_Amount.pdf", height = 4, width = 6)

RER_Daily_mean_mat <- t(matrix(c(subset(RER_lsmeans_Daily, Genotype == "Ctrl")$emmean, subset(RER_lsmeans_Daily, Genotype == "BKO")$emmean), 1, 2)) # create a 2-by-2 matrix, then transpose it
rownames(RER_Daily_mean_mat) <- c("Control","BKO")

RER_Daily_se_mat <- t(matrix(c(subset(RER_lsmeans_Daily, Genotype == "Ctrl")$SE, subset(RER_lsmeans_Daily, Genotype == "BKO")$SE), 1, 2)) # create a 2-by-2 matrix, then transpose it
rownames(RER_Daily_se_mat) <- c("Control","BKO")

fillcolours = c("dark violet","gray")
x.abscis <- barplot(
  RER_Daily_mean_mat, beside=TRUE,
  col=fillcolours,
  space=c(0.4,0.4), # spacing between bars in the same group, and then between groups
  width=0.2, # bar widths
  ylim=c(0,250),
  yaxp=c(0,250,10),
  ylab="IR Beam Breaks",
  xlim=c(0,1.5), # makes sense in the context of width and space parameters
  xlab="Treatment",
  axis.lty=1, # enable tick marks on the X axis
  main="Average Daily RER Counts",
  font.lab=2 # boDaily for axis labels
)
box(bty="L")
superpose.eb(x.abscis, RER_Daily_mean_mat, ebl=0, ebu=RER_Daily_se_mat) # +1 SEM, no descending error bar
legend(x=1, y=3, box.lty=0, legend=rownames(RER_Daily_mean_mat), fill=fillcolours, y.intersp=1)
dev.off()

###### EE Analysis (4 hours)

EE_4hrs <- lmer(Avg_EE_4hrs ~ Genotype*four_hours + BodyMass_g + (StartDate|Animal), data=data2, REML = TRUE)

Anova(EE_4hrs)

EE_emmeans_4hrs <- emmeans(EE_4hrs, list(pairwise ~ Genotype:four_hours), adjust = "fdr")

EE_lsmeans_4hrs <- EE_emmeans_4hrs$`emmeans of Genotype, four_hours`

EE_lsmeans_4hrs <- as.data.frame(EE_lsmeans_4hrs)

EE_contrasts_4hrs <- as.data.frame(EE_emmeans_4hrs$`pairwise differences of Genotype, four_hours`)

print(EE_lsmeans_4hrs)

EE_lsmeans_4hrs$emmean <- EE_lsmeans_4hrs$emmean
EE_lsmeans_4hrs$SE <- EE_lsmeans_4hrs$SE
EE_lsmeans_4hrs <- EE_lsmeans_4hrs[with(EE_lsmeans_4hrs, order(four_hours, Genotype)), ]
write.table(EE_contrasts_4hrs, file = "./output/202305_DBFX_Females_Sable_RC_LD_EE_contrasts_4hrs.csv")

EE_lsmeans_4hrs <- EE_lsmeans_4hrs[order(factor(EE_lsmeans_4hrs$four_hours, levels = c('ZT0_ZT4', 'ZT4_ZT8', 'ZT8_ZT12', 'ZT12_ZT16', 'ZT16_ZT20', 'ZT20_ZT24'))),]

EE_lsmeans_4hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)

print(EE_lsmeans_4hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_8WKRC_EE_4hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(EE_lsmeans_4hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("EE")
theme_set(opar)  
dev.off()

##### EE Analysis (Day vs Night)

EE_LD <- lmer(Avg_EE_LD ~ Genotype*day_night + BodyMass_g + (StartDate|Animal), data=data2, REML = TRUE)

Anova(EE_LD)

EE_emmeans_LD <- emmeans(EE_LD, list(pairwise ~ Genotype:day_night), adjust = "fdr")

EE_lsmeans_LD <- EE_emmeans_LD$`emmeans of Genotype, day_night`

EE_lsmeans_LD <- as.data.frame(EE_lsmeans_LD)

EE_contrasts_LD <- as.data.frame(EE_emmeans_LD$`pairwise differences of Genotype, day_night`)

print(EE_lsmeans_LD)

EE_lsmeans_LD$emmean <- EE_lsmeans_LD$emmean
EE_lsmeans_LD$SE <- EE_lsmeans_LD$SE
EE_lsmeans_LD <- EE_lsmeans_LD[with(EE_lsmeans_LD, order(day_night, Genotype)), ]
write.table(EE_contrasts_LD, file = "./output/202305_DBFX_Females_Sable_RC_LD_EE_contrasts_LD.csv")

pdf(file = "./output/202305_DBFX_Females_Sable_RC_LD_EE.pdf", height = 4, width = 6)

EE_LD_mean_mat <- t(matrix(c(subset(EE_lsmeans_LD, Genotype == "Ctrl" & day_night == "Dark")$emmean, subset(EE_lsmeans_LD, Genotype == "Ctrl" & day_night == "Light")$emmean, subset(EE_lsmeans_LD, Genotype == "BKO" & day_night == "Dark")$emmean, subset(EE_lsmeans_LD, Genotype == "BKO" & day_night == "Light")$emmean), 2, 2)) # create a 2-by-2 matrix, then transpose it
colnames(EE_LD_mean_mat) <- c("Dark", "Light")
rownames(EE_LD_mean_mat) <- c("Control","BKO")

EE_LD_se_mat <- t(matrix(c(subset(EE_lsmeans_LD, Genotype == "Ctrl" & day_night == "Dark")$SE, subset(EE_lsmeans_LD, Genotype == "Ctrl" & day_night == "Light")$SE, subset(EE_lsmeans_LD, Genotype == "BKO" & day_night == "Dark")$SE, subset(EE_lsmeans_LD, Genotype == "BKO" & day_night == "Light")$SE), 2, 2)) # create a 2-by-2 matrix, then transpose it
colnames(EE_LD_mean_mat) <- c("Dark", "Light")
rownames(EE_LD_mean_mat) <- c("Control","BKO")

fillcolours = c("dark violet","gray")
x.abscis <- barplot(
  EE_LD_mean_mat, beside=TRUE,
  col=fillcolours,
  space=c(0.1,0.4), # spacing between bars in the same group, and then between groups
  width=0.3, # bar widths
  ylim=c(0,150),
  yaxp=c(0,150,10),
  ylab="IR Beam Breaks",
  xlim=c(0.1,2), # makes sense in the context of width and space parameters
  xlab="Time Point",
  axis.lty=1, # enable tick marks on the X axis
  main="EE Counts Day and Night",
  font.lab=2 # bold for axis labels
)
box(bty="L")
superpose.eb(x.abscis, EE_LD_mean_mat, ebl=0, ebu=EE_LD_se_mat) # +1 SEM, no descending error bar
legend(x=1.5, y=2, box.lty=0, legend=rownames(EE_LD_mean_mat), fill=fillcolours, y.intersp=1)
dev.off()

## Total Daily EE

EE_Daily <- lmer(Avg_EE_Daily ~ Genotype*StartDate + BodyMass_g + (StartDate|Animal), data=data2, REML = TRUE)

Anova(EE_Daily)

EE_emmeans_Daily <- emmeans(EE_Daily, list(pairwise ~ Genotype), adjust = "tukey")

EE_lsmeans_Daily <- EE_emmeans_Daily$`emmeans of Genotype`

EE_lsmeans_Daily <- as.data.frame(EE_lsmeans_Daily)

EE_contrasts_Daily <- as.data.frame(EE_emmeans_Daily$`pairwise differences of Genotype`)

print(EE_lsmeans_Daily)

EE_lsmeans_Daily$emmean <- EE_lsmeans_Daily$emmean
EE_lsmeans_Daily$SE <- EE_lsmeans_Daily$SE
EE_lsmeans_Daily <- EE_lsmeans_Daily[with(EE_lsmeans_Daily, order(Genotype)), ]
write.table(EE_contrasts_Daily, file = "./output/202305_DBFX_Females_Sable_RC_Daily_EE_contrasts_Daily.csv")

pdf(file = "./output/202305_DBFX_Females_Sable_RC_EE_Amount.pdf", height = 4, width = 6)

EE_Daily_mean_mat <- t(matrix(c(subset(EE_lsmeans_Daily, Genotype == "Ctrl")$emmean, subset(EE_lsmeans_Daily, Genotype == "BKO")$emmean), 1, 2)) # create a 2-by-2 matrix, then transpose it
rownames(EE_Daily_mean_mat) <- c("Control","BKO")

EE_Daily_se_mat <- t(matrix(c(subset(EE_lsmeans_Daily, Genotype == "Ctrl")$SE, subset(EE_lsmeans_Daily, Genotype == "BKO")$SE), 1, 2)) # create a 2-by-2 matrix, then transpose it
rownames(EE_Daily_se_mat) <- c("Control","BKO")

fillcolours = c("dark violet","gray")
x.abscis <- barplot(
  EE_Daily_mean_mat, beside=TRUE,
  col=fillcolours,
  space=c(0.4,0.4), # spacing between bars in the same group, and then between groups
  width=0.2, # bar widths
  ylim=c(0,250),
  yaxp=c(0,250,10),
  ylab="IR Beam Breaks",
  xlim=c(0,1.5), # makes sense in the context of width and space parameters
  xlab="Treatment",
  axis.lty=1, # enable tick marks on the X axis
  main="Average Daily EE Counts",
  font.lab=2 # boDaily for axis labels
)
box(bty="L")
superpose.eb(x.abscis, EE_Daily_mean_mat, ebl=0, ebu=EE_Daily_se_mat) # +1 SEM, no descending error bar
legend(x=1, y=3, box.lty=0, legend=rownames(EE_Daily_mean_mat), fill=fillcolours, y.intersp=1)

dev.off() # close the PDF device