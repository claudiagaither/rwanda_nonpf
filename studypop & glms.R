#Rwanda 14-15 DHS data import. cleaning/new variables, and qPCR data merge for bivariate glms

#DHS data import and cleaning----
#load packages 
library(survey)
library(stats)
library(tidyverse)
library(readxl)
library(writexl)
library(haven)
library(srvyr)
library(broom)
library(purrr)
library(dplyr)
options(survey.lonely.psu="adjust")

#import DHS household recode
RW1415<-read_dta("C:/Users/cgait/OneDrive/Desktop/Rwanda nonpf/rw_nonpf data/data downloads/DHS data downloads/RW_2014-15_DHS_09052022_1445_180526/RWHR70DT/RWHR70FL.DTA")
#write_xlsx(RW1415, "C:/Users/cgait/OneDrive/Desktop/Rwanda nonpf/rw_nonpf data/DHS data/RW1415.xlsx")
DHS_cluster<-read_excel("C:/Users/cgait/OneDrive/Desktop/Rwanda nonpf/rw_nonpf data/summary datasets/DHS_cluster.xlsx")

#pull variables of interest from the full DHS dataset
rw15<-RW1415[,c("hv001","hv002","hv003","hv005","hv012","hv023","hv024","ha62_1","hb62_1","ha57_1","ha69_1","hb69_1","hv104_01","hv105_01","hv106_01","hml10_1",
"hml20_01","hv270","hv246","hv201","hml1","hv021","hv025","hv040","hv006","ha62_2","hb62_2","ha57_2","ha69_2","hb69_2","hv104_02","hv105_02","hv106_02","hml10_2",
"hml20_02","ha62_3","hb62_3","ha57_3","ha69_3","hb69_3","hv104_03","hv105_03","hv106_03","hml10_3","hml20_03","ha62_4","hb62_4","ha57_4","ha69_4","hb69_4",
"hv104_04","hv105_04","hv106_04","hml10_4","hml20_04","ha62_5","hb62_5","ha57_5","ha69_5","hb69_5","hv104_05","hv105_05","hv106_05","hml10_5","hml20_05",
"ha62_6","hb62_6","ha57_6","ha69_6","hb69_6","hv104_06","hv105_06","hv106_06","hml10_6","hml20_06","ha62_7","hb62_7","ha57_7","ha69_7","hb69_7","hv104_07",
"hv105_07","hv106_07","hml10_7","hml20_07")]

#replace blanks with NA 
write_xlsx(rw15, "C:/Users/cgait/OneDrive/Desktop/Rwanda nonpf/rw_nonpf data/DHS data/rw15")
rw15<-read_excel("C:/Users/cgait/OneDrive/Desktop/Rwanda nonpf/rw_nonpf data/DHS data/rw15")

#pulling barcodes & corresponding data out of all 14 columns they are given in
#starting with 1-7
rw1 <- rw15[,c("ha62_1","hb62_1","ha57_1","ha69_1","hb69_1","hv104_01","hv105_01","hv106_01","hml10_1","hml20_01","hv270","hv246","hv201","hml1","hv025","hv040","hv006","hv001","hv002","hv003","hv005","hv012","hv021","hv023","hv024")]
rw2 <- rw15[,c("ha62_2","hb62_2","ha57_2","ha69_2","hb69_2","hv104_02","hv105_02","hv106_02","hml10_2","hml20_02","hv270","hv246","hv201","hml1","hv025","hv040","hv006","hv001","hv002","hv003","hv005","hv012","hv021","hv023","hv024")]
rw3 <- rw15[,c("ha62_3","hb62_3","ha57_3","ha69_3","hb69_3","hv104_03","hv105_03","hv106_03","hml10_3","hml20_03","hv270","hv246","hv201","hml1","hv025","hv040","hv006","hv001","hv002","hv003","hv005","hv012","hv021","hv023","hv024")]
rw4 <- rw15[,c("ha62_4","hb62_4","ha57_4","ha69_4","hb69_4","hv104_04","hv105_04","hv106_04","hml10_4","hml20_04","hv270","hv246","hv201","hml1","hv025","hv040","hv006","hv001","hv002","hv003","hv005","hv012","hv021","hv023","hv024")]
rw5 <- rw15[,c("ha62_5","hb62_5","ha57_5","ha69_5","hb69_5","hv104_05","hv105_05","hv106_05","hml10_5","hml20_05","hv270","hv246","hv201","hml1","hv025","hv040","hv006","hv001","hv002","hv003","hv005","hv012","hv021","hv023","hv024")]
rw6 <- rw15[,c("ha62_6","hb62_6","ha57_6","ha69_6","hb69_6","hv104_06","hv105_06","hv106_06","hml10_6","hml20_06","hv270","hv246","hv201","hml1","hv025","hv040","hv006","hv001","hv002","hv003","hv005","hv012","hv021","hv023","hv024")]
rw7 <- rw15[,c("ha62_7","hb62_7","ha57_7","ha69_7","hb69_7","hv104_07","hv105_07","hv106_07","hml10_7","hml20_07","hv270","hv246","hv201","hml1","hv025","hv040","hv006","hv001","hv002","hv003","hv005","hv012","hv021","hv023","hv024")]

#renaming everything to get rid of trailing numbers
names1 = c("ha62_1","hb62_1","ha57_1","ha69_1","hb69_1","hv104_01","hv105_01","hv106_01","hml10_1","hml20_01")
names2 = c("ha62_2","hb62_2","ha57_2","ha69_2","hb69_2","hv104_02","hv105_02","hv106_02","hml10_2","hml20_02")
names3 = c("ha62_3","hb62_3","ha57_3","ha69_3","hb69_3","hv104_03","hv105_03","hv106_03","hml10_3","hml20_03")
names4 = c("ha62_4","hb62_4","ha57_4","ha69_4","hb69_4","hv104_04","hv105_04","hv106_04","hml10_4","hml20_04")
names5 = c("ha62_5","hb62_5","ha57_5","ha69_5","hb69_5","hv104_05","hv105_05","hv106_05","hml10_5","hml20_05")
names6 = c("ha62_6","hb62_6","ha57_6","ha69_6","hb69_6","hv104_06","hv105_06","hv106_06","hml10_6","hml20_06")
names7 = c("ha62_7","hb62_7","ha57_7","ha69_7","hb69_7","hv104_07","hv105_07","hv106_07","hml10_7","hml20_07")

names = c("ha62","hb62","ha57","ha69","hb69","hv104","hv105","hv106","hml10","hml20")
rw1<- rw1 %>% rename_at(all_of(names1),~names)
rw2<- rw2 %>% rename_at(all_of(names2),~names)
rw3<- rw3 %>% rename_at(all_of(names3),~names)
rw4<- rw4 %>% rename_at(all_of(names4),~names)
rw5<- rw5 %>% rename_at(all_of(names5),~names)
rw6<- rw6 %>% rename_at(all_of(names6),~names)
rw7<- rw7 %>% rename_at(all_of(names7),~names)

#drop empty barcode rows & separate by gender (a & b)
rw1a<-rw1 %>% drop_na(ha62)
rw1b<-rw1 %>% drop_na(hb62)
rw2a<-rw2 %>% drop_na(ha62)
rw2b<-rw2 %>% drop_na(hb62)
rw3a<-rw3 %>% drop_na(ha62)
rw3b<-rw3 %>% drop_na(hb62)
rw4a<-rw4 %>% drop_na(ha62)
rw4b<-rw4 %>% drop_na(hb62)
rw5a<-rw5 %>% drop_na(ha62)
rw5b<-rw5 %>% drop_na(hb62)
rw6a<-rw6 %>% drop_na(ha62)
rw6b<-rw6 %>% drop_na(hb62)
rw7a<-rw7 %>% drop_na(ha62)
rw7b<-rw7 %>% drop_na(hb62)

#bind all barcodes together; rw6 and 7 are both empty
rw15_bc<-rbind(rw1a,rw1b,rw2a,rw2b,rw3a,rw3b,rw4a,rw4b,rw5a,rw5b)

#import qPCR data with barcodes
qpcr_bc<- read_excel("C:/Users/cgait/OneDrive/Desktop/Rwanda nonpf/rw_nonpf data/summary datasets/qPCR data_working.xlsx")
qpcr_bc$h62<-qpcr_bc$sample
qpcr_bc<-qpcr_bc[,c("h62","var_CT","pm_CT","po_CT","pv_CT")]

#list of barcodes for which we have qPCR data
h62s<-qpcr_bc$h62

#creating single hiv weight & barcode based on hv104, ends up super wonky with lots of repeated barcodes and 9999 values
#rw_svya <- rw_svya %>% mutate(h62 = case_when(
#  hv104==1 ~ hb62, hv104==2 ~ ha62)) 
#rw_svya <- rw_svya %>% mutate(h69 = case_when(
#  hv104==1 ~ hb69, hv104==2 ~ ha69))
#rw_svyb <- rw_svyb %>% mutate(h62 = case_when(
#  hv104==1 ~ hb62, hv104==2 ~ ha62)) 
#rw_svyb <- rw_svyb %>% mutate(h69 = case_when(
#  hv104==1 ~ hb69, hv104==2 ~ ha69))

#values where hv104 != h62/h69
#rw_mismatch<-rw15_bc[is.na(rw15_bc$h62),]

#mismatch dataset compared to full rw15_bc
#sex (not comparable!)
#hist(rw15_bc$hv104)
#hist(rw_mismatch$hv104)
#age (not 100% comparable!)
#hist(rw15_bc$hv105)
#hist(rw_mismatch$hv105)
#region (basically identical)
#hist(rw15_bc$hv024)
#hist(rw_mismatch$hv024)
#education level (similar but not identical)
#hist(rw15_bc$hv106)
#hist(rw_mismatch$hv106)

#new variables for rw15_bc 
#household bed net y/n (hml1_cat)
rw15_bc <- rw15_bc %>% mutate(hml1_cat = case_when(
  hml1 == 0 ~ 0,
  hml1 > 0 ~ 1))

#elevation binary (elev1500, above or below)
rw15_bc <- rw15_bc %>% mutate(elev1500 = case_when(
  hv040>=1500 ~ ">= 1500",
  hv040<1500 ~ "<1500"))

#categorical variable for elevation (hv040_cat)
rw15_bc$hv040_cat <- cut(rw15_bc$hv040,
  breaks = c(0, 500, 1000, 1500, 2000, 2500, Inf),
  labels = c(1, 2, 3, 4, 5, 6), include.lowest = TRUE)

#1 bednet per 1.8 household members y/n (bednetper_cat)
rw15_bc$hh_1.8 <- (rw15_bc$hv012)/1.8
rw15_bc$net_1.8 <- (rw15_bc$hh_1.8)/(rw15_bc$hml1)
#fix values with 0 bed nets
rw15_bc$net_1.8[rw15_bc$net_1.8 == Inf]<-0
rw15_bc <- rw15_bc %>% mutate(bednetper_cat = case_when(
  net_1.8 > 1 ~ 1,
  net_1.8 < 1 ~ 0))

#binary variable for water source (hv201_cat, 1 = piped, 0 = unpiped)
rw15_bc$hv201_cat <- cut(rw15_bc$hv201, breaks=c(0, 12, Inf), labels=c(1,0), include.lowest = TRUE) 

#categorical variable for age (hv105_cat)
rw15_bc$hv105_cat <- cut(rw15_bc$hv105,
breaks = c(0, 15, 24, 35, 44, 55, Inf),
labels = c(6, 1, 2, 3, 4, 5), include.lowest = TRUE)

#hml10 missing values
summary(rw15_bc$hml10)
rw15_bc$hml10[is.na(rw15_bc$hml10)]<-12
summary(rw15_bc$hml10)

#data weights---- 
#inverse propensity of selection, HIV sampling, & transmission intensity weighting

#selection into study based on both original barcode columns
rw15_bc$selecta <- ifelse(rw15_bc$ha62 %in% h62s,1,0)
rw15_bc$selectb <- ifelse(rw15_bc$hb62 %in% h62s,1,0)
rw15_bc <- rw15_bc %>% mutate(select = case_when(selecta==1 ~ 1, selectb==1 ~ 1)) 

#replace NAs with 0s in select
rw15_bc$select[is.na(rw15_bc$select)]<-0

#propensity score with full barcodes dataset
#removed hml10 and hv201_cat from the glm because of missing data, does not change ps values a lot
ps_model <- glm(select ~ hv006+hv024+hv025+hv040+hv104+hv106+hv246+hml1+hml20+hv270+hv105_cat,family=binomial("logit"), data=rw15_bc)

#add ps to dataframe
rw15_bc$ps <- predict(ps_model, rw15_bc, type = "response")
#unstandardized
rw15_bc$ipwt_u <- ifelse(rw15_bc$select==1, 1/rw15_bc$ps, 1/(1-rw15_bc$ps))
#standardized
p_exposure <- sum(rw15_bc$select) / nrow(rw15_bc)
rw15_bc$ipwt <- ifelse(rw15_bc$select==1, p_exposure/rw15_bc$ps, (1-p_exposure)/(1-rw15_bc$ps))

#study data subset 
#merge survey subset with qPCR data
rw_svya<-rw15_bc %>% filter(selecta==1)
rw_svyb<-rw15_bc %>% filter(selectb==1)
rw_svya<-distinct(rw_svya)
rw_svyb<-distinct(rw_svyb)

rw_svya$h62<-rw_svya$ha62
rw_svya$h69<-rw_svya$ha69
rw_svyb$h62<-rw_svyb$hb62
rw_svyb$h69<-rw_svyb$hb69

rw_svy<-rbind(rw_svya, rw_svyb)
rw_svy<-distinct(rw_svy)

#transmission intensity weights
#import transmission intensity data by cluster
cluster_trans <- read_excel("C:/Users/cgait/OneDrive/Desktop/Rwanda nonpf/rw_nonpf data/summary datasets/DHS_cluster.xlsx")
#add region names to rw_svy
rw_svy <- left_join(rw_svy, cluster_trans[,c("hv001","region")],by="hv001")

#transmission intensity matching 
rw15_bc <- left_join(rw15_bc, cluster_trans[,c("hv001","trans_intens")],by="hv001")
rw_svy <- left_join(rw_svy, cluster_trans[,c("hv001","trans_intens")],by="hv001")
#check counts
addmargins(table(rw15_bc$trans_intens, useNA = "always"))

#summarize number of individuals in high/low clusters in all barcodes data set
overall <- rw15_bc %>% group_by(trans_intens) %>% count()

#summarize number of individuals in high/low clusters in sampled data set
sampled <- rw_svy %>% group_by(trans_intens) %>% count()

#forming the numerator of the transmission intensity weight
high_ov <- overall[overall$trans_intens=="high",]$n / sum(overall$n)
low_ov <- overall[overall$trans_intens=="low",]$n / sum(overall$n)

#forming the denominator of the transmission intensity weight
high_samp <- sampled[sampled$trans_intens=="high",]$n / sum(sampled$n)
low_samp <- sampled[sampled$trans_intens=="low",]$n / sum(sampled$n)

#final transmission intensity weight
high_wt <- high_ov/high_samp
low_wt <- low_ov/low_samp

#replace missing HIV weight values with 1
rw_svy$h69[rw_svy$h69 == 0]<-1000000

#HIV and ip weights for rw_svy
rw_svy$hiv_wt <- (rw_svy$h69)/1000000
rw_svy$hiv_ipw <- rw_svy$hiv_wt*rw_svy$ipwt


#add qPCR data to rw_svy-----
rw_svy<-merge(qpcr_bc,rw_svy, by="h62")

#create malaria species binary variables
rw_svy <- rw_svy %>% mutate(pf = ifelse(is.na(var_CT), 0, 1))
rw_svy <- rw_svy %>% mutate(pm = ifelse(is.na(pm_CT), 0, 1))
rw_svy <- rw_svy %>% mutate(po = ifelse(is.na(po_CT), 0, 1))
rw_svy <- rw_svy %>% mutate(pv = ifelse(is.na(pv_CT), 0, 1))

#non-pf binary variable
rw_svy$nonpf <- ifelse(rw_svy$pm == 1 | rw_svy$po == 1 | rw_svy$pv == 1, 1, 0)

#mixed infection variable (species) 
rw_svy$species <- ifelse(rw_svy$pf == 1 & rw_svy$po == 0 & rw_svy$pm == 0 & rw_svy$pv == 0, "pf",
    ifelse(rw_svy$pf == 0 & rw_svy$po == 0 & rw_svy$pm == 1 & rw_svy$pv == 0, "pm",
    ifelse(rw_svy$pf == 0 & rw_svy$po == 1 & rw_svy$pm == 0 & rw_svy$pv == 0, "po",
    ifelse(rw_svy$pf == 0 & rw_svy$po == 0 & rw_svy$pm == 0 & rw_svy$pv == 1, "pv",
    ifelse(rw_svy$pf == 1 & rw_svy$po == 1 & rw_svy$pm == 0 & rw_svy$pv == 0, "pf_po",
    ifelse(rw_svy$pf == 1 & rw_svy$po == 0 & rw_svy$pm == 1 & rw_svy$pv == 0, "pf_pm",
    ifelse(rw_svy$pf == 1 & rw_svy$po == 0 & rw_svy$pm == 0 & rw_svy$pv == 1, "pf_pv",
    ifelse(rw_svy$pf == 1 & rw_svy$po == 0 & rw_svy$pm == 1 & rw_svy$pv == 1, "pf_pm_pv",
    ifelse(rw_svy$pf == 1 & rw_svy$po == 1 & rw_svy$pm == 1 & rw_svy$pv == 0, "pf_pm_po",
    ifelse(rw_svy$pf == 0 & rw_svy$po == 1 & rw_svy$pm == 1 & rw_svy$pv == 0, "pm_po",
    ifelse(rw_svy$pf == 0 & rw_svy$po == 1 & rw_svy$pm == 1 & rw_svy$pv == 1, "pm_po_pv",
    ifelse(rw_svy$pf == 0 & rw_svy$po == 0 & rw_svy$pm == 1 & rw_svy$pv == 1, "pm_pv",
    ifelse(rw_svy$pf == 0 & rw_svy$po == 0 & rw_svy$pm == 0 & rw_svy$pv == 0, "none", "none")))))))))))))

#under 40 CT values data set
rw_svy40<-rw_svy
CT_cutoff<-40
rw_svy40$var_CT[rw_svy40$var_CT > CT_cutoff]<-NA
rw_svy40$pm_CT[rw_svy40$pm_CT > CT_cutoff]<-NA
rw_svy40$po_CT[rw_svy40$po_CT > CT_cutoff]<-NA
rw_svy40$pv_CT[rw_svy40$pv_CT > CT_cutoff]<-NA

#CT count variable (CT_count)
rw_svy$CT_count <- rowSums(rw_svy[, c("pf", "pm", "po", "pv")], na.rm = TRUE)

#infection complexity variable (infection)
rw_svy <- rw_svy %>% mutate(infection = case_when(CT_count>1 ~ "co",CT_count==1 ~ "mono",CT_count==0 ~ "none"))

#malaria binary variable
rw_svy <- rw_svy %>% mutate(malaria = case_when(CT_count>0 ~ 1, CT_count==0 ~ 0))

#create malaria infection variables for rw_svy40
rw_svy40 <- rw_svy40 %>% mutate(pf = ifelse(is.na(var_CT), 0, 1))
rw_svy40 <- rw_svy40 %>% mutate(pm = ifelse(is.na(pm_CT), 0, 1))
rw_svy40 <- rw_svy40 %>% mutate(po = ifelse(is.na(po_CT), 0, 1))
rw_svy40 <- rw_svy40 %>% mutate(pv = ifelse(is.na(pv_CT), 0, 1))

#mixed infection for rw_svy40
rw_svy40$species <- ifelse(rw_svy40$pf == 1 & rw_svy40$po == 0 & rw_svy40$pm == 0 & rw_svy40$pv == 0, "pf",
  ifelse(rw_svy40$pf == 0 & rw_svy40$po == 0 & rw_svy40$pm == 1 & rw_svy40$pv == 0, "pm",
  ifelse(rw_svy40$pf == 0 & rw_svy40$po == 1 & rw_svy40$pm == 0 & rw_svy40$pv == 0, "po",
  ifelse(rw_svy40$pf == 0 & rw_svy40$po == 0 & rw_svy40$pm == 0 & rw_svy40$pv == 1, "pv",
  ifelse(rw_svy40$pf == 1 & rw_svy40$po == 1 & rw_svy40$pm == 0 & rw_svy40$pv == 0, "pf_po",
  ifelse(rw_svy40$pf == 1 & rw_svy40$po == 0 & rw_svy40$pm == 1 & rw_svy40$pv == 0, "pf_pm",
  ifelse(rw_svy40$pf == 1 & rw_svy40$po == 0 & rw_svy40$pm == 0 & rw_svy40$pv == 1, "pf_pv",
  ifelse(rw_svy40$pf == 1 & rw_svy40$po == 0 & rw_svy40$pm == 1 & rw_svy40$pv == 1, "pf_pm_pv",
  ifelse(rw_svy40$pf == 1 & rw_svy40$po == 1 & rw_svy40$pm == 1 & rw_svy40$pv == 0, "pf_pm_po",
  ifelse(rw_svy40$pf == 0 & rw_svy40$po == 1 & rw_svy40$pm == 1 & rw_svy40$pv == 0, "pm_po",
  ifelse(rw_svy40$pf == 0 & rw_svy40$po == 1 & rw_svy40$pm == 1 & rw_svy40$pv == 1, "pm_po_pv",
  ifelse(rw_svy40$pf == 0 & rw_svy40$po == 0 & rw_svy40$pm == 1 & rw_svy40$pv == 1, "pm_pv",
  ifelse(rw_svy40$pf == 0 & rw_svy40$po == 0 & rw_svy40$pm == 0 & rw_svy40$pv == 0, "none", "none")))))))))))))

#CT_count for rw_svy40
rw_svy40$CT_count <- rowSums(rw_svy40[, c("pf", "pm", "po", "pv")], na.rm = TRUE)

#malaria binary for rw_svy40
rw_svy40 <- rw_svy40 %>% mutate(malaria = case_when(CT_count>0 ~ 1, CT_count==0 ~ 0))

#non-pf binary variable for rw_svy40
rw_svy40$nonpf <- ifelse(rw_svy40$pm == 1 | rw_svy40$po == 1 | rw_svy40$pv == 1, 1, 0)


#study dataset barcodes (for matching eco data)
svy_bc<-rw_svy$h62

#ecology data by cluster import
eco_data<-read_excel("C:/Users/cgait/OneDrive/Desktop/Rwanda nonpf/rw_nonpf data/summary datasets/eco_data.xlsx")
eco_data$bc<- ifelse(eco_data$h62 %in% svy_bc,1,0)
eco_data<-eco_data %>% filter(bc==1)

#add ecological data to rw_svy
rw_svy<-left_join(rw_svy, eco_data[,c("h62","temp")],by="h62")
rw_svy<-left_join(rw_svy, eco_data[,c("h62","rain")],by="h62")
rw_svy<-left_join(rw_svy, eco_data[,c("h62","landcover")],by="h62")

#add transmission intensity to svy and svy40 
rw_svy<-left_join(rw_svy, cluster_trans[,c("hv001","trans_intens")],by="hv001")
rw_svy40<-left_join(rw_svy40, cluster_trans[,c("hv001","trans_intens")],by="hv001")
#table(rw_svy$trans_intens, useNA = "always")
#table(rw_svy40$trans_intens, useNA = "always")

#are x and y different at all or is this just how it does.. ?
rw_svy$trans_intens<-rw_svy$trans_intens.x
rw_svy40$trans_intens<-rw_svy40$trans_intens.x

#add transmission intensity weights onto study datasets (rw_svy & rw_svy40)
rw_svy<-rw_svy %>% mutate(trans_wt=case_when(
  trans_intens=="high"~high_wt,
  trans_intens=="low"~low_wt))

rw_svy40<-rw_svy40 %>% mutate(trans_wt=case_when(
  trans_intens=="high"~high_wt,
  trans_intens=="low"~low_wt))

#calculate the final weights
#wt: HIV * IPSW * trans_wt
rw_svy$wt<-(rw_svy$hiv_ipw)*(rw_svy$trans_wt) 
rw_svy40$wt<-(rw_svy40$hiv_ipw)*(rw_svy40$trans_wt)

#missing value (in hv012), make weight=1
rw_svy$wt[is.na(rw_svy$wt)]<-1
rw_svy40$wt[is.na(rw_svy40$wt)]<-1

summary(rw_svy$wt)
summary(rw_svy40$wt)

#cleaning/adding variables for glms
#factor variables
rw_svy$hv105_cat <- as.factor(rw_svy$hv105_cat)
rw_svy$hv270 <- as.factor(rw_svy$hv270)
rw_svy$hv106 <- as.factor(rw_svy$hv106)
rw_svy$hv024 <- as.factor(rw_svy$hv024)
rw_svy$hv006 <- as.factor(rw_svy$hv006)
rw_svy$landcover <- as.factor(rw_svy$landcover)

#average temp & rain among study data set 
DHS<-svydesign(id=rw_svy$hv021, strata=rw_svy$hv023, weights=rw_svy$wt, data=rw_svy)
DHS<-as_survey_design(DHS)

temp<-svyby(~temp, ~select, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
rain<-svyby(~rain, ~select, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
avg_temp<-temp$temp
avg_rain<-rain$rain

#temp binary cats
rw_svy <- rw_svy %>% mutate(tempb = case_when(
  temp>=avg_temp ~ "at or above average",
  temp<avg_temp ~ "below avg. temp"))
#rainfall binary cats
rw_svy <- rw_svy %>% mutate(rainb = case_when(
  rain>=avg_rain ~ "at or above average",
  rain<avg_rain ~ "below avg. rainfall"))
#education binary
rw_svy <- rw_svy %>% mutate(educat = case_when(
  hv106 == "0" ~ "primary school or below",
  hv106 == "1" ~ "primary school or below",
  hv106 == "2" ~ "secondary or higher",
  hv106 == "3" ~ "secondary or higher"))
#wealth binary for glms
rw_svy <- rw_svy %>% mutate(wealth = case_when(
  hv270 == "1" ~ "wealth quintiles 1 & 2",
  hv270 == "2" ~ "wealth quintiles 1 & 2",
  hv270 == "3" ~ "quintiles 3+",
  hv270 == "4" ~ "quintiles 3+",
  hv270 == "5" ~ "quintiles 3+"))
#regroup age categories
rw_svy <- rw_svy %>% mutate(age = case_when(
  hv105_cat == "1" ~ "0-24 years",
  hv105_cat == "2" ~ "24 years or older",
  hv105_cat == "3" ~ "24 years or older",
  hv105_cat == "4" ~ "24 years or older",
  hv105_cat == "5" ~ "24 years or older",
  hv105_cat == "6" ~ "0-24 years"))
#landcover re-classification
rw_svy <- rw_svy %>% mutate(land = case_when(
  landcover == "1" ~ "crop/grassland",
  landcover == "2" ~ "forest/woodland",
  landcover == "3" ~ "forest/woodland",
  landcover == "4" ~ "forest/woodland",
  landcover == "0" ~ "crop/grassland",))

#proportion of each cluster with household bednets (prop_bednet)
p_bednet<-as.data.frame(svyby(~hml1_cat, ~hv001, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust"))
p_bednet$prop_bednet<-p_bednet$hml1_cat
rw_svy<-left_join(rw_svy, p_bednet[,c("hv001","prop_bednet")],by="hv001")

#proportion of each cluster that slept under a net last night (prop_slept)
p_slept<-as.data.frame(svyby(~hml20, ~hv001, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust"))
p_slept$prop_slept<-p_slept$hml20
rw_svy<-left_join(rw_svy, p_slept[,c("hv001","prop_slept")],by="hv001")


#DHS designs----
#DHS: wt = HIV*ipwt*trans_wt
DHS<-svydesign(id=rw_svy$hv021, strata=rw_svy$hv023, weights=rw_svy$wt, data=rw_svy)
DHS<-as_survey_design(DHS)

#DHS40: wt (but 40 cycles CT cutoff dataset)
DHS40<-svydesign(id=rw_svy40$hv021, strata=rw_svy40$hv023, weights=rw_svy40$wt, data=rw_svy40)
DHS40_design<-as_survey_design(DHS40)

#svydesign for high & low transmission cluster strata
#highpf_svy<-rw_svy %>% filter(trans_intens=="high")
#lowpf_svy<-rw_svy %>% filter(trans_intens=="low")
#hpf_dhs<-svydesign(id=highpf_svy$hv021, strata=highpf_svy$hv023, weights=highpf_svy$fin_wt, data=highpf_svy)
#hpf_dhs<-as_survey_design(hpf_dhs)
#lpf_dhs<-svydesign(id=lowpf_svy$hv021, strata=lowpf_svy$hv023, weights=lowpf_svy$fin_wt, data=lowpf_svy)
#lpf_dhs<-as_survey_design(lpf_dhs)


#weights for rw15_bc: HIV weight only and based on hv104
rw15_bc <- rw15_bc %>% mutate(count = 1)
rw15_bc <- rw15_bc %>% mutate(h62 = case_when(
  hv104==1 ~ hb62, hv104==2 ~ ha62)) 
rw15_bc <- rw15_bc %>% mutate(h69 = case_when(
  hv104==1 ~ hb69, hv104==2 ~ ha69))

#values where hv104 != h62/h69
rw_mismatch<-rw15_bc[is.na(rw15_bc$h62),]

#fill in mis-matched weights with 1s for now
rw15_bc$h69[rw15_bc$h69==0]<-1000000
rw15_bc$h69[is.na(rw15_bc$h69)]<-1000000
rw15_bc$wt<-(rw15_bc$h69)/1000000

DHS15<-svydesign(id=rw15_bc$hv021, strata=rw15_bc$hv023, weights=rw15_bc$wt, data=rw15_bc, nest=TRUE)
DHS15<-as_survey_design(DHS15)


#prevalences for maps/tables and written results (HIV survey weight, inverse prevalence and transmission intensity weights)
#prop.table(svytable(~pf, DHS_design))
svyciprop(~malaria, DHS, method="lo")
svyciprop(~pf, DHS, method="lo")
svyciprop(~pm, DHS, method="lo")
svyciprop(~po, DHS, method="lo")
svyciprop(~pv, DHS, method="lo")
svyciprop(~nonpf, DHS, method="lo")

#40-cycle CT cutoff data & prevalence
svyciprop(~malaria, DHS40, method="lo")
svyciprop(~pf, DHS40, method="lo")
svyciprop(~pm, DHS40, method="lo")
svyciprop(~po, DHS40, method="lo")
svyciprop(~pv, DHS40, method="lo")
svyciprop(~nonpf, DHS40, method="lo")

#prevalence counts (45 cycles)
as.data.frame(svyby(~select, ~region, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~malaria, ~region, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~pf, ~region, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~pm, ~region, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~po, ~region, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~pv, ~region, DHS, svytotal, survey.lonely.psu="adjust"))
#prevalence counts (40 cycles)
as.data.frame(svyby(~malaria, ~region, DHS40, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~pf, ~region, DHS40, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~pm, ~region, DHS40, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~po, ~region, DHS40, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~pv, ~region, DHS40, svytotal, survey.lonely.psu="adjust"))


#methods numbers counts
total_svy<-length(rw_svy$h62)
total_cluster<-length(DHS_cluster$hv001)

trans_int<-as.data.frame(rw_svy %>% group_by(trans_intens) %>% summarise(Total=n()/nrow(.)))
trans_int$count<-trans_int$Total*total_svy
print(trans_int)

cluster_int<-as.data.frame(DHS_cluster %>% group_by(svy_class) %>% summarise(Total=n()/nrow(.)))
cluster_int$count<-cluster_int$Total*total_cluster
print(cluster_int)


#mixed infection unweighted counts (table s4)
mixed_count<-as.data.frame(rw_svy %>% group_by(species) %>% summarise(Total=n()/nrow(.)))
mixed_count$count<-(mixed_count$Total)*4595
print(mixed_count)
#infection<-as.data.frame(rw_svy %>% group_by(infection) %>% summarise(Percentage=n()/nrow(.)))

#mixed infection unweighted counts (40 cycles cutoff)
mixed_count40<-as.data.frame(rw_svy40 %>% group_by(species) %>% summarise(Total=n()/nrow(.)))
mixed_count40$count<-(mixed_count40$Total)*4595
print(mixed_count40)
#infection40<-as.data.frame(rw_svy40 %>% group_by(infection) %>% summarise(Percentage=n()/nrow(.)))

#mixed infection weighted counts (45 cycles)
w_mixed_count<-as.data.frame(svyby(~select, ~species, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~infection, DHS, svytotal, survey.lonely.psu="adjust"))
print(w_mixed_count)

#mixed infection weighted counts (40 cycles cutoff)
w_mixed_count40<-as.data.frame(svyby(~select, ~species, DHS40, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~infection, DHS40, svytotal, survey.lonely.psu="adjust"))
print(w_mixed_count40)

#pull malaria positive locations for cluster maps (figure s1)
malaria_pos<-rw_svy[,c("species","hv001","pf","pm","po","pv")]
malaria_pos<-merge(DHS_cluster,malaria_pos, by="hv001")

pf_pos<-malaria_pos %>% filter(pf==1)
pm_pos<-malaria_pos %>% filter(pm==1)
po_pos<-malaria_pos %>% filter(po==1)
pv_pos<-malaria_pos %>% filter(pv==1)

#study data compared to overall DHS data set (table s2)
rw15vars<-c("hv104","hv105_cat","hv270","hv106","hv246","hv201_cat","hml1_cat","hml20","hml10",
            "bednetper_cat","hv024","hv025","elev1500","hv006")
svyvars<-c("hv104","hv105_cat","hv270","hv106","hv246","hv201_cat","hml1_cat","hml20","hml10",
           "bednetper_cat","hv024","hv025","elev1500","hv006","landcover")

DHS_count<-function(var) {m <- svyby(as.formula(paste0('count ~', var, DHS15, svytotal, survey.lonely.psu="adjust", na.rm=T)))
cbind(tidy(m))}

#total weighted counts for study population covariates
#DHS_counts<-map_dfr(rw15vars, DHS_count)
#colnames(DHS_totals) <- c('var','total_count','SE')
#DHS_totals%>% print(noSpaces=T) 

svyby(~count, ~hv104, DHS15, svytotal, survey.lonely.psu="adjust")
svyby(~count, ~hv105_cat, DHS15, svytotal, survey.lonely.psu="adjust")
svyby(~count, ~hv270, DHS15, svytotal, survey.lonely.psu="adjust")
svyby(~count, ~hv106, DHS15, svytotal, survey.lonely.psu="adjust")
svyby(~count, ~hv246, DHS15, svytotal, survey.lonely.psu="adjust")
svyby(~count, ~hv201_cat, DHS15, svytotal, survey.lonely.psu="adjust")
svyby(~count, ~hml1_cat, DHS15, svytotal, survey.lonely.psu="adjust")
svyby(~count, ~hml20, DHS15, svytotal, survey.lonely.psu="adjust")
svyby(~count, ~hml10, DHS15, svytotal, survey.lonely.psu="adjust")
svyby(~count, ~bednetper_cat, DHS15, svytotal, survey.lonely.psu="adjust")
#cluster level covariates
svyby(~count, ~hv024, DHS15, svytotal, survey.lonely.psu="adjust")
svyby(~count, ~hv025, DHS15, svytotal, survey.lonely.psu="adjust")
svyby(~count, ~elev1500, DHS15, svytotal, survey.lonely.psu="adjust")
svyby(~count, ~hv040_cat, DHS15, svytotal, survey.lonely.psu="adjust")
svyby(~count, ~hv006, DHS15, svytotal, survey.lonely.psu="adjust")
#svyby(~count, ~landcover, DHS15, svytotal, survey.lonely.psu="adjust")

#samples included in analysis
as.data.frame(svyby(~select, ~hv104, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~hv105_cat, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~hv270, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~hv106, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~hv246, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~hv201_cat, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~hml1_cat, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~hml20, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~hml10, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~bednetper_cat, DHS, svytotal, survey.lonely.psu="adjust"))
#cluster level covariates
as.data.frame(svyby(~select, ~hv024, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~hv025, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~elev1500, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~hv040_cat, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~hv006, DHS, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~select, ~landcover, DHS, svytotal, survey.lonely.psu="adjust"))


#table 1 numbers (study population by malaria species)-----
#P. falciparum positive counts using wt 
svyby(~pf, ~hv104, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pf, ~hv105_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pf, ~hv270, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pf, ~hv106, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pf, ~hv246, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pf, ~hv201_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pf, ~hml1_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pf, ~hml20, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pf, ~hml10, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pf, ~bednetper_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pf, ~hv024, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pf, ~hv025, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pf, ~hv040_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pf, ~hv006, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pf, ~landcover, DHS, svytotal, survey.lonely.psu="adjust")

#P. malariae positive counts using wt
svyby(~pm, ~hv104, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pm, ~hv105_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pm, ~hv270, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pm, ~hv106, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pm, ~hv246, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pm, ~hv201_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pm, ~hml1_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pm, ~hml20, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pm, ~hml10, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pm, ~bednetper_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pm, ~hv024, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pm, ~hv025, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pm, ~hv040_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pm, ~hv006, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pm, ~landcover, DHS, svytotal, survey.lonely.psu="adjust")

#P. ovale positive counts using wt
svyby(~po, ~hv104, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~po, ~hv105_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~po, ~hv270, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~po, ~hv106, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~po, ~hv246, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~po, ~hv201_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~po, ~hml1_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~po, ~hml20, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~po, ~hml10, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~po, ~bednetper_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~po, ~hv024, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~po, ~hv025, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~po, ~hv040_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~po, ~hv006, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~po, ~landcover, DHS, svytotal, survey.lonely.psu="adjust")

#P. vivax positive counts using wt
#pv_studyvars<- map_dfr(studyvars, pv_count)
#colnames(pv_studyvars) <- c('term','estimate','std.error','statistic','p.value','CIL_95','CIU_95')
#pv_studyvars %>% print(noSpaces=T) 

svyby(~pv, ~hv104, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pv, ~hv105_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pv, ~hv270, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pv, ~hv106, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pv, ~hv246, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pv, ~hv201_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pv, ~hml1_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pv, ~hml20, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pv, ~hml10, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pv, ~bednetper_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pv, ~hv024, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pv, ~hv025, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pv, ~hv040_cat, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pv, ~hv006, DHS, svytotal, survey.lonely.psu="adjust")
svyby(~pv, ~landcover, DHS, svytotal, survey.lonely.psu="adjust")


#averages by cluster
#averages for pf prevalence
svyby(~prop_bednet, ~pf, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
svyby(~prop_slept, ~pf, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
svyby(~temp, ~pf, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
svyby(~rain, ~pf, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
#averages for pm prevalence
svyby(~prop_bednet, ~pm, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
svyby(~prop_slept, ~pm, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
svyby(~temp, ~pm, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
svyby(~rain, ~pm, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
#averages for po prevalence
svyby(~prop_bednet, ~po, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
svyby(~prop_slept, ~po, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
svyby(~temp, ~po, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
svyby(~rain, ~po, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
#averages for pv prevalence
svyby(~prop_bednet, ~pv, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
svyby(~prop_slept, ~pv, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
svyby(~temp, ~pv, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
svyby(~rain, ~pv, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
#average for the whole study population
svyby(~prop_bednet, ~select, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
svyby(~prop_slept, ~select, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
svyby(~temp, ~select, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")
svyby(~rain, ~select, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust")



#bivariate glms----
#http://asdfree.com/demographic-and-heal-surveys-dhs.html
#pf_models
pf_svy<- function(var) {m <- svyglm(as.formula(paste0('pf ~', var)), DHS, family=quasibinomial("identity"))
cbind(tidy(m), confint(m))}
pm_svy<- function(var) {m <- svyglm(as.formula(paste0('pm ~', var)), DHS, family=quasibinomial("identity"))
cbind(tidy(m), confint(m))}
po_svy<- function(var) {m <- svyglm(as.formula(paste0('po ~', var)), DHS, family=quasibinomial("identity"))
cbind(tidy(m), confint(m))}

#stratified models
#pf_high <- function(var) {m <- svyglm(as.formula(paste0('pf ~', var)), hpf_dhs, family=quasibinomial("identity"))
#cbind(tidy(m), confint(m))}
#pf_low<-function(var) {m <- svyglm(as.formula(paste0('pf ~', var)), lpf_dhs, family=quasibinomial("identity"))
#cbind(tidy(m), confint(m))}

#po_high <- function(var) {m <- svyglm(as.formula(paste0('po ~', var)), hpf_dhs, family=quasibinomial("identity"))
#cbind(tidy(m), confint(m))}
#po_low<-function(var) {m <- svyglm(as.formula(paste0('po ~', var)), lpf_dhs, family=quasibinomial("identity"))
#cbind(tidy(m), confint(m))}

#pm_high <- function(var) {m <- svyglm(as.formula(paste0('pm ~', var)), hpf_dhs, family=quasibinomial("identity"))
#cbind(tidy(m), confint(m))}
#pm_low<-function(var) {m <- svyglm(as.formula(paste0('pm ~', var)), lpf_dhs, family=quasibinomial("identity"))
#cbind(tidy(m), confint(m))}

studyvars <-c("hv104","age","hv105","wealth","educat","hv246","hv201_cat",
"hml1_cat","hml20","hml10","bednetper_cat","hv025","elev1500","tempb",
"rain","rainb","land")

#collapsed categories glms for 3 species
pfsvy_glm<- map_dfr(studyvars, pf_svy)
colnames(pfsvy_glm) <- c('term','estimate','std.error','statistic','p.value','CIL_95','CIU_95')
pfsvy_glm %>% print(noSpaces=T) 

pmsvy_glm<- map_dfr(studyvars, pm_svy) 
colnames(pmsvy_glm) <- c('term','estimate','std.error','statistic','p.value','CIL_95','CIU_95')
pmsvy_glm %>% print(noSpaces=T) 
pm_svy(temp)

posvy_glm<- map_dfr(studyvars, po_svy) 
colnames(posvy_glm) <- c('term','estimate','std.error','statistic','p.value','CIL_95','CIU_95')
posvy_glm %>% print(noSpaces=T) 
po_svy(temp)

#stratified models using collapsed categories
#high transmission models/the only working instance of map_dfr :(
pfhigh_glm <- map_dfr(studyvars, pf_high)
colnames(pfhigh_glm) <- c('term','estimate','std.error','statistic','p.value','CIL_95','CIU_95')
pfhigh_glm %>% print(noSpaces=T) 

pflow_glm <- map_dfr(studyvars, pflow_high)
colnames(pflow_glm) <- c('term','estimate','std.error','statistic','p.value','CIL_95','CIU_95')
pflow_glm %>% print(noSpaces=T) 


