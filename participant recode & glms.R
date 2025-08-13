#Rwanda 14-15 DHS data import. cleaning/new variables, and qPCR data merge for bivariate glms

#DHS data import and cleaning----
#load packages & lonely psu 
library(broom)
library(chirps)
library(dplyr)
library(haven)
library(purrr)
library(readxl)
#library(rdhs) 
library(sf)
library(stats)
library(srvyr)
library(survey)
library(terra)
library(tidyverse)
library(writexl)


#clusters with only one participant (lonely psu)
options(survey.lonely.psu="adjust")

#import DHS individual recode (n=54,905)
RW1415PR<-readRDS("C:/Users/cgait/OneDrive/Desktop/IDEEL/rw_nonpf_data/DHS_recodes/RWPR70FL.rds")
#household recode (n=12,699)
#RW1415HR<-read_dta("C:/Users/cgait/OneDrive/Desktop/IDEEL/rw_nonpf_data/DHS_recodes/RWHR70FL.DTA")
#individual, HIV and GPS recodes
RW1415HIV <- readRDS("C:/Users/cgait/OneDrive/Desktop/IDEEL/rw_nonpf_data/DHS_recodes/RWAR71FL.rds")  #HIV recode (n=16,930)
RW1415IR <- read_sas("C:/Users/cgait/OneDrive/Desktop/IDEEL/rw_nonpf_data/DHS_recodes/RWIR70SD/RWIR70FL.SAS7BDAT") #Women's recode (n=13,497)
RW1415MR <- read_sas("C:/Users/cgait/OneDrive/Desktop/IDEEL/rw_nonpf_data/DHS_recodes/RWMR70SD/RWMR70FL.SAS7BDAT") #Men's recode (n=6,217)
RW1415GE <- read_sf("C:/Users/cgait/OneDrive/Desktop/IDEEL/rw_nonpf_data/DHS_recodes/RWGE72FL/RWGE72FL.shp") #GPS recode (n=492)

#DHS geocovariates
RW1415GC <- read.csv("C:/Users/cgait/OneDrive/Desktop/IDEEL/rw_nonpf_data/DHS_recodes/RWGC72FL/RWGC72FL.csv")

#import additional data
#DHS clusters transmission intensity
cluster_trans <- read.csv("C:/Users/cgait/OneDrive/Desktop/IDEEL/rw_nonpf_data/DHS_cluster.csv")
DHS_cluster <- read_excel("C:/Users/cgait/OneDrive/Desktop/IDEEL/Rwanda nonpf/data/summary datasets/DHS_cluster.xlsx")
#land cover data
rw_land<-read.csv("C:/Users/cgait/OneDrive/Desktop/IDEEL/rw_nonpf_data/sentinel_landcover.csv")
#qPCR data
qpcr_bc<-read_excel("C:/Users/cgait/OneDrive/Desktop/IDEEL/rw_nonpf_data/qpcr_data.xlsx")



# Regular expression for a 5-character string containing both letters and numbers
pattern <- "^(?=.*[A-Za-z])(?=.*[0-9])[A-Za-z0-9]{5}$"

# Ensure the column is of character type (if not already)
rw15_pr <- RW1415PR
rw15_pr$ha62 <- as.character(rw15_pr$ha62)
rw15_pr$hb62 <- as.character(rw15_pr$hb62)

# Create new column "barcodes" to pull corresponding barcodes for participants from ha and hb
rw15_pr <- rw15_pr %>%
 mutate(barcode = case_when(
      !is.na(ha62) & grepl(pattern, ha62, perl = TRUE) ~ ha62,
      !is.na(hb62) & grepl(pattern, hb62, perl = TRUE) ~ hb62,
      TRUE ~ NA_character_ ))            

#replace blank cells with NA and trim white space
rw15_pr <- rw15_pr %>%
  mutate(across(where(is.character), ~na_if(.x, "")))

rw15_pr <- rw15_pr %>%
  mutate(across(everything(), ~ if (is.character(.x)) {
    na_if(trimws(.x), "")
  } else {
    .x
  }))

#remove all NA barcodes 
rw15_bc <- rw15_pr %>% filter(!is.na(barcode)) # total of 13,226 individuals, using participant recode

## attach missing barcodes in participant recode from HIV recode
qpcr_bc$barcode <- as.character(qpcr_bc$sample)
rw15_bc$barcode <- as.character(rw15_bc$barcode)
RW1415PR$ha62 <- as.character(RW1415PR$ha62)
RW1415PR$hb62 <- as.character(RW1415PR$hb62)

# Trim any white space from barcodes
qpcr_bc$barcode <- trimws(qpcr_bc$barcode)
rw15_bc$barcode <- trimws(rw15_bc$barcode)
RW1415PR$ha62 <- trimws(RW1415PR$ha62)
RW1415PR$hb62 <- trimws(RW1415PR$hb62)

# pull barcodes missing from the participant recode
missing_barcodes <- qpcr_bc$barcode[!qpcr_bc$barcode %in% rw15_bc$barcode] #(n=454 missing (5,051-4,597))

# clean HIV recode 
RW1415HIV$barcode <- RW1415HIV$hiv01
RW1415HIV$barcode <- as.character(trimws(RW1415HIV$barcode))
hiv_missing_barcodes <- qpcr_bc$barcode[!qpcr_bc$barcode %in% RW1415HIV$barcode]

# FOUND 453 missing barcodes in HIV recode (total n = 5,050)
RW1415HIV_bc <- qpcr_bc$barcode[qpcr_bc$barcode %in% RW1415HIV$barcode]

# Make data frame including barcodes found in HIV recode (n=5050)
RW15_bc <- RW1415HIV[!is.na(RW1415HIV$barcode) & RW1415HIV$barcode %in% RW1415HIV_bc, ] #5050 samples

## combine all DBS samples (n= 16930) by aligning HIV samples with participant & individual gender recodes
# Merge data sets (See "Matching and merging data sets" https://dhsprogram.com/data/Guide-to-DHS-Statistics/Analyzing_DHS_Data.htm)

# Merge PR (household members) with HIV by cluster, household number, and household line number #362 variables
dbs_master15 <- RW15_bc
dbs_master15 <- RW1415PR %>%
  left_join(RW1415HIV, by = c("hv001" = "hivclust", "hv002" = "hivnumb", "hvidx" = "hivline"))

#Merge dbs_master (PR + HIV) with men's individual recode (MR)
dbs_master15_m <- dbs_master15 %>% filter(hv104 == 1)
dbs_master15_m <- dbs_master15_m %>%
  left_join(RW1415MR, by = c("hv001" = "MV001", "hv002" = "MV002", "hvidx" = "MV003")) # 702 RW1415MR variables - 3 equivalents + 362 dbs_master variables= 1061 variables
#Merge dbs_master (PR + HIV) with women's individual recode (IR)
dbs_master15_f <- dbs_master15 %>% filter(hv104 == 2)
dbs_master15_f <- dbs_master15_f %>%
  left_join(RW1415IR, by = c("hv001" = "V001", "hv002" = "V002", "hvidx" = "V003")) # 4572 IR19RW variables - 3 equivalents + 362 dbs_master variables= 4931 variables
#Combine men (PR + HIV + IR) and women (PR + HIV + IR)
dbs_master15 <- bind_rows(dbs_master15_m, dbs_master15_f) # 5630 variables

#Add GPS data to dbs_master15  
dbs_master15 <- dbs_master15 %>%
  left_join(RW1415GE, by = c("hv001" = "DHSCLUST")) # 21 RW1415GE variables - 1 common + 5630 dbs_master14 =5650 variables

#Make df with HIV barcodes only, start by trimming white space on all barcodes
dbs_master15$ha62 <- trimws(dbs_master15$ha62)
dbs_master15$hb62 <- trimws(dbs_master15$hb62)
dbs_master15$hiv01 <- trimws(dbs_master15$hiv01)

# Create new variable "h62" in dbs_master15 to provide DBS barcode BASED ON ha62 and hb62
dbs_master15 <- dbs_master15 %>% mutate(h62 = case_when(
  !is.na(ha62) & grepl(pattern, ha62, perl = TRUE) ~ ha62,
  !is.na(hb62) & grepl(pattern, hb62, perl = TRUE) ~ hb62,
  TRUE ~ NA_character_ ))

# Create new dataframe, retaining only individuals in household member recode who have an HIV (DBS) sample barcode
dbs_15 <- dbs_master15 %>% filter(!is.na(hiv01)) #16,930 samples with HIV barcodes (from hiv01)

## check barcode alignment for HIV blood sample (hiv01, ha62, hb62)
#check <- dbs_15 %>%
#  select(ha62, hb62, h62, hiv01) 
#any(check$h62 != check$hiv01, na.rm = TRUE) #if returns FALSE, then values are identical
#indexes and case ID ***need to be checked manually***
#check <- dbs_15%>% select(hv001, hmhidx, hvidx, idxh4, hhid) 
#Age: hv105 is not correct in 1,000 BUT ha1 and hb1 all match hml16 (corrected age)
#check <- dbs_15 %>% select(hv105, ha1, hb1, hml16) 
#check <- check %>% filter(ha1 != hml16)

#Remove columns that are entirely NA  (2,588 variables remaining)
cleaned_data15 <- dbs_15 %>% select(where(~!all(is.na(.))))

#Make data frame for all samples that were qPCR tested (n=5,050))
rw15_bc_allvars <- cleaned_data15 


#subset variables of interest (optional)
rw15_bc<-rw15_bc_allvars[,c("hv001","hv002","hv003","hv005","hv012","hv023","hv024","hiv05",
                     "hv270","hv246","hv201","hml1","hml16","hv021","hv025","hv040","hv006","barcode",
                     "ha69","hb69","hv104","hv105","hv106","hml10","hml20","hvidx")]

remove(dbs_master15, dbs_master15_f, dbs_master15_m, dbs_15, RW15_bc, rw15_pr,
       RW1415GE, RW1415HIV, RW1415IR, RW1415MR, RW1415PR, cleaned_data15,  
       hiv_missing_barcodes, missing_barcodes)


## selection and recode for study sample analysis -----
#subset to barcodes for which we have qPCR data
qpcr_bc$h62<-qpcr_bc$sample
qpcr_bc<-qpcr_bc[,c("h62","var_CT","pm_CT","po_CT","pv_CT")]

#list of all barcodes for which we have qPCR data matched to DHS
h62s<-RW1415HIV_bc

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

#at least 1 bednet per 1.8 household members y/n (bednetper_cat)
rw15_bc$hh_1.8 <- (rw15_bc$hv012)/1.8
rw15_bc$net_1.8 <- (rw15_bc$hml1)/(rw15_bc$hh_1.8)

#fix values with 0 bed nets
rw15_bc$net_1.8[rw15_bc$net_1.8 == Inf]<-0
rw15_bc <- rw15_bc %>% mutate(bednetper_cat = case_when(
  net_1.8 > 1 ~ 1,
  net_1.8 < 1 ~ 0))

#binary variable for water source (hv201_cat, 1 = piped, 0 = unpiped)
rw15_bc$hv201_cat <- cut(rw15_bc$hv201, breaks=c(0, 12, Inf), labels=c(0,1), include.lowest = TRUE) 

#categorical variable for age (hv105_cat)
rw15_bc$hv105_cat <- cut(rw15_bc$hml16,
                         breaks = c(0, 15, 25, 35, 45, 55, Inf),
                         labels = c(6, 1, 2, 3, 4, 5), include.lowest = TRUE)

#hml10 missing values indicator for using svy
rw15_bc$hml10[is.na(rw15_bc$hml10)]<-12

#factor all categorical variables 
rw15_bc<-merge(rw_land,rw15_bc, by="hv001")
rw15_bc$hv105_cat <- as.factor(rw15_bc$hv105_cat) #actually using values from hml16 corrected age
rw15_bc$hv270 <- as.factor(rw15_bc$hv270)
rw15_bc$hv106 <- as.factor(rw15_bc$hv106)
rw15_bc$hv024 <- as.factor(rw15_bc$hv024)
rw15_bc$hv006 <- as.factor(rw15_bc$hv006)
rw15_bc$landcover <- as.factor(rw15_bc$lc)

## attach temp and rain data
#pull and attach rainfall data from CHIRPS, only needs to be pulled once
# pull coordinates and dates for all survey years 
#ccoord <- as.data.frame(st_drop_geometry(cluster_trans))[, c("hv001", "lat", "long")]
#clusters <- data.frame(lon=c(ccoord$long), lat=c(ccoord$lat))

#survey months are 11-2014 to 04-2015
#dates <- c("2014-10-01", "2015-03-30") 

#pull precipitation data for all clusters during dates range
#precip_data <- get_chirps(clusters, dates, server = "ClimateSERV")
#precip_data <- precip_data %>% separate(date, into = c("year", "month", "day"), sep = "-")

#average precipitation for each month
#p_monthly_avg <- precip_data %>%
#  group_by(id, lat, lon, year, month) %>%
#  summarize(avg_chirps = mean(chirps), .groups = "drop")

#export so we don't have to do the lengthy fetch process every time 
#write_csv(precip_data, "C:/Users/cgait/OneDrive/Desktop/chirps_output.csv")

#attach precipitation data to rw_svy by survey month (1 month lag for rainfall)
chirps <- read.csv("C:/Users/cgait/OneDrive/Desktop/IDEEL/rw_nonpf_data/avg_precip.csv")

#convert precipitation average into columns for each month
chirps <- chirps %>% pivot_wider(names_from = month, values_from = avg_chirps)
chirps$hv001 <- chirps$id
chirps_collapsed <- chirps %>%
  group_by(hv001) %>%
  summarise(across(all_of(as.character(c(10, 11, 12, 1, 2, 3))),
                   ~ coalesce(.[!is.na(.)][1], NA_real_))) %>% ungroup()

#attach to survey data & match based on survey month
rw15_bc <- left_join(rw15_bc, chirps_collapsed, by = "hv001")

#create rain variable, which is the prior month's average in mm
rw15_bc <- rw15_bc %>% mutate(rain = case_when(hv006 == 11 ~ `10`,
                              hv006 == 12 ~ `11`, hv006 == 1 ~ `12`,
                              hv006 == 2 ~ `1`, hv006 == 3 ~ `2`, hv006 == 4 ~`3`))

## match cluster monthly temperature to participants by survey month
#monthly average temperature recorded in the geocovariates
rw_geo <- RW1415GC
rw_geo$hv001 <- rw_geo$DHSCLUST
rw15_bc <- left_join(rw15_bc, rw_geo, by="hv001")
rw15_bc <- rw15_bc %>% mutate(dhs_temp = case_when(hv006 == 11 ~ Temperature_November,
                              hv006 == 12 ~ Temperature_December, hv006 == 1 ~ Temperature_January,
                              hv006 == 2 ~ Temperature_February, hv006 == 3 ~ Temperature_March, 
                              hv006 == 4 ~ Temperature_April))


## data weights---- 
#inverse propensity of selection, HIV sampling, & transmission intensity weighting
#selection into study based on barcodes (n=4,597 matched)
rw15_bc$select <- ifelse(rw15_bc$barcode %in% h62s,1,0)

#replace NAs with 0s in select to use in ps model
rw15_bc$select[is.na(rw15_bc$select)]<-0

#propensity score using full 5,050 barcodes matched 
#including most analysis variables, removed hml10 and hv201_cat from the glm due to missing data, does not change ps values a lot
ps_model <- glm(select ~ hv006+hv024+hv025+hv040+hv104+hv106+hv246+hml1+hml20+hv270+hv105_cat+dhs_temp+rain,
                family=binomial("logit"), data=rw15_bc)
#add ps to dataframe
rw15_bc$ps <- predict(ps_model, rw15_bc, type = "response")
#unstandardized
rw15_bc$ipwt_u <- ifelse(rw15_bc$select==1, 1/rw15_bc$ps, 1/(1-rw15_bc$ps))
#standardized
#probability of selection into analysis from all HIV barcodes
p_exposure <- sum(rw15_bc$select) / nrow(rw15_bc)

#calculate weights based on propensity score
rw15_bc$ipwt <- ifelse(rw15_bc$select==1, p_exposure/rw15_bc$ps, (1-p_exposure)/(1-rw15_bc$ps))
summary(rw15_bc$ipwt, useNA = "always")

#attach transmission intensity categories
rw15_bc <- left_join(rw15_bc, cluster_trans[,c("hv001","trans_intens","region")],by="hv001")

#use HIV weights from HIV recode
rw15_bc$hiv_wt <- rw15_bc$hiv05/1000000

# try running with any missing hiv weights replaced with 
# whatever weight is given for others with the same cluster & sex 
#rw15_bc <- rw15_bc %>% group_by(hv001, hv104) %>%
#           mutate(hiv_wt = ifelse(hiv_wt == 0,
#           max(hiv_wt[hiv_wt != 0], na.rm = TRUE), hiv_wt)) %>% ungroup()
#table(rw15_bc$hiv_wt==0, useNA = "always")

#table(rw15_bc$hiv05==0, useNA = "always") 
#n=286 with 0 HIV weight...replace with 1 for now? otherwise will be excluded from svy calculations
rw15_bc$hiv_wt[rw15_bc$hiv_wt==0]<-1

#count for using svy in full barcodes data set
rw15_bc$count<-1

#study data subset 
rw_svy<-rw15_bc %>% filter(select==1)

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

#rename/combine HIV and ip weights for rw_svy
rw_svy$hiv_ipw <- rw_svy$hiv_wt*rw_svy$ipwt
rw_svy$h62 <- rw_svy$barcode
#table(rw_svy$hiv_ipw, useNA = "always")

#add transmission intensity weights onto study datasets (rw_svy & rw_svy40)
rw_svy<-rw_svy %>% mutate(trans_wt=case_when(
  trans_intens=="high"~high_wt,
  trans_intens=="low"~low_wt))

#calculate the final weights
#wt: HIV * IPSW * trans_wt
rw_svy$wt<-(rw_svy$hiv_ipw)*(rw_svy$trans_wt) 
summary(rw_svy$wt, useNA = "always")
#using 1s to replace missing HIV weights,                            min value = 0.07801, min value = 8.64792
#using cluster ID and participant sex to assign missing HIV weights, min value = 0.07801, max value = 8.64792 


## add qPCR data to survey data-----
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

#mixed infection binary variable (mixed)
rw_svy$mixed <- ifelse(rw_svy$species == "pf_po"|rw_svy$species == "pf_pm"|
                         rw_svy$species == "pf_pv"|rw_svy$species == "pf_pm_pv"| 
                         rw_svy$species == "pm_po"|rw_svy$species == "pf_pm_po"|
                         rw_svy$species == "pm_pv"|rw_svy$species == "pm_po_pv",1, 0)

#CT count variable (CT_count)
rw_svy$CT_count <- rowSums(rw_svy[, c("pf", "pm", "po", "pv")], na.rm = TRUE)

#infection complexity variable (infection)
rw_svy <- rw_svy %>% mutate(infection = case_when(CT_count>1 ~ "co",CT_count==1 ~ "mono",CT_count==0 ~ "none"))

#malaria binary variable
rw_svy <- rw_svy %>% mutate(malaria = case_when(CT_count>0 ~ 1, CT_count==0 ~ 0))

#clusters included in analysis
svy_cluster <- unique(rw_svy$hv001)

#check against cluster_trans
table(cluster_trans$svy_cluster)
table(rw_svy$trans_intens)
#rw_svy <- left_join(rw_svy, DHS_cluster, by="hv001")
  
#pull malaria positive clusters for maps
#pf_pos <- rw_svy %>% filter(pf==1)
#pm_pos <- rw_svy %>% filter(pm==1)
#po_pos <- rw_svy %>% filter(po==1)
#pv_pos <- rw_svy %>% filter(pv==1)


## code new variables for glms, subset 40 cycle cutoff dataset-----

#indicators for nonpf mixed infections
rw_svy <- rw_svy %>% mutate(po_mixed = case_when(
  species=="pf_po" ~ 1,
  species=="pf_pm_po" ~ 1,
  species=="pm_po" ~ 1,
  TRUE ~ 0))
rw_svy <- rw_svy %>% mutate(pm_mixed = case_when(
  species=="pf_pm" ~ 1,
  species=="pf_pm_po" ~ 1,
  species=="pf_pm_pv" ~ 1,
  species=="pm_po" ~ 1,
  species=="pm_pv" ~ 1,
  TRUE ~ 0))
rw_svy <- rw_svy %>% mutate(pv_mixed = case_when(
  species=="pf_pm_pv" ~ 1,
  species=="pm_pv" ~ 1,
  TRUE ~ 0))

#education binary coding 
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

#binary age categories
rw_svy <- rw_svy %>% mutate(age = case_when(
  hv105_cat == "1" ~ "0-24 years",
  hv105_cat == "2" ~ "24 years or older",
  hv105_cat == "3" ~ "24 years or older",
  hv105_cat == "4" ~ "24 years or older",
  hv105_cat == "5" ~ "24 years or older",
  hv105_cat == "6" ~ "0-24 years"))

#landcover binary 
rw_svy <- rw_svy %>% mutate(land = case_when(
  landcover == "1" ~ "forest or woodland",
  landcover == "2" ~ "forest or woodland",
  landcover == "3" ~ "forest or woodland",
  landcover == "4" ~ "crop/grassland",
  landcover == "8" ~ "crop/grassland"))

#at or above average (of all clusters) temperature
rw_svy$dhs_temp_high <- as.integer(rw_svy$dhs_temp >= mean(rw_svy$dhs_temp))

#at or above average prior month's rainfall
rw_svy$rain_high <- as.integer(rw_svy$rain >= mean(rw_svy$rain))


## under 40 CT values data set
rw_svy40<-rw_svy
CT_cutoff<-40
rw_svy40$var_CT[rw_svy40$var_CT > CT_cutoff]<-NA
rw_svy40$pm_CT[rw_svy40$pm_CT > CT_cutoff]<-NA
rw_svy40$po_CT[rw_svy40$po_CT > CT_cutoff]<-NA
rw_svy40$pv_CT[rw_svy40$pv_CT > CT_cutoff]<-NA

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

rw_svy40$CT_count <- rowSums(rw_svy40[, c("pf", "pm", "po", "pv")], na.rm = TRUE)
rw_svy40 <- rw_svy40 %>% mutate(malaria = case_when(CT_count>0 ~ 1, CT_count==0 ~ 0))
rw_svy40$nonpf <- ifelse(rw_svy40$pm == 1 | rw_svy40$po == 1 | rw_svy40$pv == 1, 1, 0)


## survey designs & cluster level proportions-----
#DHS: wt = HIV*ipwt*trans_wt
DHS<-svydesign(id=rw_svy$hv021, strata=rw_svy$hv023, weights=rw_svy$wt, data=rw_svy)
DHS<-as_survey_design(DHS)

#DHS_nowt: null weight, for unweighted proportions
rw_svy$no_wt <- 1
DHS_nowt<-svydesign(id=rw_svy$hv021, strata=rw_svy$hv023, weights=rw_svy$no_wt, data=rw_svy)
DHS_nowt<-as_survey_design(DHS_nowt)

#DHS40: same wt (but 40 cycles CT cutoff dataset)
DHS40<-svydesign(id=rw_svy40$hv021, strata=rw_svy40$hv023, weights=rw_svy40$wt, data=rw_svy40)
DHS40<-as_survey_design(DHS40)

#DHS15: HIV weight only for proportions in the full survey 
DHS15<-svydesign(id=rw15_bc$hv021, strata=rw15_bc$hv023, weights=rw15_bc$hiv_wt, data=rw15_bc, nest=TRUE)
DHS15<-as_survey_design(DHS15)


#proportion of each cluster with household bednets (prop_bednet)
#p_bednet<-as.data.frame(svyby(~hml1_cat, ~hv001, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust"))
#p_bednet$prop_bednet<-p_bednet$hml1_cat
#rw_svy<-left_join(rw_svy, p_bednet[,c("hv001","prop_bednet")],by="hv001")

#proportion of each cluster that slept under a net last night (prop_slept)
#p_slept<-as.data.frame(svyby(~hml20, ~hv001, DHS, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust"))
#p_slept$prop_slept<-p_slept$hml20
#rw_svy<-left_join(rw_svy, p_slept[,c("hv001","prop_slept")],by="hv001")

remove(overall, rw_land, sampled, ps_model, chirps, chirps_collapsed)

hist(rw_svy$wt)
#Descriptive statistics-----
## prevalences for maps/tables -----
total_est<-svyciprop(~malaria, DHS, method="lo")
pf_est<-svyciprop(~pf, DHS, method="lo")
pm_est<-svyciprop(~pm, DHS, method="lo")
po_est<-svyciprop(~po, DHS, method="lo")
pv_est<-svyciprop(~pv, DHS, method="lo")
nonpf_est<-svyciprop(~nonpf, DHS, method="lo")

# expected po/pv overlap?
#svy_n<-length(rw_svy$h62)
#pmpv_est<-pm_est*pv_est
#pmpv_count<-pmpv_est*svy_n
#popv_est<-po_est*pv_est
#popv_count<-popv_est*svy_n

# 40-cycle CT cutoff data & prevalence
svyciprop(~malaria, DHS40, method="lo")
svyciprop(~pf, DHS40, method="lo")
svyciprop(~pm, DHS40, method="lo")
svyciprop(~po, DHS40, method="lo")
svyciprop(~pv, DHS40, method="lo")
svyciprop(~nonpf, DHS40, method="lo")

#prevalence counts by region (45 cycles)
regional_pop <- as.data.frame(svyby(~select, ~region, DHS, svytotal, survey.lonely.psu="adjust"))
regional_malaria <- as.data.frame(svyby(~malaria, ~region, DHS, svytotal, survey.lonely.psu="adjust"))
regional_nonpf <- as.data.frame(svyby(~nonpf, ~region, DHS, svytotal, survey.lonely.psu="adjust"))
regional_pf <- as.data.frame(svyby(~pf, ~region, DHS, svytotal, survey.lonely.psu="adjust"))
regional_pm <- as.data.frame(svyby(~pm, ~region, DHS, svytotal, survey.lonely.psu="adjust"))
regional_po <- as.data.frame(svyby(~po, ~region, DHS, svytotal, survey.lonely.psu="adjust"))
regional_pv <- as.data.frame(svyby(~pv, ~region, DHS, svytotal, survey.lonely.psu="adjust"))

#prevalence counts by region (40 cycles)
regional_malaria40 <- as.data.frame(svyby(~malaria, ~region, DHS40, svytotal, survey.lonely.psu="adjust"))
regional_nonpf40 <- as.data.frame(svyby(~nonpf, ~region, DHS40, svytotal, survey.lonely.psu="adjust"))
regional_pf40 <- as.data.frame(svyby(~pf, ~region, DHS40, svytotal, survey.lonely.psu="adjust"))
regional_pm40 <- as.data.frame(svyby(~pm, ~region, DHS40, svytotal, survey.lonely.psu="adjust"))
regional_po40 <- as.data.frame(svyby(~po, ~region, DHS40, svytotal, survey.lonely.psu="adjust"))
regional_pv40 <- as.data.frame(svyby(~pv, ~region, DHS40, svytotal, survey.lonely.psu="adjust"))

#mixed infection unweighted counts (table s4)
svy_n <- nrow(rw_svy)
mixed_count<-as.data.frame(rw_svy %>% group_by(species) %>% summarise(Total=n()/nrow(.)))
mixed_count$count<-(mixed_count$Total)*svy_n
#infection<-as.data.frame(rw_svy %>% group_by(infection) %>% summarise(Percentage=n()/nrow(.)))

#mixed infection unweighted counts (40 cycles cutoff)
mixed_count40<-as.data.frame(rw_svy40 %>% group_by(species) %>% summarise(Total=n()/nrow(.)))
mixed_count40$count<-(mixed_count40$Total)*svy_n
#infection40<-as.data.frame(rw_svy40 %>% group_by(infection) %>% summarise(Percentage=n()/nrow(.)))

#mixed infection weighted counts (45 cycles)
w_mixed_count<-as.data.frame(svyby(~select, ~species, DHS, svytotal, survey.lonely.psu="adjust"))

#mixed infection weighted counts (40 cycles cutoff)
w_mixed_count40<-as.data.frame(svyby(~select, ~species, DHS40, svytotal, survey.lonely.psu="adjust"))

#study data compared to overall DHS data set (table s2)
svyvars<-c("hv104","hv105_cat","hv270","hv106","hv246","hv201_cat","hml1_cat","hml20","hml10",
           "bednetper_cat","hv024","hv025","hv040_cat","hv006","landcover")

#total weighted counts for samples included in analysis
svy_count <- function(var) {
  formula_str <- as.formula(paste("count ~", var))
  m <- svyby(formula_str, as.formula(paste("~", var)), DHS, svytotal,
             survey.lonely.psu = "adjust", na.rm = TRUE)
  
  df <- as.data.frame(m)
  names(df)[1] <- "Level"
  df$Level <- as.character(df$Level)
  df$Variable <- var
  df %>% select(Variable, Level, count, se.count)
}

#svy_totals <- map_dfr(svyvars, svy_count)
#svy_totals%>% print(noSpaces=T) 

#total weighted counts for all HIV barcodes
DHS_count <- function(var) {
  formula_str <- as.formula(paste("count ~", var))
  m <- svyby(formula_str, as.formula(paste("~", var)), DHS15, svytotal,
             survey.lonely.psu = "adjust", na.rm = TRUE)
  
  df <- as.data.frame(m)
  names(df)[1] <- "Level"
  df$Level <- as.character(df$Level)
  df$Variable <- var
  df %>% select(Variable, Level, count, se.count)
}

#DHS_totals <- map_dfr(svyvars, DHS_count)
#DHS_totals%>% print(noSpaces=T) 


## bivariate glms-----
#http://asdfree.com/demographic-and-heal-surveys-dhs.html

pf_svy<- function(var) {m <- svyglm(as.formula(paste0('pf ~', var)), DHS, family=quasibinomial("identity"))
cbind(tidy(m), confint(m))}
pm_svy<- function(var) {m <- svyglm(as.formula(paste0('pm ~', var)), DHS, family=quasibinomial("identity"))
cbind(tidy(m), confint(m))}
po_svy<- function(var) {m <- svyglm(as.formula(paste0('po ~', var)), DHS, family=quasibinomial("identity"))
cbind(tidy(m), confint(m))}
mixed_svy<- function(var) {m <- svyglm(as.formula(paste0('mixed ~', var)), DHS, family=quasibinomial("identity"))
cbind(tidy(m), confint(m))}

#continuous variables (temp or rain) struggle to map to pf and po with regular functions, but these trys will assist
safe_pf_svy <- function(var) {
  result <- try(pf_svy(var), silent = TRUE)
  if (inherits(result, "try-error")) return(NULL)
  return(result)}

safe_po_svy <- function(var) {
  result <- try(po_svy(var), silent = TRUE)
  if (inherits(result, "try-error")) return(NULL)
  return(result)}

#variables for glms
studyvars <-c("hv104","age","hv105","wealth","educat","hv246","hv201_cat","hml1_cat","hml20",
              "bednetper_cat","hv025","elev1500","dhs_temp","rain","dhs_temp_high","rain_high","land")

#collapsed categories glms for 3 species
#pfsvy_glm<- map_dfr(studyvars, safe_pf_svy)
#colnames(pfsvy_glm) <- c('term','estimate','std.error','statistic','p.value','CIL_95','CIU_95')
#pfsvy_glm %>% print(noSpaces=T) 

#pmsvy_glm<- map_dfr(studyvars, pm_svy) 
#colnames(pmsvy_glm) <- c('term','estimate','std.error','statistic','p.value','CIL_95','CIU_95')
#pmsvy_glm %>% print(noSpaces=T) 

#posvy_glm<- map_dfr(studyvars, safe_po_svy) 
#colnames(posvy_glm) <- c('term','estimate','std.error','statistic','p.value','CIL_95','CIU_95')
#posvy_glm %>% print(noSpaces=T) 

#mixedsvy_glm<- map_dfr(studyvars, mixed_svy) 
#colnames(mixedsvy_glm) <- c('term','estimate','std.error','statistic','p.value','CIL_95','CIU_95')
#mixedsvy_glm %>% print(noSpaces=T) 

## cleanup again
remove(CT_cutoff, p_exposure, cluster_trans, DHS15, qpcr_bc, regional_malaria, regional_malaria40,
       regional_nonpf, regional_nonpf40, regional_pf, regional_pf40, regional_po, regional_po40,
       regional_pm, regional_pm40, regional_pv, regional_pv40, mixed_count, mixed_count40, rw_geo,
       RW1415GC, w_mixed_count, w_mixed_count40)


## table 1 numbers (study population by malaria species)
# Function to apply svyby over each grouping variable for pf infection counts
svyby_pf_totals <- function(vars, outcome = "pf", design = DHS) {
  map(vars, ~ svyby(
    formula(paste0("~", outcome)),
    formula(paste0("~", .x)), design, svytotal,
    survey.lonely.psu = "adjust"))}

pf_totals <- svyby_pf_totals(svyvars)

# Function to apply svyby over each grouping variable for pm infection counts
svyby_pm_totals <- function(vars, outcome = "pm", design = DHS) {
  map(vars, ~ svyby(
    formula(paste0("~", outcome)),
    formula(paste0("~", .x)), design, svytotal,
    survey.lonely.psu = "adjust"))}

pm_totals <- svyby_pm_totals(svyvars)

# Function to apply svyby over each grouping variable for po infection counts
svyby_po_totals <- function(vars, outcome = "po", design = DHS) {
  map(vars, ~ svyby(
    formula(paste0("~", outcome)),
    formula(paste0("~", .x)), design, svytotal,
    survey.lonely.psu = "adjust"))}

po_totals <- svyby_po_totals(svyvars)

# Function to apply svyby over each grouping variable for pv infection counts
svyby_pv_totals <- function(vars, outcome = "pv", design = DHS) {
  map(vars, ~ svyby(
    formula(paste0("~", outcome)),
    formula(paste0("~", .x)), design, svytotal,
    survey.lonely.psu = "adjust"))}

pv_totals <- svyby_pv_totals(svyvars)


#table s4 : sensitivity analysis overall prevalences given observed false positives
#count of pf positives with CT values less than or equal to 40 (1297)
pfunder40 <- sum(rw_svy40$pf)
#total pf positives (1478)
pftotal <- sum(rw_svy$pf)
#pf positives with CT values greated than 40
pfover40 = pftotal-pfunder40

#count of pm positives with CT values less than or equal to 40
pmunder40 <- sum(rw_svy40$pm)
#total pm positives
pmtotal <- sum(rw_svy$pm)
#pm positives with CT values greated than 40
pmover40 = pmtotal-pmunder40

#count of po positives with CT values less than or equal to 40
pounder40 <- sum(rw_svy40$po)
#total po positives
pototal <- sum(rw_svy$po)
#po positives with CT values greater than 40
poover40 = pototal-pounder40

