
# R Script to complie the database with the STECF assessment results by year
# To date (Sept 2014) all assessments have been organized in folders and the data (SSB, R, Landings, F) are extracted in a csv combined by hand. 

library(ggplot2)
library(plyr)
library(dplyr)
library(gamm4)
library(reshape)
library(FLCore)
library(ggplotFL)



setwd("~/GitHub/RAM_legacy_Mediterranean_STECF")

###################################################################################
# DATA PREPARATION
###################################################################################

stocks <- read.csv("input_data/Stocks_stecf_2014_12_02_2014.csv")
names(stocks)[9] <- "Method"

# bring in Fmsy estimates
stocks_msy<-read.csv("input_data/Stocks_stecf_2014_summary_V2.csv")

#stocks<-stocks[,1:10]
stocks_msy<-subset(stocks_msy,select=c(GSA, Species, Stock, Fmsy, Assessment_URL, Meeting) )

stocks2<-merge(stocks, stocks_msy, by=c("GSA", "Stock", "Meeting"), all.x=TRUE)
stocks2$Fref <- NA
names(stocks2)[9] <- "Species"
stocks2 <- subset(stocks2, select = -Species.y)

# read assessment results from EWG 14-14
ewg14_14 <- read.csv("input_data/summary_assessments_ewg14_14_V2.csv")

names(ewg14_14)<-c("Year", "value", "variable", "Meeting", "Stock", "Fmsy", "Fref", "GSA", "Method","Assessment_URL")

ewg <- cast(ewg14_14, Stock + Year + GSA + Fref + Fmsy + Meeting + Method + Assessment_URL
 ~ variable )
names(ewg)<-c("Stock", "Year", "GSA","Fref","Fmsy","Meeting", "Method","Assessment_URL","F","Landings", "R","SSB")
ewg$Species<-NA

# NEED TO UPDATE ASSESSMENTS FROM EWG 14_19, EWG 15_11 and 15_12, 15_19

temp2 <- rbind(stocks2, ewg)
temp2$key <- paste(temp2$Stock,temp2$GSA, temp2$Meeting, sep="_")

# remove stock assessments that are obsolete or replaced by new area aggregations     
obs<-c("ANE_17_STECF 13-19", "HKE_6_2011-11_STECF 11-14", "HKE_7_2013-11_STECF 13 -22",
       "HKE_9_2011-11_STECF 11-14","MUT_6_STECF 13-19","MUT_7_2012-11_STECF 12-19",
       "MUT_9_2013-04_STECF 13-05", "NEP_9_2011-11_STECF 11-14","PIL_17_STECF 13-19",
       "WHB_6_2012-11_STECF 12-19", "WHB_9_2012-11_STECF 12-19")

temp3 <- temp2[!temp2$key %in% obs,]

# Discard assessment data from assessment quasi analytical eg VIT, SURBA etc and
# with only 2-3 time points

#discarded <- c("10_MTS", "15-16_ANK", "17_MTS", "18_MTS", "18_NEP", "25_MUT", "25_SPC",
#               "9_MTS", "9_MUR", "9_POD")
#discarded2 <- c("VIT", "VIT.", "VIT. ", "VIT. F is Fbar 1-3.", "VIT. F is Fbar 1-7.", 
#                 "VIT. F is Fbar 3-7.", "SURBA")

#temp3 <- temp3[!temp3$key %in% discarded,]
#temp3 <- temp3[!temp3$Method %in% discarded2,]

# Calculate
temp3$F_Fmsy <- temp3$F / temp3$Fmsy

species_groups_codes <- read.csv("input_data/species_groups_codes.csv", sep=";")

temp3 <- merge(temp3, species_groups_codes, by.x="Stock", by.y="code", all.x=TRUE)
temp3$GSA <- as.character(temp3$GSA)
temp3$GSA <- ifelse(temp3$GSA=="dic-16","12_16", temp3$GSA)
temp3$GSA <- as.factor(temp3$GSA)

#========================================================================================

# Bring in Assessments from ASSESSMENTS FROM EWG 14_19, EWG 15_11 and 15_12, 15_19


#rm(list=ls())
setwd("input_data/assessment_outputs_summary_CFP")

stock_files <- list.files(pattern=".Rdata", ignore.case = TRUE)
stocks <- list()
for (stk in stock_files){
  # Trim off .Rdata
  load(stk)
  stk_name <- unlist(strsplit(stk, ".R[Dd]ata"))
  stocks[stk_name] <- get(stk_name)
}

plot(FLStocks(stocks))

summarise_stock <- function(stk_in){
  print(name(stk_in))
  out <- rbind(
    cbind(measure = "ssb", as.data.frame(ssb(stk_in))),
    cbind(measure = "rec", as.data.frame(rec(stk_in))),
    cbind(measure = "catch", as.data.frame(catch(stk_in))),
    cbind(measure = "fbar", as.data.frame(fbar(stk_in)))
  )
  return(out[,c("measure", "year","data")])
}

stk_summary <- ldply(stocks, summarise_stock)

# reshape the df to be able to combine with previous years assessment data.

stk_summary$.id <- ifelse(stk_summary$.id == "DPS_1719_EWG16_19", "DPS_1719_EWG15_16", stk_summary$.id)


  stk_summary$Stock <-  substr(stk_summary$.id, 1, 3)
  stk_summary$Area  <-  substr(stk_summary$.id, 5, 6)

  # FIX areas where there are multiple GSA's
  
  stk_summary$Area <- ifelse(stk_summary$.id == "DPS_1719_EWG15_16", "17_18_19" , stk_summary$Area)
  stk_summary$Area <- ifelse(stk_summary$.id == "HKE_1718_EWG15_16", "17_18", stk_summary$Area)
  stk_summary$Area <- ifelse(stk_summary$.id == "ARS_1819_EWG15_16", "18_19", stk_summary$Area)
  stk_summary$Area <- ifelse(stk_summary$.id == "HKE_01-05-06-07_EWG15_11", "01_05_06_07", stk_summary$Area)
  stk_summary$Area <- ifelse(stk_summary$.id == "MUT_1718_EWG15_16", "17_18", stk_summary$Area)
  stk_summary$Area <- ifelse(stk_summary$.id == "HKE_09-10-11_EWG15_11", "09_10_11", stk_summary$Area)
  stk_summary$Area <- ifelse(stk_summary$.id == "MTS_1718_EWG15_16", "17_18", stk_summary$Area)
  stk_summary$Area <- ifelse(stk_summary$.id == "NEP_1718_EWG15_16", "17_18", stk_summary$Area)
  
  # fix areas to remove 0
  stk_summary$Area <- ifelse(stk_summary$Area == "01", "1", stk_summary$Area)
  stk_summary$Area <- ifelse(stk_summary$Area == "05", "5", stk_summary$Area)
  stk_summary$Area <- ifelse(stk_summary$Area == "06", "6", stk_summary$Area)
  stk_summary$Area <- ifelse(stk_summary$Area == "09", "9", stk_summary$Area)
  
  # Get the meeting name
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  stk_summary$Meeting <- substrRight(stk_summary$.id, 8)

  # Reshape dataset to wide format
  stk_summaryW <- dcast(stk_summary, .id + Area + Meeting+ Stock + year  ~ measure, value.var="data") 

  # bring in Reference points
  ref_points <- read.csv("../Stocks_stecf_ReferencePoints_01_2016_v4.csv")
  ref_points$Meeting <- as.character(ref_points$Meeting)
  
  stk_summaryW2 <- merge( stk_summaryW, ref_points, by.x=c("Stock", "Area", "Meeting"), 
                          by.y = c("Stock", "GSA", "Meeting"), all.x = TRUE)
  
  
  asfis <- read.csv("../ASFIS 6 languages_2015.csv")
  
  # Merge with asfis species
 # stk_summaryW3 <-  merge(stk_summaryW2, asfis[, 3:5], by.x = c("Stock","Species"), 
 #                         by.y = c("X3A_CODE", "English_name"), all.x =TRUE)
  stk_summaryW2 <-  merge(stk_summaryW2, asfis[, 3:5], by.x = "Stock", 
                          by.y = "X3A_CODE", all.x =TRUE)
  
 
  ################################################################################################
  
# Merge assessment data with prior data 

  # Match Names in OLD FILES
  names(temp3)[names(temp3)=="Fref"] <- "ref_point"
  temp3_sub <-  subset(temp3, select = c("Stock", "GSA", "Year","R","SSB", "Landings","F", "Species.x","Method", "Assessment_URL","Meeting","Fmsy", "key","ref_point"))
  names(temp3_sub)[names(temp3_sub)=="Species.x"] <- "Species"
  #names(temp3_sub)[names(temp3_sub)=="Species.y"] <- "Scientific_name"
  
  names(temp3_sub)[names(temp3_sub)=="Fmsy"] <- "Fref"
  names(temp3_sub)[names(temp3_sub)=="GSA"] <- "Area"
  names(temp3_sub)[names(temp3_sub)=="Year"] <- "year"
  
  temp3_sub $ Meeting_comments <- NA
  temp3_sub $ Blim <- NA
  
  temp3_sub <-  merge(temp3_sub, asfis[, 3:5], by.x = "Stock", by.y = "X3A_CODE", all.x =TRUE)
  
  
  # Match names in new assessments
  names(stk_summaryW2)[names(stk_summaryW2)==".id"] <- "key"
  names(stk_summaryW2)[names(stk_summaryW2)=="ssb"] <- "SSB"
  names(stk_summaryW2)[names(stk_summaryW2)=="rec"] <- "R"
  names(stk_summaryW2)[names(stk_summaryW2)=="catch"] <- "Landings" # NEED TO VERIFY THIS IS CONSISTENT WITH PREVIOUS DATA EXTRACTIONS
  names(stk_summaryW2)[names(stk_summaryW2)=="fbar"] <- "F"
  names(stk_summaryW2)[names(stk_summaryW2)=="Fref"] <- "ref_point"
  names(stk_summaryW2)[names(stk_summaryW2)=="Fmsy"] <- "Fref"
  
  
  # Combine two pieces of data
  cfp2015 <- rbind(temp3_sub, stk_summaryW2)
  
 # Create Assessment Year Variable
  cfp2015 <-   ddply(cfp2015, .(key),  function(x) {
  max_year = max(x$year)
  x$asses_year = max_year + 1
  return(x)
  })

# END DATA PREPARATION
  
#====================================================================================== 
# Remove outdated assessments  
  
  # remove Black Sea (area 29)
  #cfp2015$Area <- as.character(cfp2015$Area)
  #cfp2015 <- cfp2015[!cfp2015$Area=="29",] 
  
  
  # remove outdated assessment
  # remove stock assessments that are obsolete or replaced by new area aggregations     
  
  # remove assessments replaced by recent joint assessments
  # cfp2015 <-  cfp2015[!(cfp2015$Stock == "HKE" & (cfp2015$Area %in% c("1", "01","5", "6","07" ,"7", "9","09", "10", "11"))),]
  
  # remove outdated assessment
    cfp2015 <- cfp2015[(! cfp2015$key %in% c("ARA_9_2011-11_STECF 11-14", 
                        "DPS_9_2011-11_STECF 11-14", 
                        "NEP_5_2012-11_STECF 12-19",
                        "PIL_6_10-05_SG-MED 10-02")) ,]

#  cfp2015 <- cfp2015[is.na(cfp2015$F)== FALSE , ]
  
  # remove Species and rename ENGLISH NAME to Species
  cfp2015 <- subset(cfp2015, select = -Species)
  names(cfp2015)[names(cfp2015)=="English_name"] <- "Species"
  
save(cfp2015, file = "C:/ram_med/workspace/Mediterranean_cfp2015_forRAM_AllStocks_03_03_2016.RData")
      



#==========================================================================







