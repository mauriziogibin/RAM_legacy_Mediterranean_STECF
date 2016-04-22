
# Code to shape Mediterranean STECF assessments to a format matching RAM Legacy formatting
# Author = Giacomo Chato Osio
# Year = 2016


library(RODBC); library(ggplot2)
library(gamm4)

setwd("C:/ram_med")

#Load data from RAM v3.0
ram <- 'RLSADB_v3.0.mdb'


ch <- odbcConnectAccess(ram)

q0 <- paste("SELECT * FROM \"stock\"","'", sep="")
stock <- sqlQuery(ch, q0)

q1 <- paste("SELECT * FROM \"area\"","'", sep="")
area <- sqlQuery(ch, q1)

q2 <- paste("SELECT * FROM \"taxonomy\"","'", sep="")
taxonomy <- sqlQuery(ch, q2)

q3 <- paste("SELECT * FROM \"timeseries_values_views\"","'", sep="")
timeseriesvv <- sqlQuery(ch, q3)

q4 <- paste("SELECT * FROM \"bioparams_values_views\"","'", sep="")
bioparams_values_view <- sqlQuery(ch , q4)
close(ch)

# Merge
t1 <- merge (stock, area, by="areaid")
t2 <- merge (t1, taxonomy, by = c("scientificname","tsn"))
t3 <- merge (t2, timeseriesvv, by = c("stockid", "stocklong")) # merge by assessid
t4 <- merge (t3, bioparams_values_view, by = c("stockid", "stocklong", "assessid"))# merge by assessid
names(t4)[38:41] <- c("B_Bmsy","SSB_SSBmsy","F_Fmsy","U_Umsy")
#names(t4)[47:48] <- c( "B_Bmsytouse","U_Umsytouse")
names(t4)[49] <- "F_Fmgt"
head(t4)

ggplot(t4[t4$classname=="Actinopterygii" & is.na(t4$F_Fmsy)==FALSE,], aes(x=year, y=F_Fmsy))+
  geom_line()+facet_grid(region + family ~ species, drop = TRUE)+
  stat_smooth()+coord_cartesian(ylim = c(0,5), xlim=c(1900,2013))+ 
  stat_hline(yintercept=1, color="red", linetype="dashed")

# US west coast
ggplot(t4[t4$classname=="Actinopterygii" & is.na(t4$F_Fmsy)==FALSE & t4$region=="US West Coast",], 
  aes(x=year, y=B_Bmsy))+
  geom_line()+facet_wrap(myersname ~ family, drop = TRUE)+
  stat_smooth()+coord_cartesian(ylim = c(0,5), xlim=c(1900,2013))+ 
  stat_hline(yintercept=1, color="red", linetype="dashed")
                                  

ggplot(t4[t4$classname=="Actinopterygii" & is.na(t4$B_Bmsy)==FALSE,], aes(x=year, y=B_Bmsy))+
  geom_line()+facet_wrap(region~family, drop = TRUE)+
  stat_smooth()+coord_cartesian(ylim = c(0,5), xlim=c(1900,2013))+ 
  stat_hline(yintercept=1, color="red", linetype="dashed")


# Import Mediterranean Stock Assessments
#load("C:/ram_med/stecf_assessments_upto2014_v2.Rdata")
load("C:/ram_med/workspace/Mediterranean_cfp2015_forRAM_AllStocks_03_03_2016.RData")

med <- cfp2015

#med <- temp3[,1:17]
names(med)

# reconcile GSA with country
area_table <- read.csv("FAO_GFCM_GSAconversionTable2.csv")

temp1 <- merge(med, area_table, by.x="Area", by.y ="GSA", all.x=TRUE)

# Drop an assessment replaced by joint assessments
temp1 <- temp1[!(temp1$key == "HKE_07_EWG14_14") ,]

# fix species namings and groupings

species <- read.csv("C:/ram_med/ASFIS_species.csv")

sort(names(temp1))
names(species)

temp2 <- merge(temp1 , species, 
               by.x=c("Stock", "Scientific_name"), by.y= c("X3A_CODE", "Scientific_name"), 
               all.x = TRUE)


names(temp2) <- c("Stock",
                  "Scientific_name",
                  "areacode", # GSA = areacode
                  "year",
                  "R",
                  "SSB",
                  "TC", # "Landings== TC total catch
                  "F",
                  "assessmethod",
                  "Assessment_URL",
                  "Meeting",
                  "Fref", 
                  "stockid", # key == stockid
                  "ref_point",
                  "assesscomments",
                  "Blim",
                  "Species",
                  "assess_year",
                  "FAO_subarea",
                  "macroarea",
                  "fao",
                  "areatype", # gfcm = areatype
                  "areaid", # GSAname = areaid
                  "ISSCAAP",
                  "TAXOCODE",
                  "commonname",
                  "family",
                  "ordername" 
)

# need to split genus and species in Med data for proper merge with RAM

temp2$genus   <- unlist(lapply(strsplit(as.character(temp2$Scientific_name), " "), "[", 1))
temp2$species <- unlist(lapply(strsplit(as.character(temp2$Scientific_name), " "), "[", 2))

# Fill in extra information on areas, assessor, etc.

temp2 $ region <- rep("Mediterranean-Black Sea", length(temp2$year))
temp2 $ assessorid <- rep("STECF", length(temp2$year))
temp2 $ Assessor_Organization <- rep("Scientific, Technical and Economic Committee for Fisheries", length(temp2$year))
temp2 $ ordername <- tolower(temp2$ordername)


# Create F/Fmsy column
temp2$F_Fmsy <- temp2$F / temp2$Fref 

temp2 $ recorder <- rep("Giacomo Chato Osio", length(temp2$year))
temp2 $ daterecorded <- rep("2016-03-01", length(temp2$year))
temp2 $ assessContact <- rep("Giacomo Chato Osio", length(temp2$year))

# Assessment information
temp2 $ assesscat <- NA
temp2 $ assessmethod <- substr(unlist(lapply(strsplit(as.character(temp2$assessmethod), " "), "[", 1)),1,5)

# Old assessments performed in VIT were classified as Lenghth Cohort Analysis (LCA), can recode as VIT
temp2 $ assessmethod <- ifelse( temp2$ assessmethod == "LCA", "VIT", temp2 $ assessmethod)
temp2 $ assessmethod <- ifelse( temp2$ assessmethod == "sep", "sepVPA", temp2 $ assessmethod)
temp2 $ assessmethod <- ifelse( temp2$ assessmethod == "SUR", "SURBA", temp2 $ assessmethod)
temp2 $ assessmethod <- ifelse( temp2$ assessmethod == "ASP", "ASPIC", temp2 $ assessmethod)

# Link assessment method to assessment category

temp2 $ assesscat <- ifelse( temp2$ assessmethod %in% c("VIT", "XSA", "sepVPA"), "VPA", temp2 $ assesscat)
temp2 $ assesscat <- ifelse( temp2$ assessmethod %in% c("a4a", "SAM","ICA"), "Statistical catch at age model", temp2 $ assesscat)
temp2 $ assesscat <- ifelse( temp2$ assessmethod == "SURBA", "Survey index", temp2 $ assesscat)
temp2 $ assesscat <- ifelse( temp2$ assessmethod == "ASPIC", "Biomass dynamics model", temp2 $ assesscat)
temp2 $ assesscat <- ifelse( temp2$ assessmethod == "SS3", "Integrated Analysis", temp2 $ assesscat)

# Create stock description

temp2 $ stocklong <- paste(temp2 $ commonname, " ", temp2 $ areaid, " GSA =", temp2 $ areacode )


save(temp2, file = "C:/ram_med/workspace/Mediterranean_RAM_AllStocks_03_03_2016.RData")


unique(paste(temp2$stockid, temp2$ref_point))

med_units <- data.frame(stock = unique(paste(temp2$stockid, temp2$assessmethod, sep=" ")),
          R =   rep(NA, length(unique(paste(temp2$stockid, temp2$assessmethod)))),
          TC =  rep(NA, length(unique(paste(temp2$stockid, temp2$assessmethod)))),
          SSB = rep(NA, length(unique(paste(temp2$stockid, temp2$assessmethod)))),
          F =   rep(NA, length(unique(paste(temp2$stockid, temp2$assessmethod))))
          )

med_units$assessmethod  <- sapply(strsplit(as.character(med_units$stock), ""), function(x, n) paste(tail(x, n), collapse = ""), 3)

med_units[,2:5] <- ifelse(med_units$assessmethod == "XSA", c("E03","MT","MT","1/yr"), NA )

apply(med_units[,2:5], FUN= ifelse(med_units$assessmethod == "XSA", c("E03","MT","MT","1/yr"), NA ))





ggplot(temp2, aes(year, F, color = paste(Stock, areacode, sep="")))+ geom_line()+
  stat_hline(yintercept=1, color="red", linetype="dashed")+
  guides(col = guide_legend(ncol = 4), fill = guide_legend(title = "Stock"))

ggsave(last_plot(), file=paste("plots/F_Fmsy_MEDBS_all",Sys.Date(),".png"), width=9, height=6, dpi=300)


# Merge with RAM

tall <- merge(temp2, t4,  by = intersect(names(temp2), names(t4)), all=TRUE)
tall$region <- as.character(tall$region)
tall$region <- ifelse(tall$region=="Mediterranean-Black Sea", "MEDBS", tall$region)
tall$ordername <- tolower (tall$ordername)


# SUMMARY STATS FOR ALL ASSESSMENTS
summary(tall)

length(unique(paste(temp3$Stock, temp3$GSA, sep="")))


# Compare
#    1 median level of F/Fmsy by family/order by macro area
#   Bear in mind that I am comparing F/F0.1 from Med and F/Fmsy from other areas


ggplot(droplevels(tall[is.na(tall$F_Fmsy)==FALSE, ]), aes(x=year, y=F_Fmsy))+
  geom_point()+facet_wrap(region ~ family , drop = TRUE)+
  stat_smooth()+coord_cartesian(ylim = c(0,8), xlim=c(1900,2014))+ 
  stat_hline(yintercept=1, color="red", linetype="dashed")

ggplot(droplevels(tall[is.na(tall$F_Fmsy)==FALSE   , ]), aes(x=year, y=F_Fmsy))+
  geom_point()+facet_wrap(ordername ~ region , drop = TRUE)+
  stat_smooth()+coord_cartesian(ylim = c(0,8), xlim=c(1999,2014))+ 
  stat_hline(yintercept=1, color="red", linetype="dashed")

ggplot(droplevels(tall[is.na(tall$tc_ssb)==FALSE, ]), aes(x=year, y=tc_ssb))+
  geom_point()+facet_wrap( family ~ region , drop = TRUE)+
  stat_smooth()+coord_cartesian(xlim=c(1999,2014), ylim = c(-1, 5))

ggsave(last_plot(), file=paste("plots/TC_SSB_Region_Family3",Sys.Date(),".png"), width=18, height=12, dpi=300)


ggplot(tall, aes(x=year, y=F_Fmsy, color=region))+
  geom_point()+facet_wrap(family ~ .  , drop = TRUE)+
  stat_smooth()+coord_cartesian(ylim = c(0,8), xlim=c(1950,2013))+ 
  stat_hline(yintercept=1, color="red", linetype="dashed")
ggsave(last_plot(), file=paste("plots/F_Fmsy_Region_Order",Sys.Date(),".png"), width=18, height=12, dpi=300)

# F_Fmsy by Order with a smoother through regions
ggplot(tall, aes(x=year, y=F_Fmsy, color=ordername))+
  #geom_point(aes(alpha=0.5))+
  facet_wrap( region~ordername  , drop = TRUE)+
  stat_smooth()+coord_cartesian(ylim = c(0,8), xlim=c(1950,2013))+ 
  stat_hline(yintercept=1, color="red", linetype="dashed")

# Only clupeiformi
# F_Fmsy by Order with a smoother through regions
ggplot(droplevels(tall[tall$ordername=="clupeiformes",]), aes(x=year, y=F_Fmsy, color=ordername))+
  #geom_point(aes(alpha=0.5))+
  facet_wrap( region ~ species  , drop = TRUE)+
  stat_smooth()+coord_cartesian(ylim = c(0,8), xlim=c(1950,2014))+ 
  stat_hline(yintercept=1, color="red", linetype="dashed")


ggplot(tall, aes(y=F_Fmsy,x=year, color=region))+
   geom_boxplot()+facet_grid( region~ordername  , drop = TRUE)+
coord_cartesian(ylim = c(0,5), xlim=c(1950,2014))+ 
  stat_hline(yintercept=1, color="red", linetype="dashed")

ggplot(tall, aes(y=F_Fmsy,x=year, color=region))+
  geom_point()+facet_grid( region~genus  , drop = TRUE)+
  coord_cartesian(ylim = c(0,5), xlim=c(1950,2013))+ 
  stat_hline(yintercept=1, color="red", linetype="dashed")


# trim down data 

tall1<-(subset(tall, select= c("scientificname.x","areacode","year","R","TC","F","F_Fmsy","commonname",         
                              "family","ordername","region","genus",              
                               "species","Stock", "key" )))

tall1<-na.omit(subset(tall, select= c("year","F_Fmsy","family","ordername","region","genus",              
                               "species", "key")))

tall1$family <- as.factor(tall1$family)
tall1$region <- as.factor(tall1$region)

dim(na.omit(tall1))



#This is really interesting. I like the approach of accounting for other sensible covariates
#and then looking at baseline differences in F's. There are some tweaks that could be done on
#the modelling side but these are minor and I agree that it should be kept as simple as 
#possible. One nice way of plotting it could be boxplots of latest x years grouped by some 
#high level of taxonomic aggregation with regions colour coded - very closely alligned with 
#the model below only omitting s(year) by restriction to last few years. We could overlay the
#fitted means from the model too (might be some interactions of interest between family and 
#region effects).

# work out random part first
g1 <- gamm4(log(F_Fmsy)~s(year, by=region)+ordername , random=~(1|key),
            data=tall1[tall1$F_Fmsy >0 & tall1$year>1990 & tall1$year<2013,], na.action=na.omit)

g11 <- gamm4(log(F_Fmsy)~s(year, by=region)+ordername , random=~(1|family),
            data=tall1[tall1$F_Fmsy >0 & tall1$year>1990& tall1$year<2013,], na.action=na.omit)

g2 <- gamm4(log(F_Fmsy)~s(year, by=region)+ordername , random=~(1|ordername),
            data=tall1[tall1$F_Fmsy >0 & tall1$year>1990& tall1$year<2013,], na.action=na.omit)
  
g3 <-  gamm4(log(F_Fmsy)~region+s(year)+ordername, random=~(1|family), 
            data=tall1[tall1$F_Fmsy >0 & tall1$year>1990& tall1$year<2013,], na.action=na.omit)

g31 <- gamm4(log(F_Fmsy)~s(year, by = region), random=~(1|family),
            data=tall1[tall1$F_Fmsy >0 & tall1$year>1990& tall1$year<2013,], na.action=na.omit)

# drop interaction
g4 <- gamm4(log(F_Fmsy)~(year)+region + ordername, random=~(1|family),
            data=tall1[tall1$F_Fmsy >0 & tall1$year>1990 & tall1$year<2013,], na.action=na.omit)

g5 <- gamm4(log(F_Fmsy)~year+region , random=~(1|family),
            data=tall1[tall1$F_Fmsy >0 & tall1$year>1990 & tall1$year<2013,], na.action=na.omit)

# drop fixed terms
g6 <- gamm4(log(F_Fmsy)~ s(year)  , random=~(1|family),
      data=tall1[tall1$F_Fmsy >0 & tall1$year>1990 & tall1$year<2013,], na.action=na.omit)

# test convergence
relgrad <- with(g2$mer@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

AIC( g1$mer, g11$mer , g2$mer,g3$mer,g4$mer, g5$mer, g6$mer)

summary(g1)

tt <- gamm4(log(F_Fmsy)~ s(year, by= region)+SSB+R+TC , random=~(1|family),
      data=tall1[tall$F_Fmsy >0 & tall$year>1990,], na.action=na.omit)

#    2 level of Fmsy by order


#    3 SSB / Landings
tall$tc_ssb <- tall$TC / tall$SSB

ggplot(tall[tall$year >1970 & tall$region=="MEDBS" & is.na(tall$tc_ssb)==FALSE ,], aes(x=year, y=TC/SSB, color=region))+geom_point()+
  facet_grid(family~GROUP, scales= "free_y")

ggplot(tall[tall$year >1970  & is.na(tall$tc_ssb)==FALSE ,], aes(x=year, y=tc_ssb, color=region))+geom_point()+
  facet_grid(order~GROUP, scales= "free_y")


#    4 R / SSB


#    5 phase plot scaled SSB / F/Fmsy
tall2 <- ddply(tall, .(stockid), transform, sSSB = (SSB/mean(SSB, na.rm=TRUE)))

ggplot(tall2[tall2$region=="MEDBS" & is.na(tall2$sSSB)==FALSE ,], 
       aes(x=sSSB, y=F_Fmsy))+geom_path()+
  facet_grid( .~ ordernames , scales= "free_y")

ggplot(tall2, 
       aes(x=sSSB, y=F_Fmsy, color= ordername))+geom_path()+
  facet_grid( region ~ ., scales= "free_y")


#    6 Proportion of Catches assessed (sum catch in assessment and GSA by year 
#      and compare with GFCM landings)
