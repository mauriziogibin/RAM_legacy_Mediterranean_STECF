
#-------------------------------------------------------------------------------
#
# Script to prepare RAM Legacy for hte MED for Tableau
#
# By: Maurizio Gibin
# Code by: Maurizio Gibin
# Contact: maurizio.gibin@jrc.ec.europa.eu
#
# Date: 2016-09-02
#
# 
#-------------------------------------------------------------------------------
rm(list = ls(all.names = TRUE))
gc()
library(data.table)
library(stringr)
library(ggplot2)
####################
setwd("/home/gibinma/work/Ram Data/")
options(scipen = 999)
options(digits=9)
####################
codePath  <- "/home/gibinma/work/Ram Data/scripts/"         #Location where you store R scripts
dataG  <- "/home/gibinma/work/Ram Data/data/"               #Location where you store data
outPath   <- "/home/gibinma/work/Ram Data/results/"         #Location where you want to store the results
shpPath   <- "/home/gibinma/work/Ram Data/data/shp/"        #Location where you store the gis boundaries
# Setting the working driectory
setwd(dataG)
#####################################################################################
#####################################################################################
# 
RAMXTO   <- fread("RamDB_CLEAN.csv")

# Long preparation process to change from wide to long the stocks that
#
#

load("Mediterranean_RAM_med2_26_07_2016.RData")

RAM <- med2

names(RAM)   <- toupper(names(RAM))
str_detect(RAM$`AREA CODE`,pattern = glob2rx(ptrn.p))

RAM$`AREA CODE`  <-paste0('SA  ',RAM$AREACODE)

RAM$`AREA CODE`  <-gsub('_',' SA  ',RAM$`AREA CODE`)
RAM$`AREA CODE`  <-gsub('-',' SA  ',RAM$`AREA CODE`)

ptrn.p <- "SA ??"

strcount <- function(x, pattern, split){
  unlist(lapply(
    strsplit(x, split),
    function(z) na.omit(length(grep(pattern, z)))
  ))
}

#
RAM$NUMBERSA   <- strcount(RAM$`AREA CODE`, "SA ??", " ")
RAMX <- as.data.table(RAM[rep(seq(nrow(RAM)), RAM$NUMBERSA),1:21,])
setkey(RAMX,'YEAR')

RAMX[,print(.SD),by='YEAR']
RAMX[,print(.SD),by=.(YEAR,STOCKID)]

RAMX[,NUMSASEQ:=seq_along(.SD),by=.(YEAR,STOCKID)]

str_extract(RAMX[800]$`AREA CODE`, "\\d")

RAMX[,START:=1+(NUMSASEQ-1)*6,by=.(YEAR,STOCKID)]
RAMX[,STOP:=START+5,by=.(YEAR,STOCKID)]
RAMX[,AREA_CODE:=substring(RAMX$`AREA CODE`,START,STOP),by=.(YEAR,STOCKID)]

RAMX[,UNIQUEID:=paste0(STOCKID,YEAR),by=.(YEAR,STOCKID)]

RAMX   <- RAMX[!duplicated(RAMX$UNIQUEID),]

write.csv(RAMX,'RAMK2.csv',row.names=F)

RAMX2   <- fread("RAMK2.csv")

# Additional Variables
load("Mediterranean_RAM_med2_26_07_2016.RData")
RAM   <- med2;    med2   <- NULL;   gc()
RAM   <- as.data.table(RAM)
RAM$STOCKID <- RAM$stockid
setkey(RAM,'STOCKID')
setkey(RAMX,'STOCKID')

# Unique URL list
URL.list   <- unique(RAM$Assessment_URL)

# Unique Stock ID list
StockID.list    <- unique(RAM$STOCKID)
RAM.Un   <- unique(RAM)

# Join for additional variables and omti duplicates
RAMXF   <- RAMX[RAM.Un]
names(RAMXF)   <-toupper(names(RAMXF))
unique(names(RAMXF))
RAMXF   <- RAMXF[,unique(names(RAMXF)),with=F]

# Cleaning errors
unique(RAMX$MACROAREA)
RAMX$MACROAREA   <- gsub('Easter','Eastern',RAMXF$MACROAREA)
unique(RAMXF$`AREA CODE`)
RAMXF$`AREA CODE`   <- gsub('22-23','22 SA 23',RAMXF$`AREA CODE`)

# Omitting the temp columns I needed to reshape
RAMXF   <- RAMXF[,`:=`(NUMBERSA=NULL,NUMSASEQ=NULL,START=NULL,STOP=NULL,I.SSB=NULL,I.R=NULL,I.TC=NULL,I.F=NULL),]

# Finally saving
write.csv(RAMXF,'RAMK.csv',row.names=F)
RAMXF   <-fread("RAMK.csv")
RAMXF[,STATUS:=NULL,]
keyZ   <- c("STOCKID","YEAR")
setkeyv(RAMXF,keyZ)

RAMXF[F_FMSY>0,print(.SD),by=keyZ]

# Getting the last year by partition
RAMSTUN   <- RAMXF[!is.na(F_FMSY),.SD[.N],by=STOCKID]
# Creating the STATUS Varialb eclaculated on the last year available
RAMSTUN[,'STATUS':= ifelse(F_FMSY<=1,"SUSTAINABLE","OVEREXPLOITED"),]
#Selecting only the variable I wanna keep
RAMSTUN   <- RAMSTUN[,.(STOCKID,STATUS),]
#Merging the datasets
RAMXF<-merge(RAMXF,RAMSTUN,by='STOCKID',all.x=T)

write.csv(RAMXF,"RAMK.csv",row.names = F)
