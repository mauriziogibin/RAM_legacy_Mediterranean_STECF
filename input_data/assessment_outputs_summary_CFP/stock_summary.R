library(FLCore)
library(ggplotFL)
library(plyr)
rm(list=ls())

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
View(stk_summary)

