# Transpose final LN assignments -pw
# Tue Feb 19 12:06:07 2019 ------------------------------
rm(list = ls())
cat("\014")
ifelse(is.null(dev.list()["RStudioGD"]), 
       print("No charts"), 
       dev.off(dev.list()["RStudioGD"])
)

detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

library(tidyverse)
library(dtplyr)
library(lubridate)
library(stringr)
library(here)
library(readxl)


FinalLN <- read_excel(here("AllTeams_ProposedLateNightw_Pivot_PWxlsx.xlsx"), sheet="Sheet1")

TranspoRes <- FinalLN %>% group_by(Group, assocID) %>% summarise(DateList = paste(date, collapse=",")) %>% ungroup()
write.csv(TranspoRes, here("FinalResults_Aggreg.csv"), row.names=FALSE, na="")


TranspoResINDVcell <- FinalLN %>% group_by(assocID, Group) %>% mutate(LN_day = paste0("LN_day", row_number())) %>% spread(key=LN_day, value=date) %>% ungroup()
write.csv(TranspoResINDVcell, here("FinalResults_Aggreg_CellPerDay.csv"), row.names=FALSE, na="")

