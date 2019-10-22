# Generated Late Night schedueles -pw
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
library(bizdays)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# General Functions for Model------------
weight <- function(assoc, wrkDay) {
  p <- which(as.numeric(wrkDay) == preferences(as.numeric(assoc)))
  as.integer(if (length(p) == 0) {
    -100000
  } else {
    p
  })
}

preferences <- function(assoc) preference_data[[assoc]]

# Set Date Range and import Shift Bid/Team data -------------
Today <- as.Date("2019-08-19")
# FewMonth <- Today %m+% months(3)
FewMonth <- as.Date("2019-12-20")

JACKHOL <- as.Date(c("2013-01-01","2013-01-21","2013-02-18","2013-05-18","2013-07-04","2013-09-02","2013-11-28","2013-12-25","2014-01-01","2014-01-20","2014-02-17","2014-05-26","2014-07-04","2014-09-01",
                     "2014-11-27","2014-12-25","2015-01-01","2015-01-19","2015-02-16","2015-04-03","2015-05-25","2015-07-03","2015-09-07","2015-11-26","2015-12-25","2016-01-01","2016-01-18","2016-02-15",
                     "2016-03-25","2016-05-30","2016-07-04","2016-09-05","2016-11-24","2016-12-26","2017-01-02","2017-01-16","2017-02-20","2017-04-14","2017-05-29","2017-07-04","2017-09-04","2017-11-23",
                     "2017-12-25","2018-01-01","2018-01-15","2018-02-19","2018-02-28","2018-03-30","2018-07-04","2018-09-03","2018-11-22","2018-12-25","2019-01-01","2019-01-21","2019-02-18","2019-04-19",
                     "2019-05-27","2019-07-04","2019-09-02","2019-11-28","2019-12-25","2020-01-01","2020-01-20","2020-02-17","2020-04-10","2020-05-25","2020-07-03","2020-09-07","2020-11-26","2020-12-25",
                     "2021-01-01","2021-01-18","2021-02-15","2021-04-02","2021-05-31","2021-07-05","2021-09-06","2021-11-25","2021-12-24","2021-12-31","2022-01-17","2022-02-21","2022-04-15","2022-05-30",
                     "2022-07-04","2022-09-05","2022-11-24","2022-12-26","2023-01-02","2023-01-16","2023-02-20","2023-04-07","2023-05-29","2023-07-04","2023-09-04","2023-11-23","2023-12-25"))
cal <- create.calendar(weekdays=c('saturday', 'sunday'), holidays = JACKHOL, name="Actual")

DateList <- list(data.frame(date= seq(Today, FewMonth, "days")))
DateRange <- bind_rows(DateList)
DateRange <- DateRange %>% mutate(WorkingDay = case_when(is.bizday(date, cal) ~ "Y", TRUE ~ "N"),
                    DOW = weekdays(date),
                    WOY = week(date),
                    Month = months(date)) %>% 
  filter(WorkingDay == "Y")

DOWimport <- read_excel(here("All_Clear_Components_Rules_Aprl2019.xlsx"), sheet="ReqImport") %>% select(-OrigName)
Associmport <- read_excel(here("All_Clear_Components_Rules_Aprl2019.xlsx"), sheet="AssocSched")
MU <- Associmport %>% pull(ProgName)
  
# Loop to create model for each MU and write out results----------

for (Team in MU){
  print(Team)
  
  DOWReq <- DOWimport %>% filter(BidGroup %in% Team) %>% gather(key ="DOW", value="ReqPer", 2:6) #-BidGroup
  Assoc <- Associmport %>% filter(ProgName %in% Team)
  DatesReq <- DateRange %>% left_join(., DOWReq) %>% select(-WorkingDay, -DOW, -BidGroup) %>%
    arrange(date) %>% 
    mutate(DateNumber = row_number())
  
  CapQuest <- unique(DatesReq$ReqPer)
  n <- Assoc %>% pull(`Head Count`)
  m <- DatesReq %>% summarise(count = n()) %>% ungroup() %>% pull(count)
  capacity <- DatesReq %>% pull(ReqPer)
  preference_data <- lapply(seq_len(n), function(x) sample(seq_len(m), m))

  Spots <- sum(DatesReq$ReqPer)
  print(paste0("Trying to fill on average: ", Spots/n, " spots"))
  maxDesired <- floor(Spots/n ) + 1
  minDesired <- floor(Spots/n)
  newm <- m-4
  
  model <- MIPModel() %>%
    add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%
    set_objective(sum_expr(weight(i, j) * x[i, j], i = 1:n, j = 1:m)) %>%
    add_constraint(sum_expr(x[i, j], i = 1:n) == capacity[j], j = 1:m) %>% 
    add_constraint(sum_expr(x[i, j], j = 1:m) >= minDesired, i = 1:n) %>% 
    add_constraint(sum_expr(x[i, j], j = 1:m) <= maxDesired, i = 1:n) %>% 
    add_constraint(x[i, j]+ x[i, j+1]+ x[i, j+2]+ x[i, j+3]+ x[i, j+4] <= 1, i = 1:n, j=1:newm)
  
  result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
  
  matching <- result %>% 
    get_solution(x[i,j]) %>%
    filter(value > .9) %>%  
    select(i, j) %>% 
    rowwise() %>% 
    mutate(weight = weight(as.numeric(i), as.numeric(j)), 
           preferences = paste0(preferences(as.numeric(i)), collapse = ",")) %>% ungroup()
  
  suggSched <- matching %>% rename(assocID = i, datenum = j) %>% left_join(.,DatesReq, by=c("datenum" = "DateNumber")) %>% ungroup() %>% 
    mutate(assigned = 1, TeamName = Team) %>% 
    select(TeamName, assocID, date, assigned) 
  
# Create the final dataset to include all teams
  if (exists("AllTeamLN")){
    AllTeamLN <- rbind(AllTeamLN, suggSched)
    rm(suggSched)
  }
  if (!exists("AllTeamLN")){
    AllTeamLN <- suggSched
    rm(suggSched)
  }  
  
}

# unique(AllTeamLN$TeamName)

AllTeamMU <- AllTeamLN %>% left_join(., Associmport, by=c("TeamName" = "ProgName"))


write.csv(AllTeamMU, here("AllTeams_ProposedLateNight.csv"), row.names=FALSE, na="")









