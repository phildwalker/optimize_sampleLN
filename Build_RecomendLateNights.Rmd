---
title: "Generating Late Night Schedules for Shift Bid"
output:
  html_document:
    toc: true
    toc_float: true
    code_folding: hide
date: "`r format(Sys.time(), '%d %B, %Y')`"
params:
  SelectTeam:
    label: "Select MU:"
    value: NF_Part_GP
    input: select
    choices: [NF_Part_GP, NF_Pay_GP, NF_XFER_GP, NF_Adv_GP, NF_Adv_PSP, NF_NDiv, NF_Div, F_Part_GP, F_Part_PSP, F_Pay_GP, F_Pay_PSP, F_XFER_GP, F_XFER_PSP, TRADE, ANNUIT, NF_VA_QC]
  StartDate:
    label: "Start Date:"
    value: !r as.Date("2019-04-15")
    input: date
  EndDate:
    label: "End Date:"
    value: !r as.Date("2019-08-16")
    input: date
  # FileName:
  #   value: !r paste0(params$SelectTeam,"_ReportResults.html")
  # dataImport:
  #   label: "Input dataset:"
  #   value: results.csv
  #   input: file
---

## Initial Data processing
Serving as the initial version.  
The goal is to use linear programming to generate a schedule for assocaites which meet the contraints set by the business.

Goal 1: Meet to required number of associates schedule to late night for each day.   
Goal 2: Set a max number of LN assigned to assocaites by week  
Goal 3: Generate a schedule for the next shift bid period (excluding holidays from list)

```{r setup,include=FALSE,echo=FALSE,message=FALSE}
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
library(rpivotTable)

JACKHOL <- as.Date(c("2013-01-01","2013-01-21","2013-02-18","2013-05-18","2013-07-04","2013-09-02","2013-11-28","2013-12-25","2014-01-01","2014-01-20","2014-02-17","2014-05-26","2014-07-04","2014-09-01",
                     "2014-11-27","2014-12-25","2015-01-01","2015-01-19","2015-02-16","2015-04-03","2015-05-25","2015-07-03","2015-09-07","2015-11-26","2015-12-25","2016-01-01","2016-01-18","2016-02-15",
                     "2016-03-25","2016-05-30","2016-07-04","2016-09-05","2016-11-24","2016-12-26","2017-01-02","2017-01-16","2017-02-20","2017-04-14","2017-05-29","2017-07-04","2017-09-04","2017-11-23",
                     "2017-12-25","2018-01-01","2018-01-15","2018-02-19","2018-02-28","2018-03-30","2018-07-04","2018-09-03","2018-11-22","2018-12-25","2019-01-01","2019-01-21","2019-02-18","2019-04-19",
                     "2019-05-27","2019-07-04","2019-09-02","2019-11-28","2019-12-25","2020-01-01","2020-01-20","2020-02-17","2020-04-10","2020-05-25","2020-07-03","2020-09-07","2020-11-26","2020-12-25",
                     "2021-01-01","2021-01-18","2021-02-15","2021-04-02","2021-05-31","2021-07-05","2021-09-06","2021-11-25","2021-12-24","2021-12-31","2022-01-17","2022-02-21","2022-04-15","2022-05-30",
                     "2022-07-04","2022-09-05","2022-11-24","2022-12-26","2023-01-02","2023-01-16","2023-02-20","2023-04-07","2023-05-29","2023-07-04","2023-09-04","2023-11-23","2023-12-25"))
cal <- create.calendar(weekdays=c('saturday', 'sunday'), holidays = JACKHOL, name="Actual")

# Today <- as.Date("2019-04-15")
# FewMonth <- Today %m+% months(3)
# FewMonth <- as.Date("2019-08-16")

Today <- params$StartDate
FewMonth <- params$EndDate

# fileImport <- params$dataImport


```


``` {r data_wrangle,include=FALSE,echo=FALSE,message=FALSE}

DateList <- list(data.frame(date= seq(Today, FewMonth, "days")))
Q2 <- bind_rows(DateList)
Q2 <- Q2 %>% mutate(WorkingDay = case_when(is.bizday(date, cal) ~ "Y", TRUE ~ "N"),
                    DOW = weekdays(date),
                    WOY = week(date),
                    Month = months(date)) %>% 
  filter(WorkingDay == "Y")

Q2summ <- Q2 %>% group_by(DOW) %>% summarise(Amount = n())

# Team <- c("NF_Part_GP")
Team <- params$SelectTeam

DOWReq <- read_excel(here("All_Clear_Components_Rules_Aprl2019.xlsx"), sheet="ReqImport") %>% select(-OrigName) %>% filter(BidGroup == Team) %>% gather(key ="DOW", value="ReqPer", 2:6) #-BidGroup
Assoc <- read_excel(here("All_Clear_Components_Rules_Aprl2019.xlsx"), sheet="AssocSched") %>% filter(ProgName == Team)
DatesReq <- Q2 %>% left_join(., DOWReq) %>% select(-WorkingDay, -DOW, -BidGroup) %>%
  arrange(date) %>% 
  mutate(DateNumber = row_number())

minWY <- min(DatesReq$WOY)
maxWY <- max(DatesReq$WOY)

CapQuest <- unique(DatesReq$ReqPer)

# This is the amount of associates we are assigning to late nights
n <- Assoc %>% pull(`Head Count`)

# This is the amount of days we are covering...
m <- DatesReq %>% summarise(count = n()) %>% ungroup() %>% pull(count)
capacity <- DatesReq %>% pull(ReqPer)

preference_data <- lapply(seq_len(n), function(x) sample(seq_len(m), m))
preferences <- function(assoc) preference_data[[assoc]]
# preferences(1)

weight <- function(assoc, wrkDay) {
  p <- which(as.numeric(wrkDay) == preferences(as.numeric(assoc)))
  as.integer(if (length(p) == 0) {
    -100000
  } else {
    p
  })
}

# weight(1,1,5)

Spots <- sum(DatesReq$ReqPer)
maxDesired <- floor(Spots/n ) + 1
minDesired <- floor(Spots/n) 
```

```{r Show the team that the is being used for the example}

print(paste0("The team used for this example is: ", Team))

```


## Review of Requirements  
The chart below shows that required number of associates required per late night varies by day of week. 
It is possible to set specific dates with different date requirements (ie day after a holidy we might want more late night associates available).

```{r Showing Example of Late night requirements, fig.width= 12}
ggplot(DatesReq,aes(date, ReqPer))+
  geom_line(color="grey")+
  geom_point(shape=4, color="red")+
  theme_bw()+
  scale_y_continuous(breaks= c(0:max(CapQuest)))+
  scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%d %b")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ggtitle(paste0("Late Nights Required for team: ", Team))

```

## Result of Randomization in preferences  
This allows the user to see that preferences are generated randomly, which allows for associates to recieve a randomized schedule as the generated preferences will change on every run.  

```{r visualization of potential scoring, fig.width= 12}
test <- as.data.frame(do.call(rbind, preference_data))
test$assocID <- as.numeric(rownames(test))
numc <- ncol(test)-1
# testlong <- 
test %>% gather( key="dayNum", "rank", 1:numc) %>% group_by(dayNum) %>% summarise(ttlRank = sum(rank)) %>% 
  mutate(Date = substring(dayNum, regexpr("V", dayNum) + 1),
         Date = as.numeric(Date)) %>% left_join(.,DatesReq, by=c("Date" = "DateNumber")) %>% 
  ggplot(aes(date, ttlRank))+
  geom_bar(stat = "identity") + 
  viridis::scale_fill_viridis(discrete = TRUE) + 
  scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%d %b")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  # ggtitle(paste0("Late Nights Required for team: ", Team))+
  theme_bw()

```


## Model Generation
```{r create optimization model,include=FALSE,echo=FALSE,message=FALSE}
# daygap <- 5
# sx = seq(0, m - 6, 6)
# j = 1:6 + sx

newm <- m-4
# constr <-c("x[i, j]+ x[i, j+1]+ x[i, j+2]+ x[i, j+3]+ x[i, j+4] <= 1")
# constr
# parse(text=constr)

model <- MIPModel() %>%
  
  # 1 iff associate i is assigned to late night date m ...also maybe WOY y
  add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%
  
  # maximize the preferences
  set_objective(sum_expr(weight(i, j) * x[i, j], i = 1:n, j = 1:m)) %>%
  
  # we must equal the requirements for that date
  add_constraint(sum_expr(x[i, j], i = 1:n) == capacity[j], j = 1:m) %>% 
  
  # each associate should be assigned to at least one late night
  add_constraint(sum_expr(x[i, j], j = 1:m) >= minDesired, i = 1:n) %>% 

  # each associate should be assigned to less than _x_ late nights
  add_constraint(sum_expr(x[i, j], j = 1:m) <= maxDesired, i = 1:n) %>% 
  
  # associates cannot be assigned a LN less than 5 days ago
  # add_constraint(sum_expr(x[i, j], x[i, j+5]) <= 1, i = 1:n)
  # add_constraint(sum_expr(x[i, j], j = 1:5 + sx) <= 1, sx = seq(0, m-5 , 5), i = 1:n)
  add_constraint(x[i, j]+ x[i, j+1]+ x[i, j+2]+ x[i, j+3]+ x[i, j+4] <= 1, i = 1:n, j=1:newm)


model

```

```{r Run Optim Model}
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

```

## Generated Schedule
```{r Create suggested schedule, fig.width= 12}
matching <- result %>% 
  get_solution(x[i,j]) %>%
  filter(value > .9) %>%  
  select(i, j) %>% 
  rowwise() %>% 
  mutate(weight = weight(as.numeric(i), as.numeric(j)), 
         preferences = paste0(preferences(as.numeric(i)), collapse = ",")) %>% ungroup()

suggSched <- matching %>% rename(assocID = i, datenum = j) %>% left_join(.,DatesReq, by=c("datenum" = "DateNumber")) %>% ungroup() %>% mutate(assigned = 1)  

suggSched %>% mutate(ReqPer = factor(ReqPer)) %>% 
  ggplot(aes(date, assocID))+
  geom_tile(aes(fill=ReqPer), color="black")+
  theme_bw()+
  scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%d %b")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+ #, legend.position="none"
  scale_y_continuous(breaks= c(1:n))+
  ggtitle(paste0("Recommented Late Night Schedule for team: ", Team))


```

## Pivot table to play with results
```{r General Stats of Results,results='asis'}
suggSched <- suggSched %>% mutate(Month = paste0(month(date),"_",months(date)))
  
rpivotTable(suggSched, rows = "assocID", cols= c("Month", "WOY"), rendererName = "Heatmap")
  
  
```


```{r save off results,include=FALSE,echo=FALSE,message=FALSE}
AllTeamMU <- suggSched %>% mutate(Team= Assoc$Group, ProgramName=Assoc$ProgName) %>% select(-datenum, -weight, -preferences) #left_join(., Assoc, by=c("TeamName" = "ProgName"))

OutputName <- paste0(Team,"_ProposedLateNight.csv")
write.csv(AllTeamMU, here("ManualRuns",OutputName), row.names=FALSE, na="")
# write.csv(AllTeamMU, OutputName, row.names=FALSE, na="")

```




















