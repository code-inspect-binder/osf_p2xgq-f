library(dplyr)
library(readxl)
library(lubridate)
library(lme4)
library(ggplot2)
library(DHARMa)
library(car)
library(glmmTMB)
library(ggeffects)
library(gtable)
library(grid)
library(egg)
library(survival)
library(coxme)
library(tidyr)
library(cowplot)
library(multcomp)
library(ordinal)
library(ggnewscale)
library(plm)
library(forcats)
library(jtools)
library(huxtable)
library(emmeans)
options(scipen = 999)

##### Data input and cleaning #####
# start working with clean dataset in line 91 - this is the 'complete dataset 08092022.csv' in the OSF file

participation_data <- read_excel("participation data sanctuary 080922 pilot+.xlsx")
participation_datawild <- read_excel("participation data wild 170322.xlsx")

participation_data$context <- "Sanctuary"
participation_datawild$context <- "Wild"

# correct all names in dataset, using Stef's group comp as 'correct' names - these need to match later in order to merge
# the touchscreen data with the group composition to see who participated out of the group

# do this separately for sanctuary and wild datasets as some names overlap (i.e. Liffey and Liffie are spelt the same in the original dataset, need to differentiate)

participation_data$Individual<-as.factor(participation_data$Individual)
levels(participation_data$Individual)
# extract names and group they appear in
namesgroup <- table(participation_data$Individual, participation_data$Group)
namesgroup <- as.data.frame(namesgroup)
namesgroup <- subset(namesgroup, namesgroup$Freq!=0)

write.csv(namesgroup, "touchscreennames sanctuary 080922 uncorrected.csv")

# load in dataset of touchscreen names and the corrections from sanctuary masterlist
touchscreennames <- read.csv("touchscreennames sanctuary 080922.csv")
library(DataCombine)
participation_data<-as.data.frame(participation_data)
participation_data$Individual<-as.factor(participation_data$Individual)
participation_data$Individual<-as.character(participation_data$Individual)
participation_data <- FindReplace(data=participation_data, Var="Individual", replaceData=touchscreennames, 
                                  from="Var1", to="correctname", exact=TRUE, vector=FALSE)
participation_data$Individual <- as.character(participation_data$Individual)
participation_data$Individual <- dplyr::recode(participation_data$Individual, "gyzmo" = "Gizmo", "fanjan"="Fanjan")
detach("package:DataCombine", unload=TRUE)
participation_data$Individual <- as.factor(participation_data$Individual)
levels(participation_data$Individual)


# now for the wild groups
participation_datawild$Individual<-as.factor(participation_datawild$Individual)
levels(participation_datawild$Individual)
participation_datawild$Group <- dplyr::recode(participation_datawild$Group, "Ak"="AK", "Kb"="KB", "Nh"="NH")

# extract names and group they appear in
namesgroup <- table(participation_datawild$Individual, participation_datawild$Group)
namesgroup <- as.data.frame(namesgroup)
namesgroup <- subset(namesgroup, namesgroup$Freq!=0)

write.csv(namesgroup, "touchscreennames wild 080922 uncorrected.csv")

# load in dataset of touchscreen names and the corrections from sanctuary masterlist
touchscreennames <- read.csv("touchscreennames wild 080922.csv")
library(DataCombine)
participation_datawild<-as.data.frame(participation_datawild)
participation_datawild$Individual<-as.factor(participation_datawild$Individual)
participation_datawild$Individual<-as.character(participation_datawild$Individual)
participation_datawild <- FindReplace(data=participation_datawild, Var="Individual", replaceData=touchscreennames, 
                                      from="Var1", to="correctname", exact=TRUE, vector=FALSE)
detach("package:DataCombine", unload=TRUE)
participation_datawild$Individual <- as.factor(participation_datawild$Individual)
levels(participation_datawild$Individual)

# now ocmbine wild and sanctuary datasets
names(participation_data) <- names(participation_datawild)
data<-rbind(participation_data, participation_datawild)

str(data)

data$Date <- as.Date(data$Date, format="YYYY-mm-dd")

data$Group <- as.factor(data$Group)   
levels(data$Group)

data$Year <- format(as.Date(data$Date, format="YYYY-mm-dd"),"%Y")

# Check correct Group and Age for all individuals

individualgroup <- table(data$Individual, data$Group)
individualgroup <- as.data.frame(individualgroup)
individualgroup <- subset(individualgroup, individualgroup$Freq>0)
write.csv(individualgroup, "ID Groups.csv")

individualsex <- table(data$Individual, data$Sex)
individualsex <- as.data.frame(individualsex)
individualsex <- subset(individualsex, individualsex$Freq>0)
write.csv(individualsex, "ID Sex.csv")

# export full dataset to manually correct errors found above
write.csv(data, "full dataset 080922.csv")

# import clean dataset
data <- read_excel("full dataset 080922 clean.xlsx")

# proportion of group
# take group composition from NAS
# cross reference - if date of touchscreen is between Start and End dates of group comp
# count individual as potential participant

# split touchscreen data into years
data2018 <- subset(data, Date<"2019-01-01")
data2019 <- subset(data, Date<"2020-01-01" & Date>"2018-12-31")
data2020 <- subset(data, Date<"2021-01-01"&Date>"2019-12-31")
data2021 <- subset(data, Date>="2021-01-01")

data2018 %>%
  group_by(Group) %>%
  summarise(days = n_distinct(Date))
# Group   days

# 1 Boeta     56
# 2 Liffie    49
# 3 Poena     50

data2019 %>%
  group_by(Group) %>%
  summarise(days = n_distinct(Date))
#  Group   days
#
# 1 Boeta     48
# 2 Cowen      4
# 3 KB         6
# 4 Liffie    50
# 5 LT        10
# 6 NH        36
# 7 Poena     48

data2020 %>%
  group_by(Group) %>%
  summarise(days = n_distinct(Date))
#   Group  days
# 1 AK        6
# 2 Boeta     8
# 3 Cowen     5
# 4 KB       15
# 5 LT        3
# 6 NH        9
# 7 Poena     4

data2021 %>%
  group_by(Group) %>%
  summarise(days = n_distinct(Date))
#   Group  days
# 1 AK       56
# 2 BD       55
# 3 Boeta    49
# 4 Cowen    46
# 5 KB       33
# 6 NH       66
# 7 Poena    42

# data2018 - Boeta, Liffie, Poena
# data2019 - Boeta, Cowen, KB, Liffie, LT, NH, Poena
# data2020 - AK, Boeta, Cowen, KB, LT, NH, Poena
# data2021 - AK, BD, Boeta, Cowen, KB, NH, Poena

##### AK DATA #####
# split by group
data2020AK <- subset(data2020, Group=="AK")

# import group comp (group composition files can be found on OSF, labelled by Group and Year e.g. AK2020)
AK2020 <- read_excel("group comp/AK2020.xlsx")

AK2020$StartAK <- as.Date(AK2020$StartAK, format="YYYY-mm-dd")
AK2020$EndAK <- as.Date(AK2020$EndAK, format="YYYY-mm-dd")

# create new dataframe listing all the test dates in the group
dates <- unique(data2020AK$Date)
testdaysAK2020 <- as.data.frame(dates)

AKlist2020 <- list() 
for (i in 1:nrow(testdaysAK2020)) {
  k <- subset(AK2020, AK2020$StartAK<=testdaysAK2020$dates[i] & 
                AK2020$EndAK>=testdaysAK2020$dates[i])
  l <- rep(testdaysAK2020$dates[i], times=nrow(k))
  m <- cbind(k,l)
  AKlist2020[[i]] <- m
}

# create dataframe from list
AKgroup2020<-do.call(rbind.data.frame, AKlist2020)

# merge the two datasets - row for each monkey present, then adding task performance info for those that participated
AK2020wholegroup <- left_join(AKgroup2020, data2020AK, by = c("Individidual" = "Individual", "l" = "Date"))

# split by group
data2021AK <- subset(data2021, Group=="AK")

# import group comp
AK2021 <- read_excel("group comp/AK2021.xlsx")

AK2021$StartAK <- as.Date(AK2021$StartAK, format="YYYY-mm-dd")
AK2021$EndAK <- as.Date(AK2021$EndAK, format="YYYY-mm-dd")

# create new dataframe listing all the test dates in the group
dates <- unique(data2021AK$Date)
testdaysAK2021 <- as.data.frame(dates)

nrow(testdaysAK2021)

# for each test date, make a dataframe listing all the monkeys present in the group on that day
AKlist2021 <- list() 
i <- 1
for (i in 1:nrow(testdaysAK2021)) {
  k <- subset(AK2021, AK2021$StartAK<=testdaysAK2021$dates[i] & 
                AK2021$EndAK>=testdaysAK2021$dates[i])
  l <- rep(testdaysAK2021$dates[i], times=nrow(k))
  m <- cbind(k,l)
  AKlist2021[[i]] <- m
}

# create dataframe from list
AKgroup2021<-do.call(rbind.data.frame, AKlist2021)

# merge the two datasets - row for each monkey present, then adding task performance info for those that participated
AK2021wholegroup <- left_join(AKgroup2021, data2021AK, by = c("Individidual" = "Individual", "l" = "Date"))

##### BD DATA #####
# split by group
data2021BD <- subset(data2021, Group=="BD")

# import group comp
BD2021 <- read_excel("group comp/BD2021.xlsx")

BD2021$StartBD <- as.Date(BD2021$StartBD, format="YYYY-mm-dd")
BD2021$EndBD <- as.Date(BD2021$EndBD, format="YYYY-mm-dd")

# create new dataframe listing all the test dates in the group
dates <- unique(data2021BD$Date)
testdaysBD2021 <- as.data.frame(dates)

# for each test date, make a dataframe listing all the monkeys present in the group on that day
BDlist2021 <- list() 
i <- 1
for (i in 1:nrow(testdaysBD2021)) {
  k <- subset(BD2021, BD2021$StartBD<=testdaysBD2021$dates[i] & 
                BD2021$EndBD>=testdaysBD2021$dates[i])
  l <- rep(testdaysBD2021$dates[i], times=nrow(k))
  m <- cbind(k,l)
  BDlist2021[[i]] <- m
}

# create dataframe from list
BDgroup2021<-do.call(rbind.data.frame, BDlist2021)

# merge the two datasets - row for each monkey present, then adding task performance info for those that participated
BD2021wholegroup <- left_join(BDgroup2021, data2021BD, by = c("Individidual" = "Individual", "l" = "Date"))

##### KB DATA #####
# split by group
data2019KB <- subset(data2019, Group=="KB")

# import group comp
KB2019 <- read_excel("group comp/KB2019.xlsx")

KB2019$StartKB <- as.Date(KB2019$StartKB, format="YYYY-mm-dd")
KB2019$EndKB <- as.Date(KB2019$EndKB, format="YYYY-mm-dd")

# create new dataframe listing all the test dates in the group
dates <- unique(data2019KB$Date)
testdaysKB2019 <- as.data.frame(dates)

# for each test date, make a dataframe listing all the monkeys present in the group on that day
KBlist2019 <- list() 
i <- 1
for (i in 1:nrow(testdaysKB2019)) {
  k <- subset(KB2019, KB2019$StartKB<=testdaysKB2019$dates[i] & 
                KB2019$EndKB>=testdaysKB2019$dates[i])
  l <- rep(testdaysKB2019$dates[i], times=nrow(k))
  m <- cbind(k,l)
  KBlist2019[[i]] <- m
}

# create dataframe from list
KBgroup2019<-do.call(rbind.data.frame, KBlist2019)

# merge the two datasets - row for each monkey present, then adding task performance info for those that participated
KB2019wholegroup <- left_join(KBgroup2019, data2019KB, by = c("Individidual" = "Individual", "l" = "Date"))

## 2020 ##
# split by group
data2020KB <- subset(data2020, Group=="KB")

# import group comp
KB2020 <- read_excel("group comp/KB2020.xlsx")

KB2020$StartKB <- as.Date(KB2020$StartKB, format="YYYY-mm-dd")
KB2020$EndKB <- as.Date(KB2020$EndKB, format="YYYY-mm-dd")

# create new dataframe listing all the test dates in the group
dates <- unique(data2020KB$Date)
testdaysKB2020 <- as.data.frame(dates)

# for each test date, make a dataframe listing all the monkeys present in the group on that day
KBlist2020 <- list() 
i <- 1
for (i in 1:nrow(testdaysKB2020)) {
  k <- subset(KB2020, KB2020$StartKB<=testdaysKB2020$dates[i] & 
                KB2020$EndKB>=testdaysKB2020$dates[i])
  l <- rep(testdaysKB2020$dates[i], times=nrow(k))
  m <- cbind(k,l)
  KBlist2020[[i]] <- m
}

# create dataframe from list
KBgroup2020<-do.call(rbind.data.frame, KBlist2020)

# merge the two datasets - row for each monkey present, then adding task performance info for those that participated
KB2020wholegroup <- left_join(KBgroup2020, data2020KB, by = c("Individidual" = "Individual", "l" = "Date"))

## 2021 ##
# split by group
data2021KB <- subset(data2021, Group=="KB")

# import group comp
KB2021 <- read_excel("group comp/KB2021.xlsx")

KB2021$StartKB <- as.Date(KB2021$StartKB, format="YYYY-mm-dd")
KB2021$EndKB <- as.Date(KB2021$EndKB, format="YYYY-mm-dd")

# create new dataframe listing all the test dates in the group
dates <- unique(data2021KB$Date)
testdaysKB2021 <- as.data.frame(dates)

# for each test date, make a dataframe listing all the monkeys present in the group on that day
KBlist2021 <- list() 
i <- 1
for (i in 1:nrow(testdaysKB2021)) {
  k <- subset(KB2021, KB2021$StartKB<=testdaysKB2021$dates[i] & 
                KB2021$EndKB>=testdaysKB2021$dates[i])
  l <- rep(testdaysKB2021$dates[i], times=nrow(k))
  m <- cbind(k,l)
  KBlist2021[[i]] <- m
}

# create dataframe from list
KBgroup2021<-do.call(rbind.data.frame, KBlist2021)

# merge the two datasets - row for each monkey present, then adding task performance info for those that participated
KB2021wholegroup <- left_join(KBgroup2021, data2021KB, by = c("Individidual" = "Individual", "l" = "Date"))

##### NH DATA #####
## 2019 data ##
data2019NH <- subset(data2019, Group=="NH")

# import group comp
NH2019 <- read_excel("group comp/NH2019.xlsx")

NH2019$StartNH <- as.Date(NH2019$StartNH, format="YYYY-mm-dd")
NH2019$EndNH <- as.Date(NH2019$EndNH, format="YYYY-mm-dd")

# create new dataframe listing all the test dates in the group
dates <- unique(data2019NH$Date)
testdaysNH2019 <- as.data.frame(dates)

# for each test date, make a dataframe listing all the monkeys present in the group on that day
NHlist2019 <- list() 
i <- 1
for (i in 1:nrow(testdaysNH2019)) {
  k <- subset(NH2019, NH2019$StartNH<=testdaysNH2019$dates[i] & 
                NH2019$EndNH>=testdaysNH2019$dates[i])
  l <- rep(testdaysNH2019$dates[i], times=nrow(k))
  m <- cbind(k,l)
  NHlist2019[[i]] <- m
}

# create dataframe from list
NHgroup2019<-do.call(rbind.data.frame, NHlist2019)

# merge the two datasets - row for each monkey present, then adding task performance info for those that participated
NH2019wholegroup <- left_join(NHgroup2019, data2019NH, by = c("Individidual" = "Individual", "l" = "Date"))


## 2020 data ##
# split by group
data2020NH <- subset(data2020, Group=="NH")

# import group comp
NH2020 <- read_excel("group comp/NH2020.xlsx")

NH2020$StartNH <- as.Date(NH2020$StartNH, format="YYYY-mm-dd")
NH2020$EndNH <- as.Date(NH2020$EndNH, format="YYYY-mm-dd")

# create new dataframe listing all the test dates in the group
dates <- unique(data2020NH$Date)
testdaysNH2020 <- as.data.frame(dates)

# for each test date, make a dataframe listing all the monkeys present in the group on that day
NHlist2020 <- list() 
i <- 1
for (i in 1:nrow(testdaysNH2020)) {
  k <- subset(NH2020, NH2020$StartNH<=testdaysNH2020$dates[i] & 
                NH2020$EndNH>=testdaysNH2020$dates[i])
  l <- rep(testdaysNH2020$dates[i], times=nrow(k))
  m <- cbind(k,l)
  NHlist2020[[i]] <- m
}

# create dataframe from list
NHgroup2020<-do.call(rbind.data.frame, NHlist2020)

# merge the two datasets - row for each monkey present, then adding task performance info for those that participated
NH2020wholegroup <- left_join(NHgroup2020, data2020NH, by = c("Individidual" = "Individual", "l" = "Date"))

## 2021 DATA ##
# split by group
data2021NH <- subset(data2021, Group=="NH")

# import group comp
NH2021 <- read_excel("group comp/NH2021.xlsx")

NH2021$StartNH <- as.Date(NH2021$StartNH, format="YYYY-mm-dd")
NH2021$EndNH <- as.Date(NH2021$EndNH, format="YYYY-mm-dd")

# create new dataframe listing all the test dates in the group
dates <- unique(data2021NH$Date)
testdaysNH2021 <- as.data.frame(dates)

# for each test date, make a dataframe listing all the monkeys present in the group on that day
NHlist2021 <- list() 
i <- 1
for (i in 1:nrow(testdaysNH2021)) {
  k <- subset(NH2021, NH2021$StartNH<=testdaysNH2021$dates[i] & 
                NH2021$EndNH>=testdaysNH2021$dates[i])
  l <- rep(testdaysNH2021$dates[i], times=nrow(k))
  m <- cbind(k,l)
  NHlist2021[[i]] <- m
}

# create dataframe from list
NHgroup2021<-do.call(rbind.data.frame, NHlist2021)

# merge the two datasets - row for each monkey present, then adding task performance info for those that participated
NH2021wholegroup <- left_join(NHgroup2021, data2021NH, by = c("Individidual" = "Individual", "l" = "Date"))

##### SANCTUARY GROUPS #####

# split by group
dataBoeta <- subset(data, Group=="Boeta")

# import group comp
Sanctuary <- read_excel("sanctuary group comp dob sex 080922 pilot+.xlsx")

SanctuaryBoeta <- subset(Sanctuary, Group=="Boeta")

SanctuaryBoeta$Start <- as.Date(SanctuaryBoeta$Start, format="YYYY-mm-dd")
SanctuaryBoeta$End <- as.Date(SanctuaryBoeta$End, format="YYYY-mm-dd")

# create new dataframe listing all the test dates in the group
dates <- unique(dataBoeta$Date)
testdaysBoeta <- as.data.frame(dates)

# for each test date, make a dataframe listing all the monkeys present in the group on that day
Boetalist <- list() 
i <- 1
for (i in 1:nrow(testdaysBoeta)) {
  k <- subset(SanctuaryBoeta, SanctuaryBoeta$Start<=testdaysBoeta$dates[i] & 
                SanctuaryBoeta$End>=testdaysBoeta$dates[i])
  l <- rep(testdaysBoeta$dates[i], times=nrow(k))
  m <- cbind(k,l)
  Boetalist[[i]] <- m
}

# create dataframe from list
Boetagroup<-do.call(rbind.data.frame, Boetalist)

# merge the two datasets - row for each monkey present, then adding task performance info for those that participated
Boetawholegroup <- left_join(Boetagroup, dataBoeta, by = c("Individual" = "Individual", "l" = "Date"))


# split by group
dataCowen <- subset(data, Group=="Cowen")

# import group comp
Sanctuary <- read_excel("sanctuary group comp dob sex 080922 pilot+.xlsx")

SanctuaryCowen <- subset(Sanctuary, Group=="Cowen")

SanctuaryCowen$Start <- as.Date(SanctuaryCowen$Start, format="YYYY-mm-dd")
SanctuaryCowen$End <- as.Date(SanctuaryCowen$End, format="YYYY-mm-dd")

# create new dataframe listing all the test dates in the group
dates <- unique(dataCowen$Date)
testdaysCowen <- as.data.frame(dates)

# for each test date, make a dataframe listing all the monkeys present in the group on that day
Cowenlist <- list() 
i <- 1
for (i in 1:nrow(testdaysCowen)) {
  k <- subset(SanctuaryCowen, SanctuaryCowen$Start<=testdaysCowen$dates[i] & 
                SanctuaryCowen$End>=testdaysCowen$dates[i])
  l <- rep(testdaysCowen$dates[i], times=nrow(k))
  m <- cbind(k,l)
  Cowenlist[[i]] <- m
}

# create dataframe from list
Cowengroup<-do.call(rbind.data.frame, Cowenlist)

# merge the two datasets - row for each monkey present, then adding task performance info for those that participated
Cowenwholegroup <- left_join(Cowengroup, dataCowen, by = c("Individual" = "Individual", "l" = "Date"))

# split by group
dataPoena <- subset(data, Group=="Poena")

# import group comp
Sanctuary <- read_excel("sanctuary group comp dob sex 080922 pilot+.xlsx")

SanctuaryPoena <- subset(Sanctuary, Group=="Poena")

SanctuaryPoena$Start <- as.Date(SanctuaryPoena$Start, format="YYYY-mm-dd")
SanctuaryPoena$End <- as.Date(SanctuaryPoena$End, format="YYYY-mm-dd")

# create new dataframe listing all the test dates in the group
dates <- unique(dataPoena$Date)
testdaysPoena <- as.data.frame(dates)

# for each test date, make a dataframe listing all the monkeys present in the group on that day
Poenalist <- list() 
i <- 1
for (i in 1:nrow(testdaysPoena)) {
  k <- subset(SanctuaryPoena, SanctuaryPoena$Start<=testdaysPoena$dates[i] & 
                SanctuaryPoena$End>=testdaysPoena$dates[i])
  l <- rep(testdaysPoena$dates[i], times=nrow(k))
  m <- cbind(k,l)
  Poenalist[[i]] <- m
}

# create dataframe from list
Poenagroup<-do.call(rbind.data.frame, Poenalist)

# merge the two datasets - row for each monkey present, then adding task performance info for those that participated
Poenawholegroup <- left_join(Poenagroup, dataPoena, by = c("Individual" = "Individual", "l" = "Date"))

# split by group
dataLiffie <- subset(data, Group=="Liffie")

# import group comp
Sanctuary <- read_excel("sanctuary group comp dob sex 080922 pilot+.xlsx")

SanctuaryLiffie <- subset(Sanctuary, Group=="Liffie")

SanctuaryLiffie$Start <- as.Date(SanctuaryLiffie$Start, format="YYYY-mm-dd")
SanctuaryLiffie$End <- as.Date(SanctuaryLiffie$End, format="YYYY-mm-dd")

# create new dataframe listing all the test dates in the group
dates <- unique(dataLiffie$Date)
testdaysLiffie <- as.data.frame(dates)

# for each test date, make a dataframe listing all the monkeys present in the group on that day
Liffielist <- list() 
i <- 1
for (i in 1:nrow(testdaysLiffie)) {
  k <- subset(SanctuaryLiffie, SanctuaryLiffie$Start<=testdaysLiffie$dates[i] & 
                SanctuaryLiffie$End>=testdaysLiffie$dates[i])
  l <- rep(testdaysLiffie$dates[i], times=nrow(k))
  m <- cbind(k,l)
  Liffielist[[i]] <- m
}

# create dataframe from list
Liffiegroup<-do.call(rbind.data.frame, Liffielist)

# merge the two datasets - row for each monkey present, then adding task performance info for those that participated
Liffiewholegroup <- left_join(Liffiegroup, dataLiffie, by = c("Individual" = "Individual", "l" = "Date"))


##### Bind wild and sanctuary datasets together #####
NH2019wholegroup$Group <- "NH"
NH2020wholegroup$Group <- "NH"
NH2021wholegroup$Group <- "NH"
KB2020wholegroup$Group <- "KB"
KB2021wholegroup$Group<- "KB"
KB2019wholegroup$Group<- "KB"
BD2021wholegroup$Group<- "BD"
AK2020wholegroup$Group<- "AK"
AK2021wholegroup$Group<- "AK"

bigdatawild <- rbind(NH2019wholegroup[ -c(6:9) ], NH2020wholegroup[ -c(6:9) ], NH2021wholegroup[ -c(6:9) ],
                     KB2020wholegroup[ -c(6:9) ], KB2021wholegroup[ -c(6:9) ], KB2019wholegroup[ -c(6:9) ], BD2021wholegroup[ -c(6:9) ], 
                     AK2020wholegroup[ -c(6:9) ], AK2021wholegroup[ -c(6:9) ])

Boetawholegroup$Group <- "Boeta"
Cowenwholegroup$Group <- "Cowen"
Liffiewholegroup$Group <- "Liffie"
Poenawholegroup$Group <- "Poena"

bigdatasanctuary <- rbind(Boetawholegroup, Cowenwholegroup, Poenawholegroup, Liffiewholegroup)

# assign 0 total attempts to all individuals present in group who did not participate in that day of testing
bigdatawild$Total[is.na(bigdatawild$Total)] <- 0
bigdatasanctuary$Total[is.na(bigdatasanctuary$Total)] <- 0

bigdatawild$binary <- ifelse(bigdatawild$Total>=1, 1, 0)
bigdatasanctuary$binary <- ifelse(bigdatasanctuary$Total>=1, 1, 0)

# NAs in pilot data (text NAs, not true NAs)
# replace with 2 as this was the only task run at that time
bigdatasanctuary$Task <- dplyr::recode(bigdatasanctuary$Task, "NA"="2")

bigdatawild$Individual <- bigdatawild$Individidual
bigdatawild$context <- "Wild"
bigdatawild$l <- as.Date(bigdatawild$l, format="YYYY-mm-dd")

bigdatasanctuary$Group.y <- as.factor(bigdatasanctuary$Group.y)
bigdatasanctuary$Group.y<-droplevels(bigdatasanctuary$Group.y)
bigdatasanctuary$context<-"Sanctuary"
bigdatasanctuary$l <- as.Date(bigdatasanctuary$l, format="YYYY-mm-dd")

##### Set Age and Sex classes #####

bigdatawild$Sex.x<-as.factor(bigdatawild$Sex.x)
bigdatawild$Sex.x<-dplyr::recode(bigdatawild$Sex.x, "f"="F")

bigdatasanctuary$Sex.x <- as.factor(bigdatasanctuary$Sex.x)
bigdatasanctuary$Sex.x<-dplyr::recode(bigdatasanctuary$Sex.x, "f"="F", "m"="M")

## assign age class to all individuals based on date of testing 
bigdatawild$DOB <- as.Date(bigdatawild$DOB, format="YYYY-mm-dd")
bigdatawild$FirstRecorded <- as.Date(bigdatawild$FirstRecorded, format="YYYY-mm-dd")
bigdatawild$TestDate <- as.Date(bigdatawild$l, format="YYYY-mm-dd")

bigdatawild$FirstRecorded[bigdatawild$Individidual=="Nugget"] <- as.Date("2018-10-26")
bigdatawild$DOB[bigdatawild$Individidual=="Nugget"] <- as.Date("2018-10-26")

bigdatasanctuary$DOB<-as.Date(bigdatasanctuary$DOB, format="YYYY-mm-dd")
bigdatasanctuary$TestDate<-as.Date(bigdatasanctuary$l, format="YYYY-mm-dd")

## Set age at testing WILD
# Either test date - DOB
# Or test date - First Recorded
# Not calculated for males resident outside birth group (immigrant adults)
# Not calculated for individuals without DOB First Recorded before 2014 (presumed adult, 5 yrs plus, at time of testing)
i <- 1
for (i in 1:nrow(bigdatawild)) { 
  if (is.na(bigdatawild$BirthGp[i])) {
    bigdatawild$AgeAtTesting[i] <- NA
  } else
    if(bigdatawild$BirthGp[i]!=bigdatawild$Group[i]) {
      bigdatawild$AgeAtTesting[i] <- NA
    } else
      if (bigdatawild$FirstRecorded[i] < as.Date("2014-01-01")) 
      { bigdatawild$AgeAtTesting[i] <- NA
      } else
        if (is.na(bigdatawild$DOB[i]))
        { bigdatawild$AgeAtTesting[i] <- bigdatawild$TestDate[i]-bigdatawild$FirstRecorded[i] 
        } else {
          bigdatawild$AgeAtTesting[i] <- bigdatawild$TestDate[i]-bigdatawild$DOB[i]
        }
}   


bigdatawild$AgeAtTesting <- as.numeric(bigdatawild$AgeAtTesting)
bigdatawild$AgeAtTesting <- (bigdatawild$AgeAtTesting)/365

# import life history in order to make list of mothers
library(readxl)
IVP_Life_history_2feb22 <- read_excel("IVP Life history_2feb22.xlsx", 
                                      col_types = c("numeric", "text", "text", 
                                                    "text", "text", "text", "date", "date", 
                                                    "text", "text", "text", "text", "text", 
                                                    "text", "text", "text", "text", "text", 
                                                    "text", "text", "text", "text", "text", 
                                                    "text", "text", "text", "text", "text"))
lifehist <- IVP_Life_history_2feb22
# mothers <- unique(lifehist$Mother)
lifehist$FirstRecorded[lifehist$Individual=="Baby Daisy 2019"] <- "2019-10-01"
lifehist$Mother <- dplyr::recode(lifehist$Mother, "Ndaw" = "Ndawonya", "Ndawonye"="Ndawonya", "Oort"="Oortjies")

# list of all mothers organised by oldest offspring (i.e. earliest FirstRecorded date where an individual is listed as the mother)
lifehist <- as.data.frame(lifehist)
firstbirths <- lifehist %>% 
  group_by(Mother) %>% 
  filter(FirstRecorded == min(FirstRecorded)) %>% 
  ungroup()
firstbirths$Mother <- as.factor(firstbirths$Mother)

# so this produces a list of all mothers, with the FirstRecorded date as their apparent date of first birth (or should be pretty close at least)
# certainly for the time period we're concerned with (2019-2021) these should be accurate enough

# add column indicating if individual is a mother (this will help with age classification)
bigdatawild$mother <- ifelse(bigdatawild$Individidual %in% firstbirths$Mother, "yes", "no")

bigdatawild$firstbirth <- firstbirths$FirstRecorded[match(bigdatawild$Individidual, firstbirths$Mother)]

# Set age classes based on date of participation.
# Baby - F and M up to 0.25 year old
# Juvenile - Males aged 0.25 - 5 prior to dispersal (still in natal group) - Females aged 0.25 - 5 with no offspring / whose oldest offspring were born after given test date
# Adult - Males after disperal (not in group of birth) - Females with oldest offspring who were first recorded prior to test date (i.e. first birth was before given test date)

for (i in 1:nrow(bigdatawild)) {
  if ((!is.na(bigdatawild$Sex.x[i])) & bigdatawild$Sex.x[i]=="M" & (is.na(bigdatawild$BirthGp[i]) | bigdatawild$BirthGp[i]!=bigdatawild$Group[i])) {
    bigdatawild$AgeClasstest[i] <- "Adult"
  } else
    if ((!is.na(bigdatawild$Sex.x[i]))& bigdatawild$Sex.x[i]=="M" & (!is.na(bigdatawild$BirthGp[i])) & bigdatawild$BirthGp[i]==bigdatawild$Group[i] & bigdatawild$AgeAtTesting[i] >= 0.25 & bigdatawild$AgeAtTesting[i] <= 5.0) {
      bigdatawild$AgeClasstest[i] <- "Juvenile"
    } 
  else
    if ((!is.na(bigdatawild$Sex.x[i])) & bigdatawild$Sex.x[i]=="M" &  (!is.na(bigdatawild$BirthGp[i])) & bigdatawild$BirthGp[i]==bigdatawild$Group[i] & bigdatawild$AgeAtTesting[i] >= 5.0) {
      bigdatawild$AgeClasstest[i] <- "Adult" 
    } else
      if ((!is.na(bigdatawild$Sex.x[i])) & bigdatawild$Sex.x[i]=="F" & bigdatawild$mother[i]=="yes" & !is.na(bigdatawild$firstbirth[i]) & (bigdatawild$TestDate[i] > bigdatawild$firstbirth[i])) {
        bigdatawild$AgeClasstest[i] <- "Adult" 
      } 
  else
    if ((!is.na(bigdatawild$Sex.x[i])) & bigdatawild$Sex.x[i]=="F" & bigdatawild$mother[i]=="no" & bigdatawild$AgeAtTesting[i] >= 0.25 & bigdatawild$AgeAtTesting[i] <= 5.0) {
      bigdatawild$AgeClasstest[i] <- "Juvenile" 
    }  else
      if ((!is.na(bigdatawild$Sex.x[i])) & bigdatawild$Sex.x[i]=="F" & bigdatawild$mother[i]=="no" & bigdatawild$AgeAtTesting[i] >= 5.0) {
        bigdatawild$AgeClasstest[i] <- "Adult" 
      }else
        if ((!is.na(bigdatawild$Sex.x[i])) & bigdatawild$Sex.x[i]=="F" & bigdatawild$mother[i]=="yes" &  !is.na(bigdatawild$firstbirth[i]) & (bigdatawild$TestDate[i] < bigdatawild$firstbirth[i])) {
          bigdatawild$AgeClasstest[i] <- "Juvenile" 
        } else
          if (bigdatawild$AgeAtTesting[i] <= 0.25){
            bigdatawild$AgeClasstest[i] <- "Baby"} 
  else if (is.na(bigdatawild$Sex.x[i])) {
    bigdatawild$AgeClasstest[i] <- "Juvenile"
  }
}

# drop all infants less than 3 months, make sure to keep NAs in AgeAtTesting as these are our immigrant males / females with no DOB
bigdatawild <- bigdatawild[which(bigdatawild$AgeAtTesting>=0.25 | is.na(bigdatawild$AgeAtTesting)),]


## set age at testing SANCTUARY
bigdatasanctuary$AgeAtTesting <- (bigdatasanctuary$TestDate - bigdatasanctuary$DOB)/365
bigdatasanctuary$AgeAtTesting <- as.numeric(bigdatasanctuary$AgeAtTesting)

bigdatasanctuary <- bigdatasanctuary[which(bigdatasanctuary$AgeAtTesting>=0.25),]

# add column indicating if individual gave birth during testing (this will help with age classification)
birthsanc <- read_excel("motherssanctuary.xlsx", col_types=c("text", "date"))
bigdatasanctuary$Individual<-as.factor(bigdatasanctuary$Individual)
birthsanc$Mother <- as.factor(birthsanc$Mother)
bigdatasanctuary$mother <- ifelse(bigdatasanctuary$Individual %in% birthsanc$Mother, "yes", "no")

bigdatasanctuary$birth <- birthsanc$Date[match(bigdatasanctuary$Individual, birthsanc$Mother)]
library(anytime)
bigdatasanctuary$birth <- as.Date(bigdatasanctuary$birth, format="YY-mm-dd")
bigdatasanctuary$testbeforebirth <- ifelse(!is.na(bigdatasanctuary$birth) & bigdatasanctuary$birth > bigdatasanctuary$TestDate, "yes", "no")

bigdatasanctuary$AgeAtTesting <- as.numeric(bigdatasanctuary$AgeAtTesting)

# set age classes sanctuary
for (i in 1:nrow(bigdatasanctuary)){
  if (bigdatasanctuary$AgeAtTesting[i] < 0.25) {
    bigdatasanctuary$AgeClasstest[i] <- "Baby"
  } else
    if (bigdatasanctuary$Sex.x[i] =="M" & bigdatasanctuary$AgeAtTesting[i] > 0.25 & bigdatasanctuary$AgeAtTesting[i] <= 5.0) {
      bigdatasanctuary$AgeClasstest[i] <- "Juvenile"
    } else
      if (bigdatasanctuary$Sex.x[i] =="M" & bigdatasanctuary$AgeAtTesting[i] > 5.0) {
        bigdatasanctuary$AgeClasstest[i] <- "Adult"
      } else 
        if (bigdatasanctuary$Sex.x[i]=="F" & bigdatasanctuary$AgeAtTesting[i] > 0.25 & bigdatasanctuary$AgeAtTesting[i] <= 5.0 & bigdatasanctuary$mother[i]=="no") {
          bigdatasanctuary$AgeClasstest[i] <- "Juvenile"
        } else
          if (bigdatasanctuary$Sex.x[i]=="F" & bigdatasanctuary$AgeAtTesting[i] > 0.25 & bigdatasanctuary$AgeAtTesting[i] <= 5.0 & bigdatasanctuary$mother[i]=="yes" & bigdatasanctuary$testbeforebirth[i]=="yes") {
            bigdatasanctuary$AgeClasstest[i] <- "Juvenile"
          } else
            if (bigdatasanctuary$Sex.x[i]=="F" & bigdatasanctuary$AgeAtTesting[i] > 0.25 & bigdatasanctuary$AgeAtTesting[i] <= 5.0 & bigdatasanctuary$mother[i]=="yes" & bigdatasanctuary$testbeforebirth[i]=="no") {
              bigdatasanctuary$AgeClasstest[i] <- "Adult" 
            } else
              if (bigdatasanctuary$Sex.x[i]=="F" & bigdatasanctuary$AgeAtTesting[i] > 5.0) {
                bigdatasanctuary$AgeClasstest[i] <- "Adult"
              } 
}

bigdatasanctuary$AgeClasstest <- as.factor(bigdatasanctuary$AgeClasstest)
levels(bigdatasanctuary$AgeClasstest)

bigdatasanctuary$context <- "Sanctuary"

bigdatasanctuary$Individual <- dplyr::recode(bigdatasanctuary$Individual, "Poppie"="PoppieS")

bigdata <- rbind(bigdatasanctuary[,c("Group", "Individual", "DOB", "Sex.x", "TestDate", "binary", "AgeAtTesting", "AgeClasstest", "context", "Task", "Total", "Correct")], 
                 bigdatawild[,c("Group", "Individual", "DOB", "Sex.x", "TestDate", "binary", "AgeAtTesting", "AgeClasstest", "context", "Task", "Total", "Correct")])

View(bigdata)

# only 2 individuals less than 6 months participated ever
# Leo 0.36 years (4 months)
# Griffin 0.4 years (4.8 months)
# one session each before 6 months old


bigdataparticipants<-bigdata[(bigdata$binary==1),]
n_distinct(bigdataparticipants$Individual[bigdataparticipants$AgeAtTesting<=1.0 & bigdataparticipants$AgeAtTesting>=0.5])
# 19 individuals participate between 0.5 and 1 year old


##### Time of Presentation #####
timewild <- read_excel("ExperimentTimeWild06042022.xlsx", 
                       col_types = c("numeric", "date", "text", 
                                     "date", "date", "text", "text"))

timesanctuary <- read_excel("ExperimentTimeSanctuary08092022 pilot+.xlsx", 
                            col_types = c("numeric", "date", "text", 
                                          "date", "date", "text", "text"))

timesanctuary <- timesanctuary %>% 
  distinct(Date, Group, .keep_all = TRUE)

str(timewild)
str(timesanctuary)

times <- rbind(timewild, timesanctuary)

times$Date <- as.Date(times$Date, format="YYYY-mm-dd")
times$interrupted <- times$`Stop for external facotrs`

times$duration <- times$`Time end` - times$`Time start`


# times <- times %>% 
#  distinct(Date, Group, .keep_all = TRUE)

# some end and start times are entered incorrectly (swapped) so correct for that
times$duration <- ifelse(times$duration < 0, times$duration*-1, times$duration)

bigdata <- left_join(bigdata, times, by=c("Group", "TestDate"="Date"))

#bigdata <- bigdata[,-c(13,14,15,17)]

bigdata$duration.z <- as.vector(scale(bigdata$duration))

#### Add Group Size column ####

groupsizes <-bigdata %>%
  group_by(Group, TestDate) %>%
  summarise(GroupSize = n_distinct(Individual))

bigdata <- left_join(bigdata, groupsizes, by=c("Group", "TestDate"))

bigdata$Groupsize.z <- as.vector(scale(bigdata$GroupSize))

#### Add Season column ####
# seasons from Canteloup et al 2019 - May-October = winter

bigdata$month <- format(bigdata$TestDate,"%m")
bigdata$month <- as.numeric(bigdata$month)

for (i in 1:nrow(bigdata)) {
  if (bigdata$month[i] >= 5 & bigdata$month[i] <= 10){
    bigdata$season[i] <- "winter"
  } else
    bigdata$season[i] <- "summer"
}

#### Export full clean dataset ####
# this is the dataset used for analysis - it can be found on OSF #
write.csv(bigdata, "complete dataset 08092022.csv")

#### Analysis 1: Did context impact binary participation in the task? ####

# The key variable of interest is sanctuary vs wild. Age and sex will not be included due to very different sample sizes between the two contexts.
# Also including session duration as control variable (as this was significantly different between contexts)
# also if we want to test for an effect of Season makes sense to put it in this big model as well

# to fit full and null models to the same dataset we need to exclude any rows with NA for one of our predictor variables
# the glmm will do this anyway, but the null model will not, so we have to do it manually now

names(which(colSums(is.na(bigdata))>0))

# two juveniles of unknown sex (Baby Numb 2021 and Piesang)
# exclude from dataset - these individuals did not participate

bigdata <- bigdata[!is.na(bigdata$Sex.x),]

bigdata$AgeClasstest <- as.factor(bigdata$AgeClasstest)
bigdata$context <- as.factor(bigdata$context)

# sample size by context
bigdata %>%
  group_by(context)%>%
  summarise(count=n_distinct(Individual))
#   context   count
# * <fct>     <int>
# 1 Sanctuary    62
# 2 Wild        179

# exclude sessions which ended early (recorded as 'stop for external factors')
# individuals were prevented from participating in these sessions so may not be reflective of how many individuals would have participated given a chance

table(times$interrupted)
# NA  No yes Yes 
# 1 675   4  87 

bigdatanostops <- bigdata[bigdata$interrupted=="No",]
# calculate descriptive stats for remaining sessions

range(bigdatanostops$duration[bigdatanostops$context=="Sanctuary"])
# 5.9 193.4333
mean(bigdatanostops$duration[bigdatanostops$context=="Sanctuary"])
# 31.9477
range(bigdatanostops$duration[bigdatanostops$context=="Wild"])
# 6.816667 187.433333
mean(bigdatanostops$duration[bigdatanostops$context=="Wild"])
# 61.6661


bigdatanostops$duration.z <- as.vector(scale(bigdatanostops$duration))

bigdatanostops %>%
  group_by(context)%>%
  summarise(count=n_distinct(Individual))
# context   count
# <fct>     <int>
# 1 Wild        178
# 2 Sanctuary    62

# set reference level to Wild
bigdatanostops$context <- relevel(bigdatanostops$context, ref="Wild")

binmodexc <- glmer(data=bigdatanostops, binary~context+
                     duration.z+
                     (1|Individual)+
                     (1|Group),
                   family="binomial",
                   control = glmerControl(optimizer="bobyqa"))
summary(binmodexc)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: binary ~ context + duration.z + (1 | Individual) + (1 | Group)
# Data: bigdatanostops
# Control: glmerControl(optimizer = "bobyqa")
# 
# AIC      BIC   logLik deviance df.resid 
# 7466.1   7503.7  -3728.1   7456.1    13568 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -5.4081 -0.2697 -0.0816 -0.0481 12.9403 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# Individual (Intercept) 9.1445   3.0240  
# Group      (Intercept) 0.2464   0.4964  
# Number of obs: 13573, groups:  Individual, 240; Group, 8
# 
# Fixed effects:
#                    Estimate Std. Error z value             Pr(>|z|)    
#   (Intercept)      -4.04647    0.39333 -10.288 < 0.0000000000000002 ***
#   contextSanctuary  1.95572    0.62237   3.142              0.00168 ** 
#   duration.z        0.48172    0.03641  13.230 < 0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) cntxtS
# cntxtSnctry -0.577       
# duration.z  -0.075  0.069

vif(binmodexc)
# context duration.z 
# 1.004766  1.004766

nullmodexc <- glmer(data=bigdatanostops, binary~
                      (1|Individual)+
                      (1|Group),
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa"))
anova(binmodexc, nullmodexc, test="LRT")
# Data: bigdatanostops
# Models:
# nullmodexc: binary ~ (1 | Individual) + (1 | Group)
# binmodexc: binary ~ context + duration.z + (1 | Individual) + (1 | Group)
# npar    AIC    BIC  logLik deviance  Chisq Df            Pr(>Chisq)    
# nullmodexc    3 7649.7 7672.2 -3821.8   7643.7                                    
# binmodexc     5 7466.1 7503.7 -3728.1   7456.1 187.54  2 < 0.00000000000000022 ***

confint(binmodexc, method="Wald")
#                       2.5 %     97.5 %
# .sig01                   NA         NA
# .sig02                   NA         NA
# (Intercept)      -4.8173807 -3.2755600
# contextSanctuary  0.7358826  3.1755478
# duration.z        0.4103595  0.5530857


# plot model predictions and observed data
bigdataindprops <- bigdatanostops %>%
  group_by(Individual, context) %>%
  summarise(bin=sum(binary), n=n())%>%
  mutate(prop=bin/n) 

contextplotpred <- ggemmeans(binmodexc, terms=c("context"))
contextplotpred$context<- contextplotpred$x

ggplot()+ geom_boxplot(data=bigdataindprops, aes(y=prop, x=context, fill=context, color=context), outlier.color = NA, alpha=0.1)+
  geom_point(data=bigdataindprops, aes(y=prop, x=context, fill=context, colour=context),alpha=0.4, position=position_jitter(width=0.4, height=0.0), size=2)+
  geom_point(data=contextplotpred, aes(x=context, y=predicted), size=4)+
  geom_errorbar(data=contextplotpred, aes(x=context, y=predicted), ymin=contextplotpred$conf.low, ymax=contextplotpred$conf.high, width=0.2, lwd=1.2)+
  ylim(0,1)+
  theme(text=element_text(size=20))+
  xlab("Context")+
  ylab("Individual likelihood\n of participation")+
  labs(colour="Context")+
  scale_color_manual(values=c("#0A9396", "#EE9B00"))+
  scale_fill_manual(values=c("#0A9396", "#EE9B00"))+
  guides(fill="none")+
  theme(panel.background = element_rect(fill="white"), panel.border=element_rect(colour="black", fill=NA))

ggsave("Likelihood by context with observed.png", width=7, height = 5, dpi=600)

#### Analysis 2: Did age or sex impact likelihood of participation? Wild only ####
wilddatanostops <- bigdatanostops[bigdatanostops$context=="Wild",]

wilddatanostops %>%
  summarise(count=n_distinct(Individual))

wilddatanostops$AgeClasstest <- relevel(wilddatanostops$AgeClasstest, ref="Juvenile")

partmodwild <- glmer(data=wilddatanostops, binary~AgeClasstest*Sex.x+Group+duration.z+
                       +(1|Individual), family="binomial",  control = glmerControl(optimizer="bobyqa"))
summary(partmodwild)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: binary ~ AgeClasstest * Sex.x + Group + duration.z + +(1 | Individual)
# Data: wilddatanostops
# Control: glmerControl(optimizer = "bobyqa")
# 
# AIC      BIC   logLik deviance df.resid 
# 4690.1   4754.0  -2336.1   4672.1     8856 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -5.4306 -0.2570 -0.0700 -0.0378 12.7899 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# Individual (Intercept) 11.97    3.46    
# Number of obs: 8865, groups:  Individual, 178
# 
# Fixed effects:
#                          Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)               -3.3110     0.6967  -4.753         0.0000020079 ***
# AgeClasstestAdult          0.7689     0.3120   2.465              0.01371 *  
# Sex.xM                     1.5767     0.6573   2.399              0.01645 *  
# GroupBD                   -2.3693     0.7506  -3.157              0.00160 ** 
# GroupKB                   -2.2713     0.7720  -2.942              0.00326 ** 
# GroupNH                   -1.1440     0.6535  -1.751              0.08003 .  
# duration.z                 0.4776     0.0379  12.602 < 0.0000000000000002 ***
# AgeClasstestAdult:Sex.xM  -3.4194     0.6149  -5.561         0.0000000269 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) AgClsA Sex.xM GropBD GropKB GropNH drtn.z
# AgClsststAd -0.276                                          
# Sex.xM      -0.534  0.277                                   
# GroupBD     -0.589 -0.006  0.019                            
# GroupKB     -0.534  0.014  0.013  0.546                     
# GroupNH     -0.606  0.011  0.017  0.575  0.782              
# duration.z  -0.052 -0.011  0.014  0.005  0.014 -0.001       
# AgClssA:S.M  0.170 -0.527 -0.426  0.078  0.082  0.058 -0.031

vif(partmodwild)
# #                       GVIF Df GVIF^(1/(2*Df))
# AgeClasstest       1.397848  1        1.182306
# Sex.x              1.231695  1        1.109818
# Group              1.018042  3        1.002985
# duration.z         1.002677  1        1.001337
# AgeClasstest:Sex.x 1.596318  1        1.263455

partmodwildnull <- glmer(data=wilddatanostops, binary~(1|Individual), family="binomial", control = glmerControl(optimizer="bobyqa"))
anova(partmodwild, partmodwildnull, test="LRT")
# Data: wilddatanostops
# Models:
# partmodwildnull: binary ~ (1 | Individual)
# partmodwild: binary ~ AgeClasstest * Sex.x + Group + duration.z + +(1 | Individual)
#                  npar    AIC    BIC  logLik deviance Chisq Df            Pr(>Chisq)    
# partmodwildnull    2 4905.2 4919.4 -2450.6   4901.2                                   
# partmodwild        9 4690.1 4754.0 -2336.1   4672.1 229.1  7 < 0.00000000000000022 ***
#   ---

confint(partmodwild, method="Wald")
# #                             2.5 %     97.5 %
# .sig01                           NA         NA
# (Intercept)              -4.6763755 -1.9455295
# AgeClasstestAdult         0.1574418  1.3802967
# Sex.xM                    0.2883662  2.8649356
# GroupBD                  -3.8404127 -0.8981363
# GroupKB                  -3.7844326 -0.7580832
# GroupNH                  -2.4248656  0.1368769
# duration.z                0.4033006  0.5518486
# AgeClasstestAdult:Sex.xM -4.6246770 -2.2141480

Anova(partmodwild)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: binary
#                       Chisq Df            Pr(>Chisq)    
# AgeClasstest         0.2978  1              0.585245    
# Sex.x                0.0011  1              0.973015    
# Group               14.2298  3              0.002608 ** 
# duration.z         158.8199  1 < 0.00000000000000022 ***
# AgeClasstest:Sex.x  30.9196  1         0.00000002689 ***

summary(glht(partmodwild, linfct=mcp(Group="Tukey")))
#              Estimate Std. Error z value Pr(>|z|)   
# BD - AK == 0 -2.36927    0.75059  -3.157  0.00817 **
# KB - AK == 0 -2.27126    0.77204  -2.942  0.01642 * 
# NH - AK == 0 -1.14399    0.65352  -1.751  0.28827   
# KB - BD == 0  0.09802    0.72595   0.135  0.99908   
# NH - BD == 0  1.22528    0.65331   1.876  0.23030   
# NH - KB == 0  1.12726    0.48387   2.330  0.08759 . 

sexagewildpred <- ggemmeans(partmodwild, terms=c("AgeClasstest", "Sex.x"))

# add observed data - each individual's proportion of sessions participated in, with age and sex
wildbinprops <- wilddatanostops %>%
  group_by(Individual, Sex.x, AgeClasstest) %>%
  summarise(bin=sum(binary), n=n())%>%
  mutate(prop=bin/n) 

sexagewildpred$Sex.x <- sexagewildpred$group
sexagewildpred$AgeClasstest <- sexagewildpred$x

ggplot()+   geom_point(data=wildbinprops, aes(y=prop, x=Sex.x, fill=Sex.x, colour=Sex.x), alpha=0.6, position=position_jitter(width=0.15, height=0))+
  geom_boxplot(data=wildbinprops, aes(y=prop, x=Sex.x, fill=Sex.x, colour=Sex.x), alpha=0.2, outlier.colour = NA)+
  geom_errorbar(data=sexagewildpred, aes(x=Sex.x, y=predicted, ymin=sexagewildpred$conf.low, ymax=sexagewildpred$conf.high), size=1,width=0.3, lwd=1.5)+
  geom_point(data=sexagewildpred, aes(x=Sex.x, y=predicted), size=2)+
  facet_grid(~AgeClasstest)+
  scale_fill_manual(values=c("#7b2cbf", "#FF7900"))+
  scale_colour_manual(values=c("#7b2cbf", "#FF7900"))+
  ylim(0, 1)+
  ylab("Likelihood of participation")+
  xlab("Sex")+
  scale_x_discrete(labels=c("Female", "Male"))+
  theme(panel.margin.x=unit(0.2, "lines"), panel.margin.y=unit(1, "lines"))+
  theme(panel.background = element_rect(fill="white", colour="black"))+
  theme(legend.position = "none")+
  theme(text=element_text(size=20))

ggsave("observed and predicted participation by sexage wild.png", dpi=600)

#### Analysis 3: Survival model for participation frequency, wild only ####
bigdata$Task <- as.factor(bigdata$Task)
levels(bigdata$Task)

freqpart <- bigdata[bigdata$Total>=1,]
freqpart <- freqpart[freqpart$Task=="2"|freqpart$Task=="3",]

nrow(freqpart[freqpart$Total > 30,])
# 457

freqpart$totaltruncated <- ifelse(freqpart$Total >= 30, 30, freqpart$Total)
freqpart$totaltruncatedproportion <- freqpart$totaltruncated/30

# set censoring record - if indiviudal reached 30 without stopping, no "event" took place
# cens = 1 indicates an individual ceasing to participate by choice
freqpartwild$cens <- ifelse(freqpartwild$Total >= 30, 0, 1)

freqpartwild <- freqpart[freqpart$context=="Wild",]

freqpartwild <- freqpartwild[freqpartwild$Sex.x!="unk",]

freqpartwild$Sex.x<-as.factor(freqpartwild$Sex.x)
freqpartwild$Sex.x<-droplevels(freqpartwild$Sex.x)

freqpartwild$AgeClasstest<-as.factor(freqpartwild$AgeClasstest)
freqpartwild$Group<-as.factor(freqpartwild$Group)
freqpartwild$Individual<-as.factor(freqpartwild$Individual)

freqpartwild$AgeClasstest <- relevel(freqpartwild$AgeClasstest, ref="Juvenile")

survmodel2 <- coxme(Surv(totaltruncated, cens) ~ Sex.x * AgeClasstest + Group+ (1|Individual), data=freqpartwild)
summary(survmodel2)
# Cox mixed-effects model fit by maximum likelihood
# Data: freqpartwild
# events, n = 822, 1096
# Iterations= 11 61 
# NULL Integrated    Fitted
# Log-likelihood -5312.063  -5202.857 -5109.022
# 
# Chisq    df p    AIC    BIC
# Integrated loglik 218.41  7.00 0 204.41 171.43
# Penalized loglik 406.08 58.67 0 288.75  12.32
# 
# Model:  Surv(totaltruncated, cens) ~ Sex.x * AgeClasstest + Group + (1 |      Individual) 
# Fixed coefficients
#                                  coef exp(coef)  se(coef)     z     p
# Sex.xM                   -0.519462790 0.5948400 0.2776332 -1.87 0.061
# AgeClasstestAdult        -0.224265306 0.7991031 0.2819995 -0.80 0.430
# GroupBD                   0.637431778 1.8916165 0.2912600  2.19 0.029
# GroupKB                   0.544600323 1.7239192 0.3652942  1.49 0.140
# GroupNH                  -0.004614805 0.9953958 0.2881281 -0.02 0.990
# Sex.xM:AgeClasstestAdult  0.694515081 2.0027377 0.4050386  1.71 0.086
# 
# Random effects
# Group      Variable  Std Dev   Variance 
# Individual Intercept 0.7601798 0.5778733

Anova(survmodel2)
# Analysis of Deviance Table (Type II tests)
# 
# Response: Surv(totaltruncated, cens)
# Df  Chisq Pr(>Chisq)  
# Sex.x               1 0.9539    0.32873  
# AgeClasstest        1 0.2431    0.62201  
# Group               3 8.7268    0.03315 *
# Sex.x:AgeClasstest  1 2.9402    0.08640 .

cox.zph(survmodel2)
# #                  chisq df     p
# Sex.x               3.01  1 0.083
# AgeClasstest        0.27  1 0.604
# Group               6.75  3 0.080
# Sex.x:AgeClasstest  1.68  1 0.195
# GLOBAL              8.57  6 0.199

# full null comparison
survmodel2null <- coxme(Surv(totaltruncated, cens) ~  (1|Individual), data=freqpartwild)

anova(survmodel2, survmodel2null, test="LRT")
# Analysis of Deviance Table
# Cox model: response is  Surv(totaltruncated, cens)
# Model 1: ~Sex.x * AgeClasstest + Group + (1 | Individual)
# Model 2: ~(1 | Individual)
# loglik  Chisq Df P(>|Chi|)  
# 1 -5202.9                      
# 2 -5210.3 14.832  6    0.0216 *

coxme.glht<-glht(survmodel2,linfct=mcp(Group="Tukey"))
summary(coxme.glht)
# 	 Simultaneous Tests for General Linear Hypotheses

# Multiple Comparisons of Means: Tukey Contrasts
# 
# 
# Fit: coxme(formula = Surv(totaltruncated, cens) ~ Sex.x * AgeClasstest + 
#              Group + (1 | Individual), data = freqpartwild)
# 
# Linear Hypotheses:
#   Estimate Std. Error z value Pr(>|z|)  
# BD - AK == 0  0.637432   0.291260   2.189   0.1242  
# KB - AK == 0  0.544600   0.365294   1.491   0.4388  
# NH - AK == 0 -0.004615   0.288128  -0.016   1.0000  
# KB - BD == 0 -0.092831   0.337432  -0.275   0.9926  
# NH - BD == 0 -0.642047   0.257387  -2.494   0.0597 .
# NH - KB == 0 -0.549215   0.330083  -1.664   0.3390  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Adjusted p values reported -- single-step method)

# hazard ratio NH vs BD
exp(-0.642047)
# [1]  0.5262142

# plot observed data
individualrate <- freqpartwild %>%
  group_by(Individual, Group, Sex.x, AgeClasstest) %>%
  summarise(mean=mean(totaltruncated), sd=sd(totaltruncated))

individualrate$sd[is.na(individualrate$sd)] <- 0
individualrate$Group <- dplyr::recode(individualrate$Group, "AK"="Ankhase", "BD"="Baie Dankie", "KB"="Kubu", "NH"="Noha")

ggplot(data=individualrate, aes(x=Sex.x, y=mean, fill=Sex.x))+geom_boxplot(data=freqpartwild, aes(y=totaltruncated, x=Sex.x, fill=Sex.x, color=Sex.x), alpha=0.2, outlier.colour = NA)+
  facet_grid(~AgeClasstest)+
  geom_point(aes(colour=Sex.x, shape=Group),position=position_dodge(width=0.7), size=4)+
  theme(panel.background=element_rect(fill="white"))+
  scale_fill_manual(values=c("#7b2cbf", "#FF7900"), label=c("Female", "Male"))+
  xlab("Sex")+
  scale_shape_manual(values=c(15,16,17,18))+
  scale_x_discrete(labels=c("Female", "Male"))+
  ylab("Number of attempts")+
  scale_color_manual(values=c("#7b2cbf","#FF7900"), name="Sex", labels=c("Female","Male"))+
  guides(fill="none")+
  theme(text=element_text(size=20))+
  theme(panel.margin.x=unit(0.2, "lines"), panel.margin.y=unit(1, "lines"))+
  theme(panel.background = element_rect(fill="white", colour="black"))

ggsave("observed number of attempts wild.png", dpi=600, width=10, height=7)

#### Analysis 4: Effect of previous success - wild only, no stops ####
# using previous 'correct' count instead of proportion (i.e. number of rewards gained previous time they played)
# first make sure data is clean - some rows where Correct and Total are swapped
for (i in 1:nrow(bigdatanostops)) {
  if (!is.na(bigdatanostops$Correct[i]) & !is.na(bigdatanostops$Total[i]) & bigdatanostops$Correct[i] > bigdatanostops$Total[i]) {
    (bigdatanostops$TotalFixed[i] <- bigdatanostops$Correct[i]) & (bigdatanostops$CorrectFixed[i] <- bigdatanostops$Total[i])
  } else { (bigdatanostops$TotalFixed[i] <- bigdatanostops$Total[i]) & (bigdatanostops$CorrectFixed[i] <- bigdatanostops$Correct[i])
  }
}

bigdatanostops$propsuccess <- bigdatanostops$CorrectFixed / bigdatanostops$TotalFixed

bigdataprevsuc <- bigdatanostops %>%  
  arrange(TestDate) %>%
  group_by(Individual) %>%
  dplyr::mutate(correctprevious = lag(CorrectFixed, n = 1, default = NA))

bigdataprevsuc <- bigdataprevsuc %>%
  group_by(Individual) %>%
  arrange(TestDate, .by_group=TRUE)

bigdataprevsuc <- bigdataprevsuc %>%
  dplyr::group_by(Individual) %>%
  fill(correctprevious, .direction = "down") %>%
  dplyr::ungroup()

# add in Gap since previous session
bigdataprevsucgap <- left_join(bigdataprevsuc, times)

# if it's been more than 30 days since the last test session, reset 'previous success' to NA
bigdataprevsucgap <- bigdataprevsucgap %>%
  mutate(correctprevious = replace(correctprevious, which(Gap>30), NA))

# limit to wild only
bigdataprevsucgapwild <- bigdataprevsucgap[bigdataprevsucgap$context=="Wild",]

bigdataprevsucgapwild$correctprevious.z <- as.vector(scale(bigdataprevsucgapwild$correctprevious))
#bigdataprevsucgap$season <- as.factor(bigdataprevsucgap$season)
bigdataprevsucgapwild <- bigdataprevsucgapwild[!is.na(bigdataprevsucgapwild$correctprevious.z),]
#bigdataprevsucgap <- within(bigdataprevsucgap, context <- relevel(context, ref = "Wild"))
bigdataprevsucgapwild <- within(bigdataprevsucgapwild, AgeClasstest <- relevel(AgeClasstest, ref = "Juvenile"))
bigdataprevsucgapwild <- within(bigdataprevsucgapwild, Sex.x <- relevel(Sex.x, ref = "F"))
#bigdataprevsucgap <- within(bigdataprevsucgap, season <- relevel(season, ref = "winter"))

prevsucgapmodelcountwild <- glmer(data=bigdataprevsucgapwild, binary~
                                    AgeClasstest*Sex.x+
                                    correctprevious.z+
                                    Group+
                                    duration.z+
                                    (1|Individual),
                                  family="binomial",
                                  control = glmerControl(optimizer="bobyqa"))
summary(prevsucgapmodelcountwild)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: binary ~ AgeClasstest * Sex.x + correctprevious.z + Group + duration.z +      (1 | Individual)
# Data: bigdataprevsucgapwild
# Control: glmerControl(optimizer = "bobyqa")
# 
# AIC      BIC   logLik deviance df.resid 
# 3913.0   3976.0  -1946.5   3893.0     4034 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -5.6452 -0.5250 -0.2630  0.5308  6.8498 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# Individual (Intercept) 2.506    1.583   
# Number of obs: 4044, groups:  Individual, 94
# 
# Fixed effects:
#                          Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)              -0.74477    0.47058  -1.583             0.113498    
# AgeClasstestAdult         0.06113    0.28691   0.213             0.831277    
# Sex.xM                    0.66673    0.41881   1.592             0.111399    
# correctprevious.z         0.36713    0.05111   7.184    0.000000000000679 ***
# GroupBD                  -0.76992    0.48164  -1.599             0.109921    
# GroupKB                  -0.49863    0.54987  -0.907             0.364500    
# GroupNH                  -0.30808    0.47283  -0.652             0.514688    
# duration.z                0.40346    0.04023  10.028 < 0.0000000000000002 ***
# AgeClasstestAdult:Sex.xM -1.82612    0.49132  -3.717             0.000202 ***

confint(prevsucgapmodelcountwild, method="Wald")
#                               2.5 %     97.5 %
# .sig01                           NA         NA
# (Intercept)              -1.6670836  0.1775492
# AgeClasstestAdult        -0.5012021  0.6234625
# Sex.xM                   -0.1541349  1.4875859
# correctprevious.z         0.2669637  0.4672982
# GroupBD                  -1.7139104  0.1740705
# GroupKB                  -1.5763512  0.5790869
# GroupNH                  -1.2348066  0.6186554
# duration.z                0.3246021  0.4823112
# AgeClasstestAdult:Sex.xM -2.7890997 -0.8631426

rewardpredwild <- ggemmeans(prevsucgapmodelcountwild, terms=c("correctprevious.z [all]", "Sex.x", "AgeClasstest"))
rewardpredwild$sex <- rewardpredwild$group
rewardpredwild$age <- rewardpredwild$facet
rewardpredwild$reward <- rewardpredwild$x
#  unscale the values for duration 
rewardpredwild$unscalereward <- rewardpredwild$reward*sd(bigdataprevsucgapwild$correctprevious)+mean(bigdataprevsucgapwild$correctprevious)
# 
# # set NA for predictions outside the observed data range
range(bigdataprevsucgapwild$correctprevious[bigdataprevsucgapwild$Sex.x=="F"])
# # 0 36
range(bigdataprevsucgapwild$correctprevious[bigdataprevsucgapwild$Sex.x=="M"])
# # 0 43

rewardpredwild$predicted[rewardpredwild$sex=="F" & rewardpredwild$unscalereward > 36] <- NA
rewardpredwild$conf.low[rewardpredwild$sex=="F" & rewardpredwild$unscalereward > 36] <- NA
rewardpredwild$conf.high[rewardpredwild$sex=="F" & rewardpredwild$unscalereward > 36] <- NA

ggplot(data=rewardpredwild, aes(x=unscalereward, y=predicted, fill=sex, color=sex, group=sex))+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.3, color=NA)+
  facet_grid(~age)+
  geom_line(size=1.2)+
  ylab("Individual Likelihood of Participating")+
  xlab("Previous Reward Count")+
  labs(fill="Sex", color="Sex")+
  scale_fill_manual(values=c("#7B2CBF","#FF7900"), labels=c("Female", "Male"))+
  scale_color_manual(values=c("#7B2CBF", "#FF7900"), labels=c("Female", "Male"))+
  theme(text=element_text(size=20))+
  theme(panel.background = element_rect(fill="white"))+
  ylim(0.00, 1.00)+
  theme(panel.margin.x=unit(0.2, "lines"), panel.margin.y=unit(1, "lines"))+
  theme(panel.background = element_rect(fill="white", colour="black"))

ggsave("previous success predictions wild.png", dpi = 600)

vif(prevsucgapmodelcountwild)
# # #                     GVIF Df GVIF^(1/(2*Df))
# AgeClasstest       1.726432  1        1.313938
# Sex.x              1.465743  1        1.210679
# correctprevious.z  1.026102  1        1.012967
# Group              1.073749  3        1.011930
# duration.z         1.004482  1        1.002239
# AgeClasstest:Sex.x 2.041165  1        1.428693

prevsucgapmodelcountnull <- glmer(data=bigdataprevsucgapwild, binary~
                                    (1|Individual),
                                  family="binomial",
                                  control = glmerControl(optimizer="bobyqa"))

anova(prevsucgapmodelcountwild, prevsucgapmodelcountnull)
# Data: bigdataprevsucgapwild
# Models:
#   prevsucgapmodelcountnull: binary ~ (1 | Individual)
# prevsucgapmodelcountwild: binary ~ AgeClasstest * Sex.x + correctprevious.z + Group + duration.z + (1 | Individual)
# npar    AIC    BIC  logLik deviance  Chisq Df            Pr(>Chisq)    
# prevsucgapmodelcountnull    2 4102.3 4114.9 -2049.2   4098.3                                    
# prevsucgapmodelcountwild   10 3913.0 3976.0 -1946.5   3893.0 205.35  8 < 0.00000000000000022 ***
#   ---

# sample size by age sex
bigdataprevsucgapwild %>%
  group_by(Sex.x, AgeClasstest) %>%
  summarise(n=n_distinct(Individual))
#   Sex.x AgeClasstest     n
# 1 F     Juvenile        20
# 2 F     Adult           29
# 3 M     Juvenile        30
# 4 M     Adult           23

#### Analysis 5: Task 3 performance, wild vs. captive ####
# data file "Performance data task 2 and 3 .csv" is available on OSF

Task2 <- read.csv("Performance data task 2 and 3.csv")
Task2 <- Task2[Task2$Group!="LT",]
Task2$Individual <- dplyr::recode(Task2$Individual, "Ginqika"="Ginqinka", "Liffie"="Lieffie")
Task2$Sex <- bigdata$Sex.x[match(Task2$Individual, bigdata$Individual)]
Task2$Context <- bigdata$context[match(Task2$Individual, bigdata$Individual)]

Task3 <- Task2[Task2$task.3=="yes",]

Task3$Context <- as.factor(Task3$Context)
Task3$Context <- relevel(Task3$Context, ref="Wild")

# exclude individuals who started task 3 in the pilot sessions
Task3 <- Task3[Task3$Individual!="Boeta" & Task3$Individual!="Lieffie" & Task3$Individual!="Fielis" ,]

# check whether individuals passed from juvenile to adult during task 3
table(bigdata$Individual[bigdata$Task=="3"], bigdata$AgeClasstest[bigdata$Task=="3"])

# Fanjan appears as both juvenile and adult - check age and dates
View(bigdata[bigdata$Individual=="Fanjan",])

# Fanjan turns 5 with two sessions of Task 3 to go before he passes
# class him as adult as he was adult when he passed and was an old 4 year old during his Task 3 training

Task3$AgeClass <- bigdata$AgeClasstest[match(Task3$Individual, bigdata$Individual)]

# Set Fanjan as adult
Task3$AgeClass[Task3$Individual=="Fanjan"] <- "Adult"

names(Task3)[names(Task3) == "Number of touches to pass task 3"] <- "attempts3"

table(Task3$Context, Task3$Sex, Task3$AgeClass)
# , ,  = Adult
# 
# 
# F M unk
# Wild      4 1   0
# Sanctuary 2 1   0
# 
# , ,  = Juvenile
# 
# 
# F M unk
# Wild      3 7   0
# Sanctuary 4 1   0

table(Task3$Context)
# Sanctuary      Wild 
#         8        15 

# very limited sample in terms of Sex and Age Class comparisons, so limit analysis to Context only

# first try with standard poisson mixed model and check diagnostics

poismod <- glmer(data=Task3, attempts3~Context+(1|Group), family="poisson")
summary(poismod)
# 
simtask3 <- simulateResiduals(poismod)
plot(simtask3)

# overdispersed, outliers, deviates from normal
# swap to neg binomial

task3modnb <- glmmTMB(data=Task3, attempts3~Context+(1|Group), family=nbinom1())
summary(task3modnb)
# Family: nbinom1  ( log )
# Formula:          attempts3 ~ Context + (1 | Group)
# Data: Task3
# 
# AIC      BIC   logLik deviance df.resid 
# 298.1    302.6   -145.0    290.1       19 
# 
# Random effects:
#   
#   Conditional model:
#   Groups Name        Variance       Std.Dev.  
# Group  (Intercept) 0.000000001007 0.00003174
# Number of obs: 23, groups:  Group, 7
# 
# Dispersion parameter for nbinom1 family ():  136 
# 
# Conditional model:
#                     Estimate Std. Error z value            Pr(>|z|)    
#   (Intercept)        5.2505     0.2065  25.428 <0.0000000000000002 ***
#   ContextSanctuary   0.3183     0.2901   1.097               0.273  

simtask3nb <- simulateResiduals(task3modnb)
plot(simtask3nb)

# all diagnostics in DHARMa look good

testDispersion(simtask3nb)
# DHARMa nonparametric dispersion test via sd of residuals fitted vs. simulated
# 
# data:  simulationOutput
# dispersion = 1.4306, p-value = 0.336
# alternative hypothesis: two.sided

task3modnbnull <- glmmTMB(data=Task3, attempts3~(1|Group), family=nbinom1())
anova(task3modnb, task3modnbnull, test="LRT")
# Data: Task3
# Models:
#   task3modnbnull: attempts3 ~ (1 | Group), zi=~0, disp=~1
# task3modnb: attempts3 ~ Context + (1 | Group), zi=~0, disp=~1
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# task3modnbnull  3 297.22 300.63 -145.61   291.22                        
# task3modnb      4 298.10 302.64 -145.05   290.10 1.123      1     0.2893

confint(task3modnb, method="Wald")
# #                              2.5 %    97.5 %      Estimate
# (Intercept)                4.8458380 5.6552519 5.25054494598
# ContextSanctuary          -0.2502594 0.8867627 0.31825166143
# Std.Dev.(Intercept)|Group  0.0000000       Inf 0.00003173501

# plot observed data
Task3$Group <- dplyr::recode(Task3$Group, "AK"="Ankhase", "BD"="Baie Dankie", "KB"="Kubu", "NH"="Noha")
Task3$Group <- factor(Task3$Group, levels = c("Ankhase", "Baie Dankie", "Kubu", "Noha", "Boeta", "Cowen", "Liffie", "Poena"))
Task3$Group2 <- paste(Task3$Context, Task3$Group, sep=" - ")

task3plot <- as.data.frame(cbind(Task3$Individual, Task3$Group, Task3$attempts3, Task3$Context))
task3plot$V4<-as.factor(task3plot$V4)
task3plot$V3<-as.numeric(task3plot$V3)
task3plot$Context <- task3plot$V4
task3plot$Context <- ifelse(task3plot$Context==1, "Wild", "Sanctuary")
task3plot$Group <- task3plot$V2
task3plot$Group2 <- paste(task3plot$Context, task3plot$Group, sep=" - ")

ggplot(data=Task3, aes(x=Context, y=attempts3))+geom_boxplot(aes(x=Context, y=attempts3, colour=Context, fill=Context),outlier.shape = NA,  alpha=0.2, inherit.aes=FALSE)+
  scale_colour_manual(values=c("#0A9396", "#EE9B00"))+
  scale_fill_manual(values=c("#0A9396", "#EE9B00"))+
  guides(colour="none")+
  ggnewscale::new_scale_colour()+
  geom_point(data=Task3, aes(colour=Group, shape=Group, x=Context, y=attempts3),position=position_dodge(width=0.7), size=4)+
  scale_color_manual(values=c("#0A9396","#0A9396","#0A9396","#EE9B00","#EE9B00","#EE9B00","#EE9B00"))+
  scale_shape_manual(values=c(15,16,17,18,15,16,17))+
  ylab("Number of Attempts Taken\n to Pass Task 3")+
  theme(panel.background = element_rect(fill="white"))+
  theme(text=element_text(size=20))+
  ylim(0,1000)+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  guides(fill="none")

ggsave("attempts to Task 3 boxplot.png", width=7, height=5, dpi=600)


#### Supplemental Analysis 6: Effect of gap before session ####

# create variable in times dataset which gives gap from previous testdate
times <- times %>%
  group_by(Group) %>%
  arrange(Date) %>%
  dplyr::mutate(PreviousSession = dplyr::lag(Date))

# gap in days from previous test session
times$Gap <- times$Date - times$PreviousSession  

bigdatagap <- left_join(bigdata, times, by=c("Group", "TestDate"="Date"))  
#bigdatagap <- bigdatagap[,-c(13,14,18,19,20,25,26,27,28,29)]

View(bigdatagap)

range(bigdatagap$Gap)
# 426
range(bigdatagap[bigdatagap$context=="Wild",]$Gap)
# 0 310
range(bigdatagap[bigdatagap$context=="Sanctuary",]$Gap)
# 1 426

# z transform gap length to normalise
bigdatagap$Gap.z <- as.vector(scale(bigdatagap$Gap))

bigdatagap <- bigdatagap[!is.na(bigdatagap$Gap.z),]

bigdatagap$Individual <- as.factor(bigdatagap$Individual)
bigdatagap$context <- relevel(bigdatagap$context, ref="Wild")

gapmodel <- glmer(data=bigdatagap, binary~Gap.z+context+duration.z+
                    (1|Individual)+(1|Group),
                  family="binomial",
                  control = glmerControl(optimizer="bobyqa"))
summary(gapmodel)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: binary ~ Gap.z + context + duration.z + (1 | Individual) + (1 |      Group)
# Data: bigdatagap
# Control: glmerControl(optimizer = "bobyqa")
# 
# AIC      BIC   logLik deviance df.resid 
# 8209.7   8255.4  -4098.8   8197.7    15163 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -5.6851 -0.2586 -0.0811 -0.0449 13.9785 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# Individual (Intercept) 8.9633   2.9939  
# Group      (Intercept) 0.2006   0.4479  
# Number of obs: 15169, groups:  Individual, 241; Group, 8
# 
# Fixed effects:
#                  Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)      -4.11012    0.37206 -11.047 < 0.0000000000000002 ***
# Gap.z            -0.03150    0.02928  -1.076              0.28201    
# contextSanctuary  1.87758    0.59538   3.154              0.00161 ** 
# duration.z        0.55378    0.03448  16.060 < 0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) Gap.z  cntxtS
# Gap.z        0.002              
# cntxtSnctry -0.566 -0.004       
# duration.z  -0.083  0.013  0.069

vif(gapmodel)
# Gap.z    context duration.z 
# 1.000199   1.004854   1.005018 

gapmodelnull <-  glmer(data=bigdatagap, binary~
                         (1|Individual)+(1|Group),
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa"))

anova(gapmodel, gapmodelnull)
# Data: bigdatagap
# Models:
# gapmodelnull: binary ~ (1 | Individual) + (1 | Group)
# gapmodel: binary ~ Gap.z + context + duration.z + (1 | Individual) + (1 | Group)
#              npar    AIC    BIC  logLik deviance  Chisq Df            Pr(>Chisq)    
# gapmodelnull    3 8484.8 8507.7 -4239.4   8478.8                                    
# gapmodel        6 8209.7 8255.4 -4098.8   8197.7 281.17  3 < 0.00000000000000022 ***

confint(gapmodel, method="Wald")
# #                      2.5 %      97.5 %
# .sig01                    NA          NA
# .sig02                    NA          NA
# (Intercept)      -4.83933581 -3.38089907
# Gap.z            -0.08888116  0.02588619
# contextSanctuary  0.71064743  3.04451101
# duration.z        0.48619984  0.62136406

m <- ggemmeans(gapmodel, terms=c("Gap.z [all]", "context"))


# unscale the values for group size
m$unscaleg <- as.numeric(m$x*sd(bigdatagap$Gap)+mean(bigdatagap$Gap))

m$unscaleg[m$group=="Wild" & m$unscaleg > 310] <- NA
m$conf.low[m$group=="Wild" & m$unscaleg > 310] <- NA
m$conf.high[m$group=="Wild" & m$unscaleg > 310] <- NA

# set NA for predictions outside the observed data range
ggplot(data=m, aes(x=unscaleg, y=predicted, colour=group))+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=0.3, colour=NA)+
  geom_line(size=1.2)+
  ylab("Individual Likelihood of Participating")+
  xlab("Gap Between Test Sessions (Days)")+
  labs(fill="Context", color="Context")+
  theme(text=element_text(size=20))+
  scale_fill_manual(values=c("#0A9396", "#EE9B00"))+
  scale_color_manual(values=c("#0A9396", "#EE9B00"))+
  theme(panel.background = element_rect(fill="white"))+
  ylim(0.00, 1.00)

ggsave("gap in testing.png", dpi=600)

#### Supplemental Analysis 7: Predicting count of individuals participating in each session ####

bigdataparticipants <- bigdata[!is.na(bigdata$Task),]
bigdataparticipants <- bigdataparticipants[bigdataparticipants$context=="Wild",]

participationcounts <- bigdataparticipants %>%
  group_by(Group, TestDate) %>%
  summarise(n=n_distinct(Individual))

participationcounts <- left_join(participationcounts, times, by=c("Group", "TestDate"="Date"))

participationcounts$duration.z <- as.vector(scale(participationcounts$duration))

participationcounts$month <- format(participationcounts$TestDate,"%m")
participationcounts$month <- as.numeric(participationcounts$month)

for (i in 1:nrow(participationcounts)) {
  if (participationcounts$month[i] >= 5 & participationcounts$month[i] <= 10){
    participationcounts$season[i] <- "winter"
  } else
    participationcounts$season[i] <- "summer"
}

participationcounts <- left_join(participationcounts, groupsizes, by=c("Group", "TestDate"))

participationcounts$Groupsize.z <- as.vector(scale(participationcounts$GroupSize))

participationcounts$Group <- as.factor(participationcounts$Group)



countmodel <- glm(data=participationcounts, n~duration.z+
                    Group, family="poisson")
summary(countmodel)
# # Coefficients:
#              Estimate Std. Error z value            Pr(>|z|)    
# (Intercept)  1.634577   0.054901  29.773 <0.0000000000000002 ***
# duration.z   0.254427   0.024286  10.476 <0.0000000000000002 ***
# GroupBD      0.155829   0.079043   1.971              0.0487 *  
# GroupKB     -0.190498   0.085117  -2.238              0.0252 *  
# GroupNH     -0.006138   0.067487  -0.091              0.9275    

# 
countmodelnull <- glm(data=participationcounts, n~1, family="poisson")
# 
anova(countmodelnull, countmodel, test="Chisq")
# Model 1: n ~ 1
# Model 2: n ~ duration.z + Group
#        Resid. Df Resid. Dev Df Deviance              Pr(>Chi)    
# 1       282     372.40                                      
# 2       278     252.03  4   120.37 < 0.00000000000000022 ***

vif(countmodel)
# # #            GVIF Df GVIF^(1/(2*Df))
# duration.z 1.057053  1        1.028131
# Group      1.057053  3        1.009290
# 
testDispersion(countmodel)
# DHARMa nonparametric dispersion test via sd of residuals fitted vs. simulated
# 
# data:  simulationOutput
# dispersion = 0.88093, p-value = 0.16
# alternative hypothesis: two.sided

confint(countmodel, method="Wald")
#                     2.5 %      97.5 %
# (Intercept)  1.5251726855  1.74043349
# duration.z   0.2066205898  0.30182899
# GroupBD      0.0006261013  0.31060435
# GroupKB     -0.3583841362 -0.02454521
# GroupNH     -0.1377814970  0.12685478

Anova(countmodel)
# Analysis of Deviance Table (Type II tests)
# 
# Response: n
#             LR Chisq Df            Pr(>Chisq)    
# duration.z  104.659  1 < 0.00000000000000022 ***
# Group        16.154  3              0.001055 ** 

summary(glht(countmodel, linfct=mcp(Group="Tukey")))
# Linear Hypotheses:
#               Estimate Std. Error z value Pr(>|z|)    
# BD - AK == 0  0.155829   0.079043   1.971   0.1964    
# KB - AK == 0 -0.190498   0.085117  -2.238   0.1116    
# NH - AK == 0 -0.006138   0.067487  -0.091   0.9997    
# KB - BD == 0 -0.346326   0.086776  -3.991   <0.001 ***
# NH - BD == 0 -0.161966   0.070343  -2.303   0.0962 .  
# NH - KB == 0  0.184360   0.077747   2.371   0.0815 .  


countgrp<-ggemmeans(countmodel, terms=c("Group"))

countgrp <- countgrp %>%
  mutate(x = fct_relevel(x, 
                         "AK", "BD", "KB", 
                         "NH"))
p <- ggplot(data=countgrp, aes(x=x, y=predicted, color=x))+geom_point(size=2.3)+geom_errorbar(ymin=countgrp$conf.low, ymax=countgrp$conf.high, width=0.5, size=1)+
  scale_color_manual(name="Group", values=c("#001219", "#005F73", "#0A9396","#94D2BD"))+
  ylab("Number of Individuals Participating")+
  theme(panel.background = element_rect(fill="white"))+
  theme(text=element_text(size=20))+
  ylim(0,17)+
  xlab("Group")

p + geom_point(data=participationcounts, aes(x=Group, y=n, color=Group), position="jitter", alpha=0.2)

ggsave("counts participating group wild only.png", dpi=600)

#### Descriptive stats and data summary ####
# session durations
range(bigdatanostops[bigdatanostops$context=="Wild",]$duration)
#[1]   6.816667 187.433333
range(bigdatanostops[bigdatanostops$context=="Sanctuary",]$duration)
#[1]   5.9000 193.4333
mean(bigdatanostops[bigdatanostops$context=="Wild",]$duration)
#[1] 61.6661
mean(bigdatanostops[bigdatanostops$context=="Sanctuary",]$duration)
#[1] 31.9477


# total number of test sessions
bigdata %>%
  group_by(Group) %>%
  summarise(n=n_distinct(TestDate))

# total testing time per group
times[!is.na(times$duration),] %>%
  group_by(Group) %>%
  summarise(sum=sum(duration)/60)

# demographic info age sex class min and max
countsavailablegroup <- bigdatanostops %>%
  group_by(TestDate, Group, AgeClasstest, Sex.x) %>%
  summarise(available=n_distinct(Individual))

countsavailablegrouprange <- countsavailablegroup %>%
  group_by(Group, AgeClasstest, Sex.x) %>%
  summarise(rangeav=range(available))
write.table(countsavailablegrouprange, "groupcounts.txt")
View(countsavailablegrouprange)

# how many in each category were in the groups during the testing period?
countsavailable <- bigdata %>%
  group_by(context, AgeClasstest, Sex.x) %>%
  summarise(available=n_distinct(Individual))
# context   AgeClasstest Sex.x available
# <chr>     <fct>        <fct>     <int>
# 1 Sanctuary Adult        F             7
# 2 Sanctuary Adult        M             7
# 3 Sanctuary Juvenile     F            22
# 4 Sanctuary Juvenile     M            37
# 5 Wild      Adult        F            53
# 6 Wild      Adult        M            40
# 7 Wild      Juvenile     F            43
# 8 Wild      Juvenile     M            56

# including only those in the sample (i.e. excluding sessions with early stops)
countsavailable <- bigdatanostops %>%
  group_by(context, AgeClasstest, Sex.x) %>%
  summarise(available=n_distinct(Individual))
countsavailable
# context   AgeClasstest Sex.x available
# <fct>     <fct>        <fct>     <int>
# 1 Wild      Adult        F            53
# 2 Wild      Adult        M            40
# 3 Wild      Juvenile     F            43
# 4 Wild      Juvenile     M            55
# 5 Sanctuary Adult        F             7
# 6 Sanctuary Adult        M             7
# 7 Sanctuary Juvenile     F            22
# 8 Sanctuary Juvenile     M            37


# how many wild individuals appear as both adult and juvenile in the dataset?
wildadults <- bigdatanostops[bigdatanostops$AgeClasstest=="Adult",]
wildadults <- wildadults[wildadults$context=="Wild",]

wildjuveniles <- bigdatanostops[bigdatanostops$AgeClasstest=="Juvenile",]
wildjuveniles <- wildjuveniles[wildjuveniles$context=="Wild",]

wildadults$Individual <- droplevels(wildadults$Individual)
levels(wildadults$Individual)

wildjuveniles$Individual <- droplevels(wildjuveniles$Individual)
levels(wildjuveniles$Individual)

intersect(wildadults$Individual, wildjuveniles$Individual)
#  [1] "Umtata"    "Guatemala" "Rabat"     "Roslin"    "Rioja"     "Xiashan"   "Hondjie"   "Sari"      "Sirkus"   
# [10] "Oerwoud"   "Ndawonya"  "Goduka"    "Ngenakubo"


# how many of individuals in each category participated at some point?
countsparticipating <- bigdatanostops[bigdatanostops$Total >0 & (bigdatanostops$Task==2|bigdatanostops$Task==3),] %>% 
  group_by(context, AgeClasstest, Sex.x) %>%
  summarise(available=n_distinct(Individual))
countsparticipating
# context   AgeClasstest Sex.x available
# <fct>     <fct>        <fct>     <int>
# 1 Wild      Adult        F            23
# 2 Wild      Adult        M            19
# 3 Wild      Juvenile     F            20
# 4 Wild      Juvenile     M            26
# 5 Sanctuary Adult        F             3
# 6 Sanctuary Adult        M             4
# 7 Sanctuary Juvenile     F            13
# 8 Sanctuary Juvenile     M            28

# how many participating individuals participated as both adult and juvenile?
participants <- bigdatanostops[bigdatanostops$Total > 0,]
participants <- participants[participants$Task==2|participants$Task==3,]
wildadults <- participants[participants$AgeClasstest=="Adult",]
wildadults <- wildadults[wildadults$context=="Wild",]

wildjuveniles <- participants[participants$AgeClasstest=="Juvenile",]
wildjuveniles <- wildjuveniles[wildjuveniles$context=="Wild",]

wildadults$Individual <- droplevels(wildadults$Individual)
levels(wildadults$Individual)

wildjuveniles$Individual <- droplevels(wildjuveniles$Individual)
levels(wildjuveniles$Individual)

intersect(wildadults$Individual, wildjuveniles$Individual)

# Group size min and max
countsavailablegroup2 <- bigdata %>%
  group_by(TestDate, Group) %>%
  summarise(available=n_distinct(Individual))

countsavailablegrouprange2 <- countsavailablegroup2 %>%
  group_by(Group) %>%
  summarise(rangeav=range(available))
write.table(countsavailablegrouprange, "groupcounts.txt")
View(countsavailablegrouprange2)

  