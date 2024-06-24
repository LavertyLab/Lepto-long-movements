###########################################################
############ Determine long-distance movements ############
############## Script by: Theresa M. Laverty ##############
################ Contact: tlaverty@nmsu.edu ###############
###########################################################
############ Date Last Modified: 23-June-2024 #############
###########################################################

#Clear work environment
rm(list=ls())

#Set working directory
# Set wd to data on this computer. Also direct to homewd
#should be wherever "Lepto-long-movements" is stored on your computer
homewd = "/Users/tlaverty/Desktop/R/R_repositories/Lepto-long-movements/" 
setwd(paste0(homewd, "/", "data/"))

#View additional digits (for inspecting PIT tag numbers)
options(digits = 15)

#Load packages
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)

########################################################
#########################DATA###########################
########################################################

#Read in field data from capturing and PIT tagging bats
tagged_bats <- read.csv("raw-tagging-data/taggedLeptos.csv", header=T)
nrow(tagged_bats) #4025 
tagged_bats$id[duplicated(tagged_bats$id)=="TRUE"] #7 recaptures in data
length(unique(tagged_bats$id)) #4018 tagged bats total (7 recaptures)
tagged_bats$id[duplicated(tagged_bats$id)]
### Six of these seven tags were verified recaptures 
### ***DATA CLEANING ISSUES***
### "*989001040637656" is unlikely to be a recapture, but unfortunately can't say for sure
### Added an extra unknown Pinacate bat row for tag "*989001040638410" 

#Read in PIT tag reader data
#Subsetted to reads from only those bats that displayed movements >178 km
tag_reads <- read.csv("raw-reader-data/PITtagreads.csv", header=T)
nrow(tag_reads) #77608 
length(unique(tag_reads$id)) #84 bats

#read in distances between roosts
distances <- read.csv("raw-reader-data/distances_km.csv", header=T)

########################################################
####################PREPARE & PLOT######################
########################################################

#merge all tagged bat data with tag read data
all_master <- merge(tag_reads, tagged_bats, by = "id", all.x = TRUE)
head(all_master)
#order by date and time of tag read
newdata_all <- all_master[order(all_master$date, all_master$time),]
#filter to relevant columns
newdata <- newdata_all[,c('id', 'date','time', 'readsite', 'tagsite', 'tagdate', 'sex', 'reproductive_status','age')]
#add in a detect column
newdata$detect <- 1

#format date columns
newdata$date <- as.Date(newdata$date)
newdata$tagdate <- as.Date(newdata$tagdate, format = "%m/%d/%Y")
#order by date and time
newdata <- newdata[order(newdata$date, newdata$time),]

#add a grp variable to group consecutive dates at the same readsite together
test <- newdata %>%
  arrange(date) %>%
  group_by(id, tagdate) %>%
  mutate(grp = cumsum(c(TRUE, diff(date) > 1 | readsite[-1]!= readsite[-length(readsite)]))) %>%
  group_by(id,readsite, tagsite, tagdate, grp) %>%
  summarize(START = min(date), END = max(date)) %>%
  ungroup()
#format as a dataframe
test <- as.data.frame(test)
#reorder rows by tagsite and tagdate
test <- test[order(test$tagsite, test$tagdate),]

#add grp=0 for each unique tag to represent tagging event
test0 <- newdata %>%
  arrange(date) %>%
  group_by(id, tagdate) %>%
  filter(row_number()==1) %>%
  select(-c(date, detect)) %>%
  mutate(grp = 0, START = as.Date(tagdate), END = as.Date(tagdate), readsite=tagsite) %>%
  ungroup()%>%
  select(id, readsite, tagsite, tagdate, grp, START, END)
#format as dataframe
test0 <- as.data.frame(test0)

#merge test0 and test
test_all <- rbind(test,test0)
str(test_all)
#reorder by tagsite and tagdate and grp
test_all <- test_all[order(test_all$tagsite, test_all$tagdate, test_all$grp),]
#add lag_n column to include the previous grp's readsite
#lag_n will be NA for tagging events (when grp=0)
#lag_n will be the tagsite (when grp=1)
test_all <- (test_all %>% group_by(id) %>% mutate(lag_n = lag(readsite, n=1, default = NA)))
#reorder by tagsite and tagdate and grp
test_all <- test_all[order(test_all$tagsite, test_all$tagdate, test_all$grp),]
length(unique(test_all$id)) #84 bats

#bring in distance information
#build pairwise distance dataframe for tagsites
(test_all_0 <- merge(test_all[test_all$grp==0,],distances,by=c("readsite", "tagsite")))
test_all_0$places <- paste0(test_all_0$tagsite,"-",test_all_0$readsite)
colnames(distances)[2] <- "lag_n"

#build pairwise distance dataframe for readsites
(test_all_1 <- merge(test_all[test_all$grp>0,],distances,by=c("readsite", "lag_n")))
test_all_1$places <- paste0(test_all_1$lag_n,"-",test_all_1$readsite)

#reorder columns to match
colnames(test_all_0)
colnames(test_all_1)
test_all_1 <- test_all_1[,c(1,4,3,5:8,2,9:10)]

#combine test_all_0 and test_all_1
test_all <- rbind(test_all_0, test_all_1)
#reorder by tagsite, tagdate, and grp
test_all <- test_all[order(test_all$tagsite, test_all$tagdate, test_all$grp),]
#add timediff column with number of days between START date of one row and the previous END date
test_all <- test_all %>%
       group_by(id) %>%
       mutate(timediff = difftime(START, lag(END), unit='day'))
View(test_all)

#View movements and distances
table(test_all$distance_km)
View(test_all[rev(order(test_all$distance_km)),])
#Look at maximum movement distances by id
maxdist <- test_all%>%group_by(id)%>%summarize(max=max(distance_km))%>%arrange(desc(max))
View(maxdist)

#filter to movements > 178 km
bigdist2 <- test_all%>%group_by(id)%>%filter(distance_km>178)
nrow(bigdist2) #115 movements of >178km
length(unique(bigdist2$id)) #made by 84 individuals
View(bigdist2)

#look at time it takes to make movements
View(bigdist2 %>%
       group_by(places)%>%
       summarize(min=min(timediff), max=max(timediff)))
#filter to those movements that take only 1 day
View(bigdist2[bigdist2$timediff=="1",]) #10 rows

#rename some readsites/tagsites
newdata2 <- newdata
newdata2$readsite[newdata2$readsite=="gitana"] <- "sierra_cacachilas"
newdata2$readsite[newdata2$readsite=="chivato"] <- "sierra_cacachilas"
newdata2$tagsite[newdata2$tagsite=="gitana"] <- "sierra_cacachilas"
newdata2$tagsite[newdata2$tagsite=="chivato"] <- "sierra_cacachilas"
newdata2$readsite[newdata2$readsite=="fort_huachuca"] <- "huachuaca"
newdata2$readsite[newdata2$readsite=="coronado"] <- "huachuaca"
newdata2$tagsite[newdata2$tagsite=="fort_huachuca"] <- "huachuaca"
newdata2$tagsite[newdata2$tagsite=="coronado"] <- "huachuaca"
newdata2$tagsite[newdata2$tagsite=="huachuca_3_F"] <- "huachuaca"

#continue renaming some readsites/tagsites
df <- newdata2
df$readsite_plot <- plyr::mapvalues(df$readsite, from = c("big_hatchet", "peloncillo_1_F", "rincon","huachuca","organ_pipe","pinacate", "mariana", "navachiste","carmen", "sierra_cacachilas","don_panchito","atoyac"), to = c('Big Hatchet', 'Peloncillo', 'Tucson', 'Huachuca','Organ Pipe Cactus Nat. Monument', 'Pinacate', 'Mariana', 'Navachiste', 'Carmen', 'Sierra de las Cacachilas', 'Don Panchito', 'Atoyac'))
df$readsite_plot <- factor(df$readsite_plot, levels=c('Big Hatchet', 'Peloncillo', 'Tucson', 'Huachuca','Organ Pipe Cactus Nat. Monument', 'Pinacate', 'Mariana', 'Navachiste', 'Carmen', 'Sierra de las Cacachilas', 'Don Panchito', 'Atoyac'))
levels(df$readsite_plot)

df$tagsite_plot <- plyr::mapvalues(df$tagsite, from = c("big_hatchet", "peloncillo_1_F", "huachuca","organ_pipe","pinacate","mariana", "navachiste","carmen", "sierra_cacachilas", "don_panchito"), to = c('Big Hatchet', 'Peloncillo', 'Huachuca','Organ Pipe Cactus Nat. Monument', 'Pinacate', 'Mariana', 'Navachiste', 'Carmen', 'Sierra de las Cacachilas', 'Don Panchito'))
df$tagsite_plot[df$tagsite_new=="huachuca_5_F"] <- "Huachuca"
df$tagsite_plot[df$tagsite_new=="organ_pipe_1_h2o"] <- "Organ Pipe Cactus Nat. Monument"
df$tagsite_plot <- factor(df$tagsite_plot, levels=c('Big Hatchet', 'Peloncillo', 'Tucson', 'Huachuca','Organ Pipe Cactus Nat. Monument', 'Mariana', 'Pinacate', 'Navachiste', 'Carmen', 'Sierra de las Cacachilas', 'Don Panchito', 'Atoyac'))
levels(df$tagsite_plot)

#define color palette
MyPalette <- c('Big Hatchet'="#A6D854", 'Peloncillo'="#0f5e0c", 'Tucson'="#B3B3B3", 'Huachuca'="#8DA0CB",'Organ Pipe Cactus Nat. Monument'="#E78AC3", 'Pinacate'="#E5C494", 'Mariana'= "brown", 'Navachiste'="#FFD92F", 'Carmen'="#66C2A5", 'Sierra de las Cacachilas'="#FC8D62", 'Don Panchito'="#FF0000", 'Atoyac'="black")

#add column for Event and rows for tagging event
df2 <- df %>%
  as_tibble()%>%
  group_by(id) %>%
  group_modify(~add_row(.x, .before=0)) %>%
  mutate(id=id[2], tagdate=tagdate[2], tagsite=tagsite[2], tagsite_plot=tagsite_plot[2], sex=sex[2])%>%
  mutate(Event= if_else(row_number() == 1, "Tagged", "Detected")) %>%
  ungroup()%>%
  mutate(id = fct_reorder(id, as.numeric(tagdate)))

#determine number of males and females
df2%>%group_by(id)%>%slice(1)%>%group_by(sex)%>%summarize(N=n())
#make labels for males and females for facet_grid
df2$sex[df2$sex=="female"] <- "Female\n(n = 68)"
df2$sex[df2$sex=="male"] <- "Male\n(n = 16)"
#name Event for legend
df2$Event <- factor(df2$Event, levels=c('Tagged', 'Detected'))
levels(df2$Event)


##############################################
####ADD IN DATA ON WHEN READERS ARE ACTIVE####
##############################################

#big hatchet was active all days since 2017-08-14
#last download was 2023-10-30
bigh <- data.frame(date = seq(as.Date("2017-08-14"), as.Date("2023-10-30"), by="days"),
                  id = "A",
                  tagdate = as.Date("2013-11-01"),
                  readsite = NA,
                  tagsite = NA,
                  detect = 1,
                  sex = "Tag reader\nfunctioning",
                  readsite_plot = "Big Hatchet", 
                  tagsite_plot = NA,
                  Event = "Detected")
bigh <- bigh[,c(2,1,3,4,5,6,7,8,9,10)]
#Rincon was active on and off
#2014-09-20 to 2014-09-20
#2015-08-27 to 2015-11-15
#2016-08-25 to 2016-12-03
#2017-08-25 to 2017-12-27
#2018-08-24 to 2018-12-19
#2019-08-21 to 2019-12-05
#2020-09-02 to 2020-11-20
#2021-09-03 to 2021-11-08
#2022-08-19 to 2022-12-15
#2023-08-03 to 
#Last download was 2024-01-12
rin <- data.frame(date = c(as.Date("2014-09-20"),
                           seq(as.Date("2015-08-27"), as.Date("2015-11-15"), by="days"),
                           seq(as.Date("2016-08-25"), as.Date("2016-12-03"), by="days"),
                           seq(as.Date("2017-08-25"), as.Date("2017-12-27"), by="days"),
                           seq(as.Date("2018-08-24"), as.Date("2018-12-19"), by="days"),
                           seq(as.Date("2019-08-21"), as.Date("2019-12-05"), by="days"),
                           seq(as.Date("2020-09-02"), as.Date("2020-11-20"), by="days"),
                           seq(as.Date("2021-09-03"), as.Date("2021-11-08"), by="days"),
                           seq(as.Date("2022-08-19"), as.Date("2022-12-15"), by="days"),
                           seq(as.Date("2023-08-03"), as.Date("2024-01-12"), by="days")),
                  id ="B",
                  tagdate = as.Date("2013-10-01"),
                  readsite = NA,
                  tagsite = NA,
                  detect = 1,
                  sex = "Tag reader\nfunctioning",
                  readsite_plot = "Tucson",
                  tagsite_plot = NA,
                  Event = "Detected")
rin <- rin[,c(2,1,3,4,5,6,7,8,9,10)]
#Coronado was active on and off
#2016-08-08 to 2016-11-30
#2017-07-03 to 2017-11-25
#2018-07-01 to 2018-11-29
#2019-07-06 to 2019-12-18
#2020-07-02 to 2020-12-13
#2021-07-07 to 2021-10-14
#2022-07-14 to 2022-11-14
#2023-06-26 to 2023-11-16
#Last download was 2023-11-16
coro <- data.frame(date = c(seq(as.Date("2016-08-08"), as.Date("2016-11-30"), by="days"),
                           seq(as.Date("2017-07-03"), as.Date("2017-11-25"), by="days"),
                           seq(as.Date("2018-07-01"), as.Date("2018-11-29"), by="days"),
                           seq(as.Date("2019-07-06"), as.Date("2019-12-18"), by="days"),
                           seq(as.Date("2020-07-02"), as.Date("2020-12-13"), by="days"),
                           seq(as.Date("2021-07-07"), as.Date("2021-10-14"), by="days"),
                           seq(as.Date("2022-07-14"), as.Date("2022-11-14"), by="days"),
                           seq(as.Date("2023-06-26"), as.Date("2023-11-16"), by="days")),
                  id ="C",
                  tagdate = as.Date("2013-09-01"),
                  readsite = NA,
                  tagsite = NA,
                  detect = 1,
                  sex = "Tag reader\nfunctioning",
                  readsite_plot = "Huachuca",
                  tagsite_plot = NA,
                  Event = "Detected")
coro <- coro[,c(2,1,3,4,5,6,7,8,9,10)]
#Fort Huachuca was active on and off
#Last download was 2023-02-28
fort <- data.frame(date = c(as.Date("2019-08-17"),as.Date("2019-08-24"), as.Date("2019-08-31"), as.Date("2019-09-06"),as.Date("2019-09-13"),
                           as.Date("2019-09-21"), as.Date("2019-10-01"), as.Date("2019-10-16"), as.Date("2019-10-28"), as.Date("2019-11-24"),
                           as.Date("2020-06-29"), as.Date("2020-07-27"), as.Date("2020-08-08"), as.Date("2020-08-22"), as.Date("2020-09-02"),
                           as.Date("2020-09-11"), as.Date("2020-09-19"), as.Date("2020-09-30"), as.Date("2020-10-25"), as.Date("2020-11-25"),
                           seq(as.Date("2021-07-27"), as.Date("2021-08-15"), by="days"),
                           seq(as.Date("2021-08-22"), as.Date("2021-08-31"), by="days"),
                           seq(as.Date("2021-09-05"), as.Date("2021-09-22"), by="days"),
                           seq(as.Date("2022-07-28"), as.Date("2022-08-11"), by="days"),
                           seq(as.Date("2022-08-16"), as.Date("2023-02-28"), by="days")#,
                           #seq(as.Date("2023-07-07"), as.Date("2023-10-28"), by="days")
                           ),
                  id ="C",
                  tagdate = as.Date("2013-09-01"),
                  readsite = NA,
                  tagsite = NA,
                  detect = 1,
                  sex = "Tag reader\nfunctioning",
                  readsite_plot = "Huachuca",
                  tagsite_plot = NA,
                  Event = "Detected")
fort <- fort[,c(2,1,3,4,5,6,7,8,9,10)]
#merge coro and fort
huac <- rbind(coro, fort)
#remove duplicate rows
huac <- huac[!duplicated(huac$date),]
#Organ Pipe was active on and off
#2018-08-06 to 2018-08-07
#2018-08-21 
#2019-05-09 
#2019-06-08 
#2019-08-03 
#2019-08-12 
#2019-08-22 
#2020-06-13 to 2020-06-14
#2020-08-05 to 2020-08-06
#2020-08-22 
#2021-05-04
#2021-09-07
#2022-04-07 to 2022-10-10
#2023-05-25 to 2024-01-11 (north adit appeared to stop working around 2023-08-10)
#2023-03-07 to present (issues at south adit)
#Last download was: 2024-05-16
orpi <- data.frame(date = c(seq(as.Date("2018-08-06"), as.Date("2018-08-07"), by="days"),
                             as.Date("2018-08-21"),
                             as.Date("2019-05-09"),
                             as.Date("2019-06-08"),
                             as.Date("2019-08-03"),
                             as.Date("2019-08-12"),
                             as.Date("2019-08-22"),
                             seq(as.Date("2020-06-13"), as.Date("2020-06-14"), by="days"),
                             seq(as.Date("2020-08-05"), as.Date("2020-08-06"), by="days"),
                             as.Date("2020-08-22"),
                             as.Date("2021-05-04"),
                             #need to fill this in
                             as.Date("2021-09-07"),
                             seq(as.Date("2022-04-07"), as.Date("2022-10-10"), by="days"),
                             seq(as.Date("2023-05-25"), as.Date("2024-01-11"), by="days"),
                             seq(as.Date("2024-03-07"), as.Date("2024-05-16"), by="days")),
                    id ="E",
                    tagdate = as.Date("2013-07-01"),
                    readsite = NA,
                    tagsite = NA,
                    detect = 1,
                    sex = "Tag reader\nfunctioning",
                    readsite_plot = "Organ Pipe Cactus Nat. Monument",
                    tagsite_plot = NA,
                    Event = "Detected")
orpi <- orpi[,c(2,1,3,4,5,6,7,8,9,10)]
#pinacate was active on and off
#2021-05-22 to 2021-09-14
#2022-05-08 to 2022-09-18
#2023-05-15 to 2023-05-18 (reader failure this season)
#last download was 2023-10-30
pin <- data.frame(date = c(seq(as.Date("2021-05-22"), as.Date("2021-09-14"), by="days"),
                           seq(as.Date("2022-05-08"), as.Date("2022-09-18"), by="days"),
                           seq(as.Date("2023-05-15"), as.Date("2023-05-18"), by="days")),
                  id = "F",
                  tagdate = as.Date("2013-06-01"),
                  readsite = NA,
                  tagsite = NA,
                  detect = 1,
                  sex = "Tag reader\nfunctioning",
                  readsite_plot = "Pinacate", 
                  tagsite_plot = NA,
                  Event = "Detected")
pin <- pin[,c(2,1,3,4,5,6,7,8,9,10)]
#navachiste was active all days since 2017-08-14
#last download was 2024-03-30
nav <- data.frame(date = seq(as.Date("2022-02-25"), as.Date("2024-03-30"), by="days"),
                  id = "G",
                  tagdate = as.Date("2013-05-01"),
                  readsite = NA,
                  tagsite = NA,
                  detect = 1,
                  sex = "Tag reader\nfunctioning",
                  readsite_plot = "Navachiste", 
                  tagsite_plot = NA,
                  Event = "Detected")
nav <- nav[,c(2,1,3,4,5,6,7,8,9,10)]
#carmen was largely active continuously but had some problems pop up
#last download was 2024-01-01
car <- data.frame(date = c(seq(as.Date("2015-03-31"), as.Date("2015-12-31"), by="days"),
                           seq(as.Date("2016-02-01"), as.Date("2017-05-10"), by="days"),
                           seq(as.Date("2017-05-26"), as.Date("2021-05-25"), by="days"),
                           seq(as.Date("2021-05-29"), as.Date("2021-11-30"), by="days"),
                           seq(as.Date("2022-01-01"), as.Date("2022-04-30"), by="days"),
                           seq(as.Date("2022-06-10"), as.Date("2022-10-08"), by="days"),
                           seq(as.Date("2022-10-10"), as.Date("2022-10-20"), by="days"),
                           seq(as.Date("2022-10-23"), as.Date("2022-12-03"), by="days"),
                           seq(as.Date("2022-12-11"), as.Date("2023-01-31"), by="days"),
                           seq(as.Date("2023-04-12"), as.Date("2023-05-03"), by="days"),
                           seq(as.Date("2023-05-05"), as.Date("2023-05-14"), by="days"),
                           seq(as.Date("2023-06-26"), as.Date("2024-01-01"), by="days")#,
                           #seq(as.Date("2022-10-23"), as.Date("2022-12-03"), by="days")
                  ),
                  id = "H",
                  tagdate = as.Date("2013-04-01"),
                  readsite = NA,
                  tagsite = NA,
                  detect = 1,
                  sex = "Tag reader\nfunctioning",
                  readsite_plot = "Carmen", 
                  tagsite_plot = NA,
                  Event = "Detected")
car <- car[,c(2,1,3,4,5,6,7,8,9,10)]
#chivato was largely active continuously but had some problems pop up
#last download was 2024-01-07
chi <- data.frame(date = c(seq(as.Date("2015-01-02"), as.Date("2016-08-11"), by="days"),
                           seq(as.Date("2016-09-04"), as.Date("2016-12-26"), by="days"),
                           as.Date("2017-01-03"),
                           seq(as.Date("2017-01-06"), as.Date("2017-06-16"), by="days"),
                           seq(as.Date("2018-01-02"), as.Date("2020-11-17"), by="days"),
                           as.Date("2020-11-19"), as.Date("2020-11-23"), as.Date("2020-11-25"),
                           seq(as.Date("2020-11-27"), as.Date("2020-11-28"), by="days"),
                           as.Date("2020-12-03"),
                           seq(as.Date("2020-12-06"), as.Date("2020-12-09"), by="days"),
                           as.Date("2020-12-11"),
                           seq(as.Date("2020-12-14"), as.Date("2020-12-21"), by="days"),
                           seq(as.Date("2020-12-23"), as.Date("2020-12-24"), by="days"),
                           seq(as.Date("2020-12-26"), as.Date("2020-12-27"), by="days"),
                           seq(as.Date("2020-12-29"), as.Date("2020-12-31"), by="days"),
                           seq(as.Date("2021-02-01"), as.Date("2021-02-03"), by="days"),
                           seq(as.Date("2021-02-05"), as.Date("2021-02-17"), by="days"),
                           seq(as.Date("2021-02-21"), as.Date("2021-02-22"), by="days"),
                           seq(as.Date("2021-02-24"), as.Date("2021-03-01"), by="days"),
                           seq(as.Date("2021-03-06"), as.Date("2021-03-07"), by="days"),
                           seq(as.Date("2021-03-09"), as.Date("2021-03-15"), by="days"),
                           seq(as.Date("2021-03-17"), as.Date("2021-05-29"), by="days"),
                           seq(as.Date("2021-06-01"), as.Date("2021-06-07"), by="days"),
                           as.Date("2021-06-09"),
                           seq(as.Date("2021-06-11"), as.Date("2021-06-14"), by="days"),
                           seq(as.Date("2021-06-16"), as.Date("2021-06-21"), by="days"),
                           seq(as.Date("2021-06-23"), as.Date("2021-06-25"), by="days"),
                           as.Date("2021-06-27"),
                           seq(as.Date("2021-06-29"), as.Date("2021-07-02"), by="days"),
                           seq(as.Date("2021-07-07"), as.Date("2021-07-08"), by="days"),
                           as.Date("2021-07-10"), as.Date("2021-07-13"),as.Date("2021-07-18"),
                           seq(as.Date("2021-07-21"), as.Date("2021-07-24"), by="days"),
                           seq(as.Date("2021-07-26"), as.Date("2021-07-27"), by="days"),
                           seq(as.Date("2021-07-31"), as.Date("2021-08-01"), by="days"),
                           as.Date("2021-08-09"), as.Date("2021-08-17"), as.Date("2021-08-27"),
                           seq(as.Date("2021-08-31"), as.Date("2021-09-01"), by="days"),
                           seq(as.Date("2021-09-04"), as.Date("2021-09-08"), by="days"),
                           as.Date("2021-09-12"),as.Date("2021-09-15"),as.Date("2021-09-19"), as.Date("2021-09-25"),
                           seq(as.Date("2021-09-28"), as.Date("2021-09-29"), by="days"),
                           seq(as.Date("2021-10-04"), as.Date("2021-10-08"), by="days"),
                           seq(as.Date("2021-10-10"), as.Date("2021-10-17"), by="days"),
                           as.Date("2021-10-22"),
                           seq(as.Date("2021-10-24"), as.Date("2021-10-25"), by="days"),
                           as.Date("2021-10-27"),as.Date("2021-10-31"),
                           seq(as.Date("2021-11-03"), as.Date("2021-11-06"), by="days"),
                           seq(as.Date("2021-11-08"), as.Date("2021-11-18"), by="days"),
                           seq(as.Date("2021-11-20"), as.Date("2021-11-30"), by="days"),
                           as.Date("2021-12-03"),as.Date("2021-12-05"),
                           seq(as.Date("2021-12-10"), as.Date("2021-12-11"), by="days"),
                           seq(as.Date("2021-12-13"), as.Date("2021-12-14"), by="days"),
                           seq(as.Date("2021-12-17"), as.Date("2021-12-18"), by="days"),
                           seq(as.Date("2021-12-24"), as.Date("2021-12-25"), by="days"),
                           as.Date("2021-12-28"),
                           seq(as.Date("2022-01-08"), as.Date("2022-01-09"), by="days"),
                           as.Date("2022-01-25"),as.Date("2022-01-27"),
                           seq(as.Date("2022-02-02"), as.Date("2022-02-03"), by="days"),
                           seq(as.Date("2022-02-24"), as.Date("2022-02-25"), by="days"),
                           as.Date("2022-03-13"),as.Date("2022-03-15"),
                           seq(as.Date("2022-06-14"), as.Date("2024-01-07"), by="days")
                  ),
                  id = "J",
                  tagdate = as.Date("2013-03-01"),
                  readsite = NA,
                  tagsite = NA,
                  detect = 1,
                  sex = "Tag reader\nfunctioning",
                  readsite_plot = "Sierra de las Cacachilas", 
                  tagsite_plot = NA,
                  Event = "Detected")
chi <- chi[,c(2,1,3,4,5,6,7,8,9,10)]
#gitana was largely active continuously but had some problems pop up
#last download was 2024-01-07
gita <- data.frame(date = c(seq(as.Date("2017-01-02"), as.Date("2017-06-01"), by="days"),
                            as.Date("2017-06-03"), 
                            seq(as.Date("2017-06-05"), as.Date("2017-06-06"), by="days"),
                            as.Date("2017-06-09"), as.Date("2017-06-12"), 
                            seq(as.Date("2017-06-14"), as.Date("2017-06-21"), by="days"),
                            seq(as.Date("2017-09-25"), as.Date("2022-06-06"), by="days"),
                            as.Date("2022-06-08"),
                            seq(as.Date("2022-06-10"), as.Date("2022-06-12"), by="days"),
                            seq(as.Date("2022-06-14"), as.Date("2022-07-02"), by="days"),
                            as.Date("2022-07-04"), as.Date("2022-07-11"),
                            seq(as.Date("2022-07-18"), as.Date("2022-07-19"), by="days"),
                            seq(as.Date("2022-07-23"), as.Date("2022-07-25"), by="days"),
                            seq(as.Date("2022-07-29"), as.Date("2022-07-30"), by="days"),
                            as.Date("2022-08-01"),as.Date("2022-08-06"),
                            seq(as.Date("2022-08-08"), as.Date("2022-08-11"), by="days"),
                            seq(as.Date("2022-08-16"), as.Date("2022-08-17"), by="days"),
                            seq(as.Date("2022-08-19"), as.Date("2022-08-22"), by="days"),
                            as.Date("2022-08-24"),
                            seq(as.Date("2022-08-28"), as.Date("2022-08-29"), by="days"),
                            seq(as.Date("2022-09-01"), as.Date("2022-09-02"), by="days"),
                            as.Date("2022-09-11"),as.Date("2022-09-19"),
                            seq(as.Date("2022-09-22"), as.Date("2022-09-23"), by="days"),
                            as.Date("2022-09-29"), as.Date("2022-10-18"),
                            seq(as.Date("2022-10-25"), as.Date("2022-10-27"), by="days"),
                            as.Date("2022-10-30"),as.Date("2022-11-04"),as.Date("2022-11-13"),as.Date("2022-12-11"),
                            seq(as.Date("2022-12-13"), as.Date("2022-12-15"), by="days"),
                            as.Date("2022-12-20"),
                            seq(as.Date("2022-12-23"), as.Date("2022-12-25"), by="days"),
                            as.Date("2022-12-28"),
                            seq(as.Date("2023-01-11"), as.Date("2023-01-14"), by="days"),
                            seq(as.Date("2023-01-16"), as.Date("2023-01-17"), by="days"),
                            as.Date("2023-01-20"),as.Date("2023-01-25"),
                            seq(as.Date("2023-01-29"), as.Date("2023-01-30"), by="days"),
                            as.Date("2023-02-15"),as.Date("2023-02-19"), as.Date("2023-03-06"),as.Date("2023-03-09"), as.Date("2023-03-18"),
                            seq(as.Date("2023-03-24"), as.Date("2023-03-25"), by="days"),
                            as.Date("2023-03-27"),as.Date("2023-04-01"), as.Date("2023-04-04"), as.Date("2023-04-09"),
                            seq(as.Date("2023-04-11"), as.Date("2023-04-12"), by="days"),
                            seq(as.Date("2023-04-16"), as.Date("2023-04-18"), by="days"),
                            seq(as.Date("2023-04-20"), as.Date("2023-04-26"), by="days"),
                            seq(as.Date("2023-04-29"), as.Date("2023-04-30"), by="days"),
                            seq(as.Date("2023-05-02"), as.Date("2023-05-03"), by="days"),
                            as.Date("2023-05-06"),
                            seq(as.Date("2023-07-01"), as.Date("2024-01-07"), by="days")
                  ),
                  id = "J",
                  tagdate = as.Date("2013-03-01"),
                  readsite = NA,
                  tagsite = NA,
                  detect = 1,
                  sex = "Tag reader\nfunctioning",
                  readsite_plot = "Sierra de las Cacachilas", 
                  tagsite_plot = NA,
                  Event = "Detected")
gita <- gita[,c(2,1,3,4,5,6,7,8,9,10)]
#merge chi and gita
caca <- rbind(chi, gita)
#remove duplicate rows
caca <- caca[!duplicated(caca$date),]

#don_panchito was active 2023-12-03 to 2024-04-08 (vulture had messed up the antenna)
#last download was 2024-04-23
don <- data.frame(date = seq(as.Date("2023-12-03"), as.Date("2024-04-08"), by="days"),
                  id = "L",
                  tagdate = as.Date("2013-01-01"),
                  readsite = NA,
                  tagsite = NA,
                  detect = 1,
                  sex = "Tag reader\nfunctioning",
                  readsite_plot = "Don Panchito",
                  tagsite_plot = NA,
                  Event = "Detected")
don <- don[,c(2,1,3,4,5,6,7,8,9,10)]

#atoyac was active all days since 2022-06-21
#last download was 2024-05-17
ato <- data.frame(date = seq(as.Date("2022-06-21"), as.Date("2024-05-17"), by="days"),
                  id = "M", #change to M when don_panchito data comes in
                  tagdate = as.Date("2012-01-01"),
                  readsite = NA,
                  tagsite = NA,
                  detect = 1,
                  sex = "Tag reader\nfunctioning",
                  readsite_plot = "Atoyac", 
                  tagsite_plot = NA,
                  Event = "Detected")
ato <- ato[,c(2,1,3,4,5,6,7,8,9,10)]

#filter to relevant columns of df2
df2o <- df2[,c('id', 'date', 'tagdate', 'readsite', 'tagsite', 'detect',
              'sex', 'readsite_plot', 'tagsite_plot', 'Event')]

#using collapsed data
df2a <- rbind(df2o, bigh)
df2b <- rbind(df2a,rin)
df2c <- rbind(df2b,huac)
df2d <- rbind(df2c,orpi)
df2e <- rbind(df2d,pin)
df2f <- rbind(df2e,nav)
df2g <- rbind(df2f,car)
df2h <- rbind(df2g,caca)
df2i <- rbind(df2h,don)
df3 <- rbind(df2i,ato) 

##############################################
################PLOT TIMELINES################
##############################################

#plot the timeline for Figure 1d
timelines <- df3%>%
  mutate(id = fct_reorder(id, as.numeric(tagdate))) %>%
  ggplot(aes(date,id)) +
  geom_point(data= . %>% filter(Event=="Detected")%>%
               mutate(id = fct_reorder(id, as.numeric(tagdate))), 
             aes(color=readsite_plot, shape=Event), size=1)+
  geom_point(data= . %>% filter(Event=="Tagged")%>%
               mutate(id = fct_reorder(id, as.numeric(tagdate))), aes(x=tagdate, y=id, color=tagsite_plot, shape=Event),  size=1, stroke=1.3)+
  geom_point(data = . %>% filter(Event=="Detected") %>% filter(readsite_plot!=tagsite_plot)%>%
               mutate(id = fct_reorder(id, as.numeric(tagdate))),
             aes(color=readsite_plot), size=1, shape=16)+
  geom_point(data=data.frame(date=as.Date("2015-01-01"), id="*989001000315515", sex="Female\n(n = 68)"), size=1.5, shape=8, colour="#66C2A5")+
  facet_grid(sex~., scales = "free", space = "free")+
  guides(shape = guide_legend(override.aes = list(size = .3)))+
  guides(color = guide_legend(override.aes = list(size = .3)))+
  scale_shape_manual(values = c(4,16), breaks= c("Tagged", "Detected"))+ 
  scale_colour_manual(values = MyPalette, breaks= names(MyPalette))+
  scale_x_date(limit=c(as.Date("2015-01-01"),as.Date("2024-06-01")))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        panel.background = element_rect(fill="white"),
        strip.text.y = element_text(angle = 0))+
  labs(x="Date", y="Capture-detection histories for individual bats", color= "Site")+
  guides(color = guide_legend( 
    override.aes=list(shape = 16)))+ 
  theme(legend.position = c(.23, .74), legend.box = "horizontal",
        legend.background = element_rect(fill="transparent")
  )
timelines
ggsave(paste0(homewd,"figures/Figure1d.tiff"), width=9.2, height=6.5, units="in")


