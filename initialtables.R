# initial looks at arson files for 2010-2015

options(scipen=999) 
#reading in the arson files
ar2012 <- read.table('fire2012/arson.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)
ar2013 <- read.table('fire2013/arson.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)
ar2014 <- read.table('fire2014/arson.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)
ar2015 <- read.table('fire2015/arson.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)


# reading in the basic incident files
bi2012 <- read.table('fire2012/basicincident.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)
bi2013 <- read.table('fire2013/basicincident.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)
bi2014 <- read.table('fire2014/basicincident.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)
bi2015 <- read.table('fire2015/basicincident.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)


# reading in the juv files
aj2012 <- read.table('fire2012/arsonjuvsub.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)
aj2013 <- read.table('fire2013/arsonjuvsub.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)
aj2014 <- read.table('fire2014/arsonjuvsub.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)
aj2015 <- read.table('fire2015/arsonjuvsub.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)


# reading in the header files
header2012 <- read.table('fire2012/fdheader.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)
header2013 <- read.table('fire2013/fdheader.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)
header2014 <- read.table('fire2014/fdheader.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)
header2015 <- read.table('fire2015/fdheader.txt', sep = '^', header = TRUE, stringsAsFactors = FALSE)

library(dplyr)
# joining basic incident and header
bih12 <- inner_join(bi2012, header2012)
bih12$VERSION <- NULL
bih13 <- inner_join(bi2013, header2013)
bih14 <- inner_join(bi2014, header2014)
bih15 <- inner_join(bi2015, header2015)


# joining basic incident, header, and arson
ars2012 <- inner_join(bih12, ar2012)
ars2012$VERSION <- NULL
ars2013 <- inner_join(bih13, ar2013)
ars2014 <- inner_join(bih14, ar2014)
ars2015 <- inner_join(bih15, ar2015)


# joining basic incident, header, arson, and juv
arson2012 <- inner_join(ars2012, aj2012)
arson2013 <- inner_join(ars2013, aj2013)
arson2014 <- inner_join(ars2014, aj2014)
arson2015 <- inner_join(ars2015, aj2015)

under1812 <- subset(x = arson2012, subset = arson2012$AGE <= 18)
under1813 <- subset(x = arson2013, subset = arson2013$AGE <= 18)
under1814 <- subset(x = arson2014, subset = arson2014$AGE <= 18)
under1815 <- subset(x = arson2015, subset = arson2015$AGE <= 18)

# now that i have 3 tables with all cases of arson for these years commited by youth 18 or under,
# I'm going to split up the dates and begin anaylzing the data
library(lubridate)
under1812$INC_DATE <- mdy(under1812$INC_DATE)
under1813$INC_DATE <- mdy(under1813$INC_DATE)
under1814$INC_DATE <- mdy(under1814$INC_DATE)
under1815$INC_DATE <- mdy(under1815$INC_DATE)

library(tidyr)
library(data.table)
under1812 <- under1812 %>% separate(col = INC_DATE, c("Year", "Month", "Date"))
under1813 <- under1813 %>% separate(col = INC_DATE, c("Year", "Month", "Date"))
under1814 <- under1814 %>% separate(col = INC_DATE, c("Year", "Month", "Date"))
under1815 <- under1815 %>% separate(col = INC_DATE, c("Year", "Month", "Date"))
under1813$VERSION <- NULL
under1814$VERSION <- NULL
under1815$VERSION <- NULL
under1812$VERSION <- NULL

full <- rbind(under1812, under1813, under1814, under1815)
full <- as.data.table(full)

# deleting columns I will not use in analysis
full$ALARM <- NULL
full$ARRIVAL <- NULL
full$FD_STR_NO <- NULL
full$FD_STR_PRE <- NULL
full$FD_STREET <- NULL
full$FD_STR_TYP <- NULL
full$ADD_WILD <- NULL
full$INC_CONT <- NULL
full$LU_CLEAR <- NULL
full$SHIFT <- NULL
full$ALARMS <- NULL
full$FD_PHONE <- NULL
full$FD_EMAIL <- NULL
full$CENSUS <- NULL
full$FD_STR_SUF <- NULL
full$FD_FAX <- NULL
full$LAB_USED1 <- NULL
full$LAB_USED2 <- NULL
full$LAB_USED3 <- NULL
full$LAB_USED4 <- NULL
full$LAB_USED5 <- NULL
full$LAB_USED6 <- NULL
full$EXP_NO <- NULL
full$DISTRICT <- NULL
full$INIT_OB1 <- NULL
full$INIT_OB2 <- NULL
full$INIT_OB3 <- NULL
full$INIT_OB4 <- NULL
full$INIT_OB5 <- NULL
full$INIT_OB6 <- NULL
full$INIT_OB7 <- NULL
full$INIT_OB8 <- NULL
full$ACT_TAK1 <- NULL
full$ACT_TAK2 <- NULL
full$ACT_TAK3 <- NULL
full$NO_STATION <- NULL
full$ENTRY_METH<- NULL
full$PROP_OWNER<- NULL
full$AID<- NULL
full$INC_TYPE<- NULL
full$INV_INFO1 <- NULL
full$INV_INFO2 <- NULL
full$INV_INFO3 <- NULL
full$INV_INFO4 <- NULL
full$INV_INFO5 <- NULL
full$INV_INFO6 <- NULL
full$INV_INFO7 <- NULL
full$INV_INFO8 <- NULL
full$GRP_INVOL1 <- NULL
full$GRP_INVOL2 <- NULL
full$GRP_INVOL3 <- NULL
full$JUV_DISPO <- NULL
full$DEVI_CONT <- NULL
full$DEVI_FUEL <- NULL
full$DEVI_IGNIT <- NULL
full$EXT_FIRE <- NULL
full$RISK_FACT8 <- NULL
full$FF_DEATH <- NULL
full$CASE_STAT <- NULL
full$APP_MOD <- NULL
full$DEPT_STA <- NULL
full$AVAIL_MFI <- NULL
full$SUP_APP <- NULL
full$EMS_APP <- NULL
full$OTH_APP <- NULL
full$SUP_PER <- NULL
full$EMS_PER <- NULL
full$OTH_PER <- NULL
full$RESOU_AID <- NULL
full$OTH_DEATH <- NULL
full$FF_INJ <- NULL
full$OTH_INJ <- NULL
full$NO_VOL_PDC <- NULL
full$DET_ALERT <- NULL
full$HAZ_REL <- NULL
full$MIXED_USE <- NULL
full$FD_FIP_CTY <- NULL
full$NO_PD_FF <- NULL
full$NO_VOL_FF <- NULL
full$PROP_VAL <- NULL
full$CONT_VAL <- NULL
full$PROP_USE <- NULL

# changing codes to show their definition 

factor(full$MOT_FACTS1)
factor(full$MOT_FACTS2)
factor(full$MOT_FACTS3)
factor(full$RISK_FACT1)
factor(full$RISK_FACT2)
factor(full$RISK_FACT3)
factor(full$RISK_FACT4)
factor(full$RISK_FACT5)
factor(full$RISK_FACT6)
factor(full$RISK_FACT7)
factor(full$RISK_FACT8)

# RISK FACTORS
levels(factor(full$RISK_FACT1))
levels(full$RISK_FACT1) <- c(levels(full$RISK_FACT1), "mild", "moderate", "extreme", 'unknown')
full$RISK_FACT1[full$RISK_FACT1 == "1 -- Mild curiosity about fire"] <- "mild"
full$RISK_FACT1[full$RISK_FACT1 == "2 -- Moderate curiosity about fire"] <- "moderate"
full$RISK_FACT1[full$RISK_FACT1 == "3 -- Extreme curiosity about fire"] <- "extreme"
full$RISK_FACT1[full$RISK_FACT1 == "U -- Unknown"] <- 'unknown'
full$RISK_FACT1[full$RISK_FACT1 == "Z"] <- 'unknown'
full$RISK_FACT1[full$RISK_FACT1 == ''] <- 'unknown'
full$RISK_FACT1[full$RISK_FACT1 == 'unknown'] <- NA
full$RISK_FACT1[full$RISK_FACT1 == "1"] <- "mild"
full$RISK_FACT1[full$RISK_FACT1 == "2"] <- "moderate"
full$RISK_FACT1[full$RISK_FACT1 == "3"] <- "extreme"
colnames(full)[which(names(full) == "RISK_FACT1")] <- "Fire Curiosity Level"
full$`Fire Curiosity Level`[is.na(full$`Fire Curiosity Level`)] <- 'unknown'
#ADD/ADHD
colnames(full)[which(names(full) == "RISK_FACT2")] <- "ADD_ADHD"
full$ADD_ADHD[full$ADD_ADHD == '4'] <- 'yes'
full$ADD_ADHD[full$ADD_ADHD != 'yes'] <- 'no'
full$ADD_ADHD[is.na(full$ADD_ADHD)] <- 'no'

# out of school trouble
colnames(full)[which(names(full) == "RISK_FACT3")] <- "TroubleOutsideSchool"
full$TroubleOutsideSchool[full$TroubleOutsideSchool == '5'] <- 'yes'
full$TroubleOutsideSchool[full$TroubleOutsideSchool != 'yes'] <- 'no'
full$TroubleOutsideSchool[is.na(full$TroubleOutsideSchool)] <- 'no'


# history of stealing or shoplifting 
colnames(full)[which(names(full) == "RISK_FACT4")] <- "StealingShoplifting"
full$StealingShoplifting[full$StealingShoplifting == '6'] <- 'yes'
full$StealingShoplifting[full$StealingShoplifting != 'yes'] <- 'no'
full$StealingShoplifting[is.na(full$StealingShoplifting)] <- 'no'


#history of physical assault
colnames(full)[which(names(full) == "RISK_FACT5")] <- "PhysicalAssault"
full$PhysicalAssault[full$PhysicalAssault == '7'] <- 'yes'
full$PhysicalAssault[full$PhysicalAssault != 'yes'] <- 'no'
full$PhysicalAssault[is.na(full$PhysicalAssault)] <- 'no'

# history of fireplay/firesetting
colnames(full)[which(names(full) == "RISK_FACT6")] <- "Firesetting"
full$Firesetting[full$Firesetting == '8'] <- 'yes'
full$Firesetting[full$Firesetting != 'yes'] <- 'no'
full$Firesetting[is.na(full$Firesetting)] <- 'no'


# transiency
colnames(full)[which(names(full) == "RISK_FACT7")] <- "Transiency"
full$Transiency[full$Transiency == '9'] <- 'yes'
full$Transiency[full$Transiency != 'yes'] <- 'no'
full$Transiency[is.na(full$Transiency)] <- 'no'


# MOTIVATION FACTORS 1
levels(full$MOT_FACTS1)
levels(full$MOT_FACTS1) <- c(levels(full$MOT_FACTS1), 'unknown', 'extortion', 'labor unrest', 'insurance fraud', 
                                 'intimidation', 'void contract/lease', 'foreclosed property', 'personal', 'hate crime', 
                                 'institutional', 'societal', 'protest', 'civil unrest', 'fireplay/curiosity', 
                                 'vanity/recognition', 'thrills', 'attention/sympathy', 'homicide', 'suicide', 
                                 'domestic violence', 'burglary', 'homicide concealment', 'burglary concealment',
                                 'auto theft concealment', 'destroy records/evidence', 'other')
full$MOT_FACTS1[full$MOT_FACTS1 == ''] <- 'unknown'
full$MOT_FACTS1[full$MOT_FACTS1 == 'UU'] <- 'unknown'
full$MOT_FACTS1[full$MOT_FACTS1 == '11'] <- 'extortion'
full$MOT_FACTS1[full$MOT_FACTS1 == '12'] <- 'labor unrest'
full$MOT_FACTS1[full$MOT_FACTS1 == '13'] <- 'insurance fraud'
full$MOT_FACTS1[full$MOT_FACTS1 == '14'] <- 'intimidation'
full$MOT_FACTS1[full$MOT_FACTS1 == '15'] <- 'void contract/lease'
full$MOT_FACTS1[full$MOT_FACTS1 == '16'] <- 'foreclosed property'
full$MOT_FACTS1[full$MOT_FACTS1 == '21'] <- 'personal'
full$MOT_FACTS1[full$MOT_FACTS1 == '22'] <- 'hate crime'
full$MOT_FACTS1[full$MOT_FACTS1 == '23'] <- 'institutional'
full$MOT_FACTS1[full$MOT_FACTS1 == '24'] <- 'societal'
full$MOT_FACTS1[full$MOT_FACTS1 == '31'] <- 'protest'
full$MOT_FACTS1[full$MOT_FACTS1 == '32'] <- 'civil unrest'
full$MOT_FACTS1[full$MOT_FACTS1 == '41'] <- 'fireplay/curiosity'
full$MOT_FACTS1[full$MOT_FACTS1 == '42'] <- 'vanity/recognition'
full$MOT_FACTS1[full$MOT_FACTS1 == '43'] <- 'thrills'
full$MOT_FACTS1[full$MOT_FACTS1 == '44'] <- 'attention/sympathy'
full$MOT_FACTS1[full$MOT_FACTS1 == '51'] <- 'homicide'
full$MOT_FACTS1[full$MOT_FACTS1 == '52'] <- 'suicide'
full$MOT_FACTS1[full$MOT_FACTS1 == '53'] <- 'domestic violence'
full$MOT_FACTS1[full$MOT_FACTS1 == '54'] <- 'burglary'
full$MOT_FACTS1[full$MOT_FACTS1 == '61'] <- 'homicide concealment'
full$MOT_FACTS1[full$MOT_FACTS1 == '62'] <- 'burglary concealment'
full$MOT_FACTS1[full$MOT_FACTS1 == '63'] <- 'auto theft concealment'
full$MOT_FACTS1[full$MOT_FACTS1 == '64'] <- 'destroy records/evidence'
full$MOT_FACTS1[full$MOT_FACTS1 == '00'] <- 'other'
full$MOT_FACTS1[is.na(full$MOT_FACTS1)] <- 'unknown'


# MOTIVATION FACTORS 2

levels(full$MOT_FACTS2)
levels(full$MOT_FACTS2) <- c(levels(full$MOT_FACTS2),'labor unrest', 'insurance fraud', 'intimidation', 'void contract/lease',
                                'personal', 'institutional', 'societal', 'protest', 
                                'fireplay/curiosity', 'vanity/recognition', 'thrills', 'attention/sympathy', 
                                'homicide', 'suicide', 'domestic violence', 'burglary', 'burglary concrealment',
                                'auto theft concealment', 'destroy records/evidence')

full$MOT_FACTS2[full$MOT_FACTS2 == '12'] <- 'labor unrest'
full$MOT_FACTS2[full$MOT_FACTS2 == '13'] <- 'insurance fraud'
full$MOT_FACTS2[full$MOT_FACTS2 == '14'] <- 'intimidation'
full$MOT_FACTS2[full$MOT_FACTS2 == '15'] <- 'void contract/lease'
full$MOT_FACTS2[full$MOT_FACTS2 == '21'] <- 'personal'
full$MOT_FACTS2[full$MOT_FACTS2 == '23'] <- 'institutional'
full$MOT_FACTS2[full$MOT_FACTS2 == '24'] <- 'societal'
full$MOT_FACTS2[full$MOT_FACTS2 == '31'] <- 'protest'
full$MOT_FACTS2[full$MOT_FACTS2 == '41'] <- 'fireplay/curiosity'
full$MOT_FACTS2[full$MOT_FACTS2 == '42'] <- 'vanity/recognition'
full$MOT_FACTS2[full$MOT_FACTS2 == '43'] <- 'thrills'
full$MOT_FACTS2[full$MOT_FACTS2 == '44'] <- 'attention/sympathy'
full$MOT_FACTS2[full$MOT_FACTS2 == '51'] <- 'homicide'
full$MOT_FACTS2[full$MOT_FACTS2 == '52'] <- 'suicide'
full$MOT_FACTS2[full$MOT_FACTS2 == '53'] <- 'domestic violence'
full$MOT_FACTS2[full$MOT_FACTS2 == '54'] <- 'burglary'
full$MOT_FACTS2[full$MOT_FACTS2 == '62'] <- 'burglary concealment'
full$MOT_FACTS2[full$MOT_FACTS2 == '63'] <- 'auto theft concealment'
full$MOT_FACTS2[full$MOT_FACTS2 == '64'] <- 'destroy records/evidence'
full$MOT_FACTS2[is.na(full$MOT_FACTS2)] <- 'unknown'
full$MOT_FACTS2[full$MOT_FACTS2 == ''] <- 'unknown'


# MOTIVATION FACTORS 3

levels(factor(full$MOT_FACTS3))
levels(full$MOT_FACTS3) <- c(levels(full$MOT_FACTS3), 'insurance fraud', 'personal', 'fireplay/curiosity', 
                                 'vanity/recognition', 'thrills', 'attention/sympathy', 'sexual excitement', 'suicide', 
                                 'domestic violence', 'auto theft concealment')

full$MOT_FACTS3[full$MOT_FACTS3 == '13'] <- 'insurance fraud'
full$MOT_FACTS3[full$MOT_FACTS3 == '21'] <- 'personal'
full$MOT_FACTS3[full$MOT_FACTS3 == '41'] <- 'fireplay/curiosity'
full$MOT_FACTS3[full$MOT_FACTS3 == '42'] <- 'vanity/recognition'
full$MOT_FACTS3[full$MOT_FACTS3 == '43'] <- 'thrills'
full$MOT_FACTS3[full$MOT_FACTS3 == '44'] <- 'attention/sympathy'
full$MOT_FACTS3[full$MOT_FACTS3 == '45'] <- 'sexual excitement'
full$MOT_FACTS3[full$MOT_FACTS3 == '52'] <- 'suicide'
full$MOT_FACTS3[full$MOT_FACTS3 == '53'] <- 'domestic violence'
full$MOT_FACTS3[full$MOT_FACTS3 == '63'] <- 'auto theft concealment'
full$MOT_FACTS3[full$MOT_FACTS3 == '14'] <- 'intimidation'
full$MOT_FACTS3[full$MOT_FACTS3 == '32'] <- 'civil unrest'
full$MOT_FACTS3[full$MOT_FACTS3 == '64'] <- 'destroy records/evidence'
full$MOT_FACTS3[full$MOT_FACTS3 == '51'] <- 'homicide'
full$MOT_FACTS3[full$MOT_FACTS3 == '23'] <- 'institutional'
full$MOT_FACTS3[full$MOT_FACTS3 == '54'] <- 'burglary'
full$MOT_FACTS3[full$MOT_FACTS3 == ''] <- 'unknown'
full$MOT_FACTS3[is.na(full$MOT_FACTS3)] <- 'unknown'


# FAMILY TYPE

levels(factor(full$FAM_TYPE))
levels(full$FAM_TYPE) <- c(levels(full$FAM_TYPE), 'other', 'single-parent family', 'foster parents', 'two-parent family',
                               'extended family', 'no family unit', 'unknown')
full$FAM_TYPE[full$FAM_TYPE == '0'] <- "other"
full$FAM_TYPE[full$FAM_TYPE == '1'] <- "single-parent family"
full$FAM_TYPE[full$FAM_TYPE == '2'] <- "foster parents"
full$FAM_TYPE[full$FAM_TYPE == '3'] <- "two-parent family"
full$FAM_TYPE[full$FAM_TYPE == '4'] <- "extended family"
full$FAM_TYPE[full$FAM_TYPE == 'N'] <- "no family unit"
full$FAM_TYPE[full$FAM_TYPE == 'U'] <- "unknown"
full$FAM_TYPE[full$FAM_TYPE == ''] <- "unknown"
full$FAM_TYPE[full$FAM_TYPE == 'unknown'] <- NA
full$FAM_TYPE[is.na(full$FAM_TYPE)] <- 'unknown'


# RACE

levels(factor(full$RACE))
levels(full$RACE) <- c(levels(full$RACE), 'other/multiracial', 'White', 'Black or African American', 'Asian', 
                           'Native Hawaiian/Pacific Islander', "American Indian or Alaska Native", "undetermined")
full$RACE[full$RACE == '0'] <- 'other/multiracial'
full$RACE[full$RACE == '1'] <- 'White'
full$RACE[full$RACE == '2'] <- 'Black or African American'
full$RACE[full$RACE == '3'] <-"American Indian or Alaska Native"
full$RACE[full$RACE == '4'] <- "Asian"
full$RACE[full$RACE == '5'] <- 'Native Hawaiian/Pacific Islander'
full$RACE[full$RACE == 'U'] <- "undetermined"
full$RACE[is.na(full$RACE)] <- 'undetermined'
full$RACE[full$RACE == ''] <-  'undetermined'


# Ethnicity

levels(factor(full$ETHNICITY))
levels(full$ETHNICITY) <- c(levels(full$ETHNICITY), 'Hispanic/Latino', "Non Hispanic/Latino", 'unknown')
full$ETHNICITY[full$ETHNICITY == '0'] <- 'Non Hispanic/Latino'
full$ETHNICITY[full$ETHNICITY == '1'] <- 'Hispanic/Latino'
full$ETHNICITY[full$ETHNICITY == ''] <- 'unknown'
full$ETHNICITY[is.na(full$ETHNICITY)] <- 'unknown'


# GENDER

levels(factor(full$GENDER))
levels(full$GENDER) <- c(levels(full$GENDER), 'Male', 'Female', 'unknown')
full$GENDER[full$GENDER == '1'] <- 'Male'
full$GENDER[full$GENDER == '2'] <- 'Female'
full$GENDER[is.na(full$GENDER)] <- 'unknown'


head(full, n=10)

library(dplyr)
detach("package:plyr", unload=TRUE)
countingfullN <- full %>% 
  group_by(Year, STATE) %>%
  summarise(number = n())
max(countingfullN$number)


library(tidyr)
countingfull <- spread(countingfullN, key = Year, value = number)
countingfull[is.na(countingfull)] <- 0
head(countingfull, n = 10)
countingfu <- countingfull[, 2:5]
countingfullT <- cbind(countingfull, Total = rowSums(countingfu))

library(plyr)
library(ggplot2)

countingfullsub <- subset(countingfullN, STATE == 'MA')
countingfullsub2 <- subset(countingfullN, STATE == 'NY')
countingfullsub3 <- subset(countingfullN, STATE == "CT")
manyct <- rbind(countingfullsub, countingfullsub2, countingfullsub3)


manyctPLOT <- ggplot(manyct, aes(x=STATE,y = number, fill = Year)) +geom_bar(stat = "identity")
plot(manyctPLOT)


library(dplyr)
detach("package:plyr", unload=TRUE)
racegen <- full %>% 
  ungroup(STATE) %>%
  group_by(RACE, GENDER, Year) %>%
  summarise(number = n()) 
racegen <- subset(racegen, GENDER != 'unknown')
racegen <- subset(racegen, RACE != 'undetermined')
head(racegen, n = 10)

library(plyr)
library(ggplot2)
rgplot <- ggplot(racegen, aes(x = GENDER, y = number, fill = RACE)) +
         geom_bar(stat= 'identity') 

plot(rgplot)

head(countingfullT, n = 20)

#stats 
#2012 
c(countingfullT$STATE[countingfullT$`2012` == max(countingfullT$`2012`)], 
  max(countingfullT$`2012`))
c(countingfullT$STATE[countingfullT$`2012` == min(countingfullT$`2012`)],
  min(countingfullT$`2012`))

#2013
c(countingfullT$STATE[countingfullT$`2013` == max(countingfullT$`2013`)],
  max(countingfullT$`2013`))
c(countingfullT$STATE[countingfullT$`2013` == min(countingfullT$`2013`)],
  min(countingfullT$`2013`))

#2014
c(countingfullT$STATE[countingfullT$`2014` == max(countingfullT$`2014`)],
  max(countingfullT$`2014`))
c(countingfullT$STATE[countingfullT$`2014` == min(countingfullT$`2014`)],
  min(countingfullT$`2014`))

#2015
c(countingfullT$STATE[countingfullT$`2015` == max(countingfullT$`2015`)],
  max(countingfullT$`2015`))
c(countingfullT$STATE[countingfullT$`2015` == min(countingfullT$`2015`)],
  min(countingfullT$`2015`))

# total
c(countingfullT$STATE[countingfullT$Total == max(countingfullT$Total)],
  max(countingfullT$Total))
c(countingfullT$STATE[countingfullT$Total == min(countingfullT$Total)],
  min(countingfullT$Total))


#map data
library(ggplot2)
state <- full[, STATE]
citystate <- as.data.frame(state)
citystate <- unique(citystate)

geocodes <- geocode(as.character(citystate$state))
latlong <- cbind(citystate, geocodes)
fullat <- inner_join(x = countingfullN, y = latlong, by = c("STATE" = "state"))
mapone <- subset(fullat, Year == 2012)
maptwo <- subset(fullat, Year == 2013)
mapthr <- subset(fullat, Year == 2014)
mapfou <- subset(fullat, Year == 2015)
MAmap <- get_map(location= "USA",
                 source= "google", 
                 zoom= 4, scale="auto")

mmap <- ggmap(MAmap)

map1 <- 
  mmap +
  geom_point(data=mapone, aes(x=lon, y=lat, color=number))

map2 <- 
  mmap +
  geom_point(data=maptwo, aes(x=lon, y=lat, color=number))

map3 <- 
  mmap +
  geom_point(data=mapthr, aes(x=lon, y=lat, color=number))

map4 <- 
  mmap +
  geom_point(data=mapfou, aes(x=lon, y=lat, color=number))

plot(map1)
plot(map2) 
plot(map3)
plot(map4)
