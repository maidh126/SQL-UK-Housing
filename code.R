# install.packages("DBI")
# install.packages("odbc")
library(haven)
library(dplyr)
library(odbc)
library(readxl)

##### House Paid ##### 
housepaid <- read.csv("/Users/maido/Desktop/Dataset/Data Science/HousePaid.csv")

##### BroadBand #####
broadband <- read.csv("/Users/maido/Desktop/Dataset/Data Science/Broadband.csv")


##### House Median ##### 
HM <- read_excel("/Users/maido/Desktop/Dataset/Data Science/HouseMedian.xlsx")

# Rename the column name
names(HM)[names(HM) == "Ward name"] <- "WardName"
names(HM)[names(HM) == "Ward code"] <- "WardCode"
names(HM)[names(HM) == "Local authority code"] <- "DistrictCode"
names(HM)[names(HM) == "Local authority name"] <- "DistrictName"


# Create District table
district <- subset(HM, select=c(DistrictCode, DistrictName))
# Remove duplicate 
district <- district %>% distinct(DistrictCode, .keep_all = TRUE)

# Create Ward table
ward <- subset(HM, select=c(DistrictCode, WardCode, WardName))
# Remove duplicate
ward <- ward %>% distinct(WardCode, .keep_all = TRUE)


# Remove "Year ending " in column name
for ( col in 1:ncol(HM)){
  colnames(HM)[col] <-  sub("Year ending ", "", 
                                     colnames(HM)[col])}
# Remove " "
names(HM) <- gsub("\\ ", "", names(HM))

# Extract Quarter and Year
Quarter <- substring(names(HM[,5:101]),1,3)
Year <- substring(names(HM[,5:101]),4,7)

# Create vector Quarter and Year
Quarter <- rep(Quarter, times = nrow(HM))
Year <- rep(Year, times = nrow(HM))

# Transpose HM
HMtran <- t(HM)

# Create a vector that replicate n times corresponding to n row of HMtran (exclude first 4 columns that not contain data)
WardCode <- rep(c(HMtran[3,1:ncol(HMtran)]),times=nrow(HMtran)-4)
  
# Create a vector that contains all of house median value
Median <- as.vector(HMtran[5:nrow(HMtran),])

# If median value is not a number then convert to 0
for(j in 1:length(Median)){if (is.na(as.double(Median[j]))){Median[j] = 0}}


# Create housemedian dataframe
housemedian <- data.frame(WardCode,Quarter,Year,Median)



##### Postcode #####
postcode <- read.csv("/Users/maido/Desktop/Dataset/Data Science/Postcode.csv", header = TRUE)

# Select only Oxfordshire
postcode <- postcode %>% filter(lad11nm == "Oxford")

# Rename the column name
names(postcode)[1] <- "PostCode"
names(postcode)[names(postcode) == "wd11cd"] <- "WardCode"

# Keep PostCode and WardCode
postcode <- subset(postcode, select = c(PostCode, WardCode))


##### Connect with Database ######

# Connect to DB
conn <- dbConnect(RSQLite::SQLite(), "Database.db")

# Insert Ward
dbGetQuery(conn, "SELECT count(*) FROM Ward")
dbWriteTable(conn, name="Ward", value=ward, append=TRUE)

# Insert District
dbGetQuery(conn, "SELECT count(*) FROM District")
dbWriteTable(conn, name="District", value=district, append=TRUE)

# Insert HouseMedian
dbGetQuery(conn, "SELECT count(*) FROM HouseMedian")
dbWriteTable(conn, name="HouseMedian", value=housemedian, append=TRUE)

# Insert HousePaid
dbGetQuery(conn, "SELECT count(*) FROM HousePaid")
dbWriteTable(conn, name="HousePaid", value=housepaid, append=TRUE)

# Insert PostCode
dbGetQuery(conn, "SELECT count(*) FROM PostCode")
dbWriteTable(conn, name="PostCode", value=postcode, append=TRUE)

# Insert Broadband
dbGetQuery(conn, "SELECT count(*) FROM Broadband")
dbWriteTable(conn, name="Broadband", value=broadband, append=TRUE)


##### Question 3-8 ######

# Question 3: The average prices of houses in 2018 in Cherwell
dbGetQuery(conn, "select avg(Median) from HouseMedian where year = '2018' and 
WardCode in (select WardCode from Ward where DistrictCode = (select 
           DistrictCode from District where DistrictName = 'Cherwell'))")

# Question 4: The average increase in prices between 2017 and 2018 in Headington Ward
dbGetQuery(conn,"select 	w.WardName, hm2017.Year as Year2017, 
(sum(hm2017.Median)/4) as AvgPrice2017, hm2018.Year as Year2018, 
(sum(hm2018.Median)/4) as AvgPrice2018, (sum(hm2018.Median)/4) - 
(sum(hm2017.Median)/4) as ChangeInPrice,((sum(hm2018.Median)/4) - 
(sum(hm2017.Median)/4))*100/(sum(hm2017.Median)/4) 
as 'ChangeInPercent(%)'
	from HouseMedian hm2017
	inner join HouseMedian hm2018 on hm2017.WardCode = hm2018.WardCode
	inner join Ward w on hm2017.WardCode = w.WardCode
	where w.WardName = 'Headington' and hm2017.Year = '2017' and hm2018.Year = '2018'
	group by hm2017.Year, hm2018.Year,w.WardName")


# Question 5: Find a ward which has the highest house price in Mar 2018
dbGetQuery(conn,"select  la.DistrictName, w.WardName,hp.Year, hp.Quarter, 
max(hp.PricePaid) from HousePaid hp
inner join PostCode pc on pc.PostCode = hp.PostCode
inner join Ward w on w.WardCode = pc.WardCode
inner join District la on la.DistrictCode = w.DistrictCode
where la.DistrictName in ('Oxford','Cherwell','South Oxfordshire',
'Vale of White Horse','West Oxfordshire')
and hp.Quarter = 'Mar' and hp.Year = '2018'
group by hp.Quarter, hp.Year, la.DistrictName, w.WardName
order by max(hp.PricePaid) desc
limit 1")


# Question 6: find a ward which has the lowest house price in Dec 2019.
dbGetQuery(conn,"select  la.DistrictName, w.WardName,hp.Year, hp.Quarter, 
max(hp.PricePaid) from HousePaid hp
inner join PostCode pc on pc.PostCode = hp.PostCode
inner join Ward w on w.WardCode = pc.WardCode
inner join District la on la.DistrictCode = w.DistrictCode
where la.DistrictName in ('Oxford','Cherwell','South Oxfordshire',
'Vale of White Horse','West Oxfordshire')
and hp.Quarter = 'Dec' and hp.Year = '2019'
group by hp.Quarter, hp.Year, la.DistrictName, w.WardName
order by max(hp.PricePaid)
limit 1")


# Question 7: Broadband speed broadband availability (%) in Headington Ward.
dbGetQuery(conn,"select w.WardName, bs.* from Broadband bs 
inner join Ward w on bs.WardCode = w.WardCode
where w.WardName = 'Headington'")


# Question 8:
# Find the ward which has max average download speed
dbGetQuery(conn,"select  max(bs.AverageDownloadSpeed ), w.WardName 
from Broadband bs inner join Ward w on bs.WardCode = w.WardCode")

# Find the ward which has max broadband availability percent
dbGetQuery(conn,"select  max(bs.AvailabilityPercent ), w.WardName 
from Broadband bs inner join Ward w on bs.WardCode = w.WardCode")



dbGetQuery(conn, "delete from HouseMedian")
dbGetQuery(conn, "delete from Ward")
dbGetQuery(conn, "delete from HousePaid")
dbGetQuery(conn, "delete from Broadband")
dbGetQuery(conn, "delete from District")
dbGetQuery(conn, "delete from PostCode")


##### Disconnect ######
dbDisconnect(conn)


