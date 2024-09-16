# 2024-09-16
# this program reads a number of raw Excel file, does some preprocessing and computes differences between different locations

library(data.table)
library(readxl)
library(units)

library(sf)

data_dir <- file.path("..","..","raw_data")

rail1_2019 = as.data.table(read_excel(file.path(data_dir,"Bahnreport ORBS 2019 - 2021 _ 28JUL22.xlsx"), sheet = "2019"))
rail1_2020 = as.data.table(read_excel(file.path(data_dir,"Bahnreport ORBS 2019 - 2021 _ 28JUL22.xlsx"), sheet = "2020"))
rail1_2021 = as.data.table(read_excel(file.path(data_dir,"Bahnreport ORBS 2019 - 2021 _ 28JUL22.xlsx"), sheet = "2021"))
rail2_2019 = as.data.table(read_excel(file.path(data_dir,"Bahnbuchungsliste Offline Buchungen 2019 - 2021 _ 28JUL22.xlsx"), sheet = "2019"))
rail2_2020 = as.data.table(read_excel(file.path(data_dir,"Bahnbuchungsliste Offline Buchungen 2019 - 2021 _ 28JUL22.xlsx"), sheet = "2020"))
rail2_2021 = as.data.table(read_excel(file.path(data_dir,"Bahnbuchungsliste Offline Buchungen 2019 - 2021 _ 28JUL22.xlsx"), sheet = "2021"))

rail1_2022 = as.data.table(read_excel(file.path(data_dir,"Rail_Report_2022.xlsx"), sheet = "Tabelle1"))
rail1_2023 = as.data.table(read_excel(file.path(data_dir,"Rail_Report_2023.xlsx"), sheet = "Tabelle1"))

rail1_2019[,year:=2019]
rail1_2020[,year:=2020]
rail1_2021[,year:=2021]

rail2_2019[,year:=2019]
rail2_2020[,year:=2020]
rail2_2021[,year:=2021]

rail1_2022[,year:=2022]
rail1_2023[,year:=2023]

# first set
rail1 <- rbind(rail1_2019, rail1_2020, rail1_2021, rail1_2021, rail1_2022, rail1_2023, fill=TRUE)
# create a running id to keep the original order
rail1[, orderID:=.I]
# create a marker to distinguish orders by institute or offline
rail1[, purchase:="institute"]
rail1

# fill missing entries
setnafill(rail1, "locf", cols=c("CYTRIC_TRIP_ID","TOTAL_PRICE"))

rail1[,table(table(CYTRIC_TRIP_ID))]

rail1[ , BOOKING_STATUS2 := shift(BOOKING_STATUS, fill=BOOKING_STATUS[1L]), by=CYTRIC_TRIP_ID]

rail1[,table(BOOKING_STATUS)]
rail1[BOOKING_STATUS=="Aktiv",]

rail1[BOOKING_STATUS=="Storniert",]
rail1Storniert <- rail1[BOOKING_STATUS2=="Storniert",]

rail1b <- rail1[BOOKING_STATUS2!="Storniert",]



rail1pur <- rail1b[,.(year=year, orderID=orderID, purchase=purchase, orderNumber=CYTRIC_TRIP_ID, origin=RAIL_DEPARTURE_TRAIN_STATION_NAME, destination=RAIL_ARRIVAL_TRAIN_STATION_NAME)]
rail1pur


# second set
rail2 <- rbind(rail2_2019, rail2_2020, rail2_2021)
# create a running id to keep the original order add 10000 to differ from first set
# use even number to spare the odd rows for splitted trips
rail2[, orderID:=.I*2+10000]
# create a marker to distinguish orders by institute or offline
rail2[, purchase:="offline"]

# ignore lines with 'Zahlungsmittelentgelt' and Reservierung and small gross price
rail2[`Bahn-Klasse`==0 | Bahntarif=="Reservierung" | `Verkaufspreis Brutto`< 5.00,]
rail2[!`Bahn-Klasse`==0 & !Bahntarif=="Reservierung" & !`Verkaufspreis Brutto`< 5.00,]

rail2pur <- rail2[!`Bahn-Klasse`==0 & !Bahntarif=="Reservierung" & !`Verkaufspreis Brutto`< 5.00, .(year=year, orderID=orderID, purchase=purchase, connection=Bahnrouting)]

rail2pur


# split 3er connections into 2 2er connections

check1 <- "^(.*)-(.*)-(.*)$"
sub(check1, "\\1-\\2####\\2-\\3", "Berlin - Rostock - Berlin")

cases3A <- rail2pur[grepl(check1, connection)]
cases3B <- copy(cases3A)
cases3A[,connection:=sub(check1,"\\1-\\2", connection)]
cases3B[,connection:=sub(check1,"\\2-\\3", connection)]
cases3B[,orderID:=orderID+1]
cases3A
cases3B

rail2pur2 <- rbind(rail2pur[!grepl(check1, connection)], cases3A, cases3B)

check2 <- "^(.*) - (.*)$"
rail2pur2[,origin:=trimws(sub(check2, "\\1", connection))]
rail2pur2[,destination:=trimws(sub(check2, "\\2", connection))]
rail2pur2

rail12 <- rbind(rail1pur[,.(year, orderID, purchase, origin, destination)], rail2pur2[,.(year, orderID, purchase, origin, destination)])
rail12
save(rail12, file="rail12.Rdata")
# fwrite(rail12, file="rail12.csv", quote=TRUE)


# create an auxiliary file with all the different stations only

stations0 <- data.table(station_name=c(rail12$origin,rail12$destination))
stations0
table(duplicated(stations0))

(stationstable0 <- stations0[!duplicated(stations0)])
# prepare the next step
stationstable0[,station_name_replace:=station_name]
save(stationstable0, file="stationstable0.Rdata")
# fwrite(stationstable0, file="stationstable0.csv", quote=TRUE)


# run a program that consolidates the station name an adds coordinates via Nominatim
# it uses data from an external database, results may change over time
# do not run this subprogram if not needed
source("railDistanceCoordinates.R")



# next step merge the known names and distances

load(file="rail12.Rdata")

# stationstable1 connection between station names and consolidated station names
load(file="stationstable1.Rdata")

# coordinates5 coordinates for consolidated station names
load(file="coordinates5.Rdata")

rail12geom221 <- merge(rail12, stationstable1, by.x = "origin", by.y = "station_name", all.x=TRUE)
rail12geom22 <- merge(rail12geom221, coordinates5, by.x = "station_name_replace", by.y = "station_name_replace", all.x=TRUE)

setnames(rail12geom22, old = c('station_name_replace','name','segment','geom'), new = c('origin_replace','origin_osm_name','origin_segment','origin_geom'))

rail12geom2 <- merge(rail12geom22, stationstable1, by.x = "destination", by.y = "station_name", all.x=TRUE)
rail12geom <- merge(rail12geom2, coordinates5, by.x = "station_name_replace", by.y = "station_name_replace")

setnames(rail12geom, old = c('station_name_replace','name','segment','geom'), new = c('destination_replace','destination_osm_name','destination_segment','destination_geom'))

rail12geom[, distance:=st_distance(origin_geom, destination_geom, by_element=TRUE)]
units(rail12geom$distance) <- make_units(km)

setkeyv(rail12geom, c("year", "orderID"))
rail12geom
rail12geom[,sum(distance)]
rail12geom[,sum(distance)/.N]

rail12geom[,.N, by=year]
rail12geom[,sum(distance), by=year]
rail12geom[,sum(distance)/.N, by=year]

rail12geom[,.N, by=.(purchase, year)]
rail12geom[,sum(distance), by=.(purchase, year)]
rail12geom[,sum(distance)/.N, by=.(purchase, year)]

rail12geom[,.(number=.N, distance=sum(distance)/.N, total=sum(distance)), by=.(purchase, year)]


save(rail12geom, file="rail12geom.Rdata")

# fwrite(rail12geom, file="rail12geom.csv", quote=TRUE)


#
