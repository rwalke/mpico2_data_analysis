# 2024-09-16
# within this program we use Nominatim to get coordinates for a number of railway stations
# input is a list of stations, output are a list of stations with consolidated names and coordinates aquired by nominatim


library(data.table)
library(nominatimlite)
library(urltools)
library(ggplot2)
library(sf)

# https://wiki.openstreetmap.org/wiki/Nominatim/Country_Codes

# example
# s7 <- "Köln Messe/Deutz Gl."
# s7 <- "Köln Messe/Deutz"
# url_encode(s7)
# 
# railway7 <- geo_lite_sf(url_encode(s7), limit=20, full_results=TRUE, custom_query = list(countrycodes="de,ch,dk,se,nl,si"))
# r7 <- as.data.table(railway7)
# r7[type=='station',]


# read all unique station names from file
# stationstable0
load(file="stationstable0.Rdata")
stationstable0

# step 1
# modify unknown names, this is an iterative process, it should give reliable results from Nominatim
stationstable1 <- copy(stationstable0)

stationstable1[station_name=="Koebenhavn H", station_name_replace:="København"]
stationstable1[station_name=="Koebenhavns Lufthavn St (Dk)", station_name_replace:="Københavns Lufthavn"]

stationstable1[station_name=="Hauptbahnhof, Wiesba", station_name_replace:="Hauptbahnhof, Wiesbaden"]

stationstable1[station_name=="Groningen Cs Hoofdst", station_name_replace:="Groningen Hoofdstation"]
stationstable1[station_name=="Groningen", station_name_replace:="Groningen Hoofdstation"]
stationstable1[station_name=="Groningen (Nl)", station_name_replace:="Groningen Hoofdstation"]

stationstable1[station_name=="Düsseldorf Flugh.", station_name_replace:="Düsseldorf Flughafen"]

stationstable1[station_name=="Ljubljana (Si)", station_name_replace:="Ljubljana station"]
stationstable1[station_name=="Köln Messe/Deutz Gl.", station_name_replace:="Köln Messe/Deutz"]
stationstable1[station_name=="Köln", station_name_replace:="Köln Hbf"]
stationstable1[station_name=="Koeln Hbf", station_name_replace:="Köln Hbf"]
stationstable1[station_name=="Koln Hbf (De)", station_name_replace:="Köln Hbf"]

stationstable1[station_name=="Leiden (Nl)", station_name_replace:="Leiden Centraal"]
stationstable1[station_name=="Amsterdam", station_name_replace:="Amsterdam Centraal"]
stationstable1[station_name=="Amsterdam Central Station", station_name_replace:="Amsterdam Centraal"]

stationstable1[station_name=="Augsburg Hbf (De)", station_name_replace:="Augsburg Hbf"]
stationstable1[station_name=="Bielefeld Hbf (De)", station_name_replace:="Bielefeld Hbf"]
stationstable1[station_name=="Wiesbaden Hbf (De)", station_name_replace:="Wiesbaden Hbf"]
stationstable1[station_name=="Wien Hbf (At)", station_name_replace:="Wien Hbf"]
stationstable1[station_name=="Stendal (De)", station_name_replace:="Stendal Hbf"]
stationstable1[station_name=="Schwerin (De)", station_name_replace:="Schwerin Hbf"]
stationstable1[station_name=="Leipzig Hbf (De)", station_name_replace:="Leipzig Hbf"]
stationstable1[station_name=="Mannheim Hbf (De)", station_name_replace:="Mannheim Hbf"]
stationstable1[station_name=="Mannheim", station_name_replace:="Mannheim Hbf"]

stationstable1[station_name=="Dortmund Hbf (De)", station_name_replace:="Dortmund Hbf"]
stationstable1[station_name=="Dresden Hbf (De)", station_name_replace:="Dresden Hbf"]
stationstable1[station_name=="Fulda (De)", station_name_replace:="Fulda"]
stationstable1[station_name=="Halle (Saale) Hbf (De)", station_name_replace:="Halle (Saale) Hbf"]
stationstable1[station_name=="Halle(Saale)Hbf", station_name_replace:="Halle (Saale) Hbf"]
stationstable1[station_name=="Leer (Ostfriesl) (De)", station_name_replace:="Leer (Ostfriesland) Bahnhof"]

stationstable1[station_name=="Heidelberg Hbf (De)", station_name_replace:="Heidelberg Hbf"]
stationstable1[station_name=="Jena Paradies (De)", station_name_replace:="Jena Paradies"]
stationstable1[station_name=="Kassel Hbf (De)", station_name_replace:="Kassel Hbf"]
stationstable1[station_name=="Kassel", station_name_replace:="Kassel Hbf"]

stationstable1[station_name=="Goettingen", station_name_replace:="Göttingen"]
stationstable1[station_name=="Göttingen (De)", station_name_replace:="Göttingen"]


stationstable1[station_name=="Den Haag Centraal (Nl)", station_name_replace:="Den Haag Centraal"]
stationstable1[station_name=="Den Haag", station_name_replace:="Den Haag Centraal"]

stationstable1[station_name=="Bremen Hbf (De)", station_name_replace:="Bremen Hbf"]
stationstable1[station_name=="Bremen", station_name_replace:="Bremen Hbf"]


stationstable1[station_name=="Odense st", station_name_replace:="Odense station"]
stationstable1[station_name=="Odense", station_name_replace:="Odense station"]
stationstable1[station_name=="Odense St (Dk)", station_name_replace:="Odense station"]

stationstable1[station_name=="Brüssel Central", station_name_replace:="Brussel Centraal"]
stationstable1[station_name=="Brüssel", station_name_replace:="Brussel Centraal"]
stationstable1[station_name=="Trient", station_name_replace:="Trento FTM"]
stationstable1[station_name=="Trento", station_name_replace:="Trento FTM"]

stationstable1[station_name=="Berlin Rath.Steglitz", station_name_replace:="Berlin Rathaus Steglitz"]
stationstable1[station_name=="Berlin Friedrichstra", station_name_replace:="Berlin Friedrichstraße"]

stationstable1[station_name=="Berlin Brandenb. Flug", station_name_replace:="Berlin-Brandenburg Flughafen"]
stationstable1[station_name=="Berlin-Brandenb. Flughf.", station_name_replace:="Berlin-Brandenburg Flughafen"]
stationstable1[station_name=="Berlin-Schoenef.Flug", station_name_replace:="Berlin-Brandenburg Flughafen"]
stationstable1[station_name=="Berlin airport", station_name_replace:="Berlin-Brandenburg Flughafen"]
stationstable1[station_name=="Berlin-Schönefeld Flughafen (De)", station_name_replace:="Berlin-Brandenburg Flughafen"]
stationstable1[station_name=="Stadtmitte (U), Berlin", station_name_replace:="Berlin Stadtmitte Bahnhof"]


stationstable1[station_name=="Berlin Ost Bf (De)", station_name_replace:="Berlin Ostbahnhof"]
stationstable1[station_name=="Berlin Ostbahnhof (S)", station_name_replace:="Berlin Ostbahnhof"]

stationstable1[station_name=="Berlin Hbf (tief)", station_name_replace:="Berlin Hbf"]
stationstable1[station_name=="Berlin Hbf (Tief)", station_name_replace:="Berlin Hbf"]
stationstable1[station_name=="Berlin Hbf (De)", station_name_replace:="Berlin Hbf"]
stationstable1[station_name=="Berlin Hbf S Bahn", station_name_replace:="Berlin Hbf"]
stationstable1[station_name=="Berlin", station_name_replace:="Berlin Hbf"]

stationstable1[station_name=="Hamburg Hbf (De)", station_name_replace:="Hamburg Hbf"]
stationstable1[station_name=="Hamburg", station_name_replace:="Hamburg Hbf"]
stationstable1[station_name=="Hamburg Dammtor (De)", station_name_replace:="Hamburg Dammtor"]
stationstable1[station_name=="Hamburg Flughafen", station_name_replace:="Hamburg Flughafen Bahnhof"]
stationstable1[station_name=="Hamburg Airport", station_name_replace:="Hamburg Flughafen Bahnhof"]

stationstable1[station_name=="Flughafen Tegel (Airport), Berlin", station_name_replace:="Tegel, Berlin"]
stationstable1[station_name=="Berlin-Tegel (S)", station_name_replace:="Tegel, Berlin"]

stationstable1[station_name=="Rostock Parkstrasse", station_name_replace:="Rostock Hbf"]
stationstable1[station_name=="Massmannstrasse, Rostock", station_name_replace:="Rostock Hbf"]
stationstable1[station_name=="Rostock-Lichtenhagen", station_name_replace:="Rostock Hbf"]
stationstable1[station_name=="Warnemuende Werft", station_name_replace:="Rostock Hbf"]
stationstable1[station_name=="Warnemünde (De)", station_name_replace:="Rostock Hbf"]
stationstable1[station_name=="Parkstrasse (S), Rostock", station_name_replace:="Rostock Hbf"]
stationstable1[station_name=="Rostock Holbeinplatz", station_name_replace:="Rostock Hbf"]
stationstable1[station_name=="Rostock Albrecht-Kos", station_name_replace:="Rostock Hbf"]
stationstable1[station_name=="rostock hauptbahnhof", station_name_replace:="Rostock Hbf"]
stationstable1[station_name=="Rostock", station_name_replace:="Rostock Hbf"]
stationstable1[station_name=="Rostock Hbf (De)", station_name_replace:="Rostock Hbf"]

stationstable1[station_name=="Muenchen Flughafen T", station_name_replace:="München Flughafen Terminal"]
stationstable1[station_name=="Muenchen Flughafen Te", station_name_replace:="München Flughafen Terminal"]
stationstable1[station_name=="München-Ost Hbf", station_name_replace:="München-Ost Bahnhof"]
stationstable1[station_name=="München", station_name_replace:="München Hbf"]
stationstable1[station_name=="Muenchen Hbf", station_name_replace:="München Hbf"]

stationstable1[station_name=="WiesbadenHbf (De)", station_name_replace:="Wiesbaden Hbf"]
stationstable1[station_name=="Warszawa Centralna (Pl)", station_name_replace:="Warszawa Centralna"]
stationstable1[station_name=="Rotterdam Cs (Nl)", station_name_replace:="Rotterdam Centraal"]
stationstable1[station_name=="Paris Est (Fr)", station_name_replace:="Paris Est"]
stationstable1[station_name=="Paris", station_name_replace:="Paris Est"]

stationstable1[station_name=="Brüssel-Midi", station_name_replace:="Brussel-Zuid"]
stationstable1[station_name=="Bruxelles-Nord (Be)", station_name_replace:="Bruxelles-Nord"]


stationstable1[station_name=="Tegernsee (De)", station_name_replace:="Tegernsee"]
stationstable1[station_name=="Adrian-Stoop-Strasse, Bad Wiessee", station_name_replace:="Tegernsee"]

stationstable1[station_name=="Praha Hlavni Nadrazi (Cz)", station_name_replace:="Praha Hlavni Nadrazi"]
stationstable1[station_name=="Praha hl.n.", station_name_replace:="Praha Hlavni Nadrazi"]

stationstable1[station_name=="Potsdam Hbf (S)", station_name_replace:="Potsdam Hbf"]
stationstable1[station_name=="Platz der Einheit/West, Potsdam", station_name_replace:="Potsdam Hbf"]

# fwrite(stationstable1, file="stationstable1.csv", quote=TRUE)
save(stationstable1, file="stationstable1.Rdata")


# step 2
# use geo_lite_sf to get a list of coordinates from Nominatim
stations2 <- stationstable1[,sort(unique(station_name_replace))]


get_coord1 <- function(station1="Rostock Hbf") {
  sf1 <- geo_lite_sf(url_encode(station1), limit=20, full_results=TRUE, custom_query = list(countrycodes="de,ch,be,fr,nl,it,dk,cz,pl,at,si,se"))
  sf1d <- as.data.table(sf1)
  sf1d[,station_name_replace:=station1]
  # print(sf1d[type=='station',])
  Sys.sleep(1) # to avoid overload
  return(sf1d)
}


coordinates1 <- list()

for (i in 1:length(stations2)){
  print(i)
  c1 <- get_coord1(stations2[i])
  coordinates1[[i]] <- c1
}

# save temporary results
# save(coordinates1, file="coordinates1.Rdata")

# load(file="coordinates1.Rdata")

# step 3
# reduce the number of elements to 1 per place

coordinates2 <- list()
for (i in 1:length(coordinates1)){
  
  e1 <- coordinates1[[i]]
  e1Station <- e1[type=='station',]
  
  dim_e1 <- dim(e1)[1]
  min_place_id_e1 <- e1[,min(place_id)]
  
  dim_e1Station  <- dim(e1Station)[1]
  min_place_id_e1Station <- ifelse(dim_e1Station==0, NA, e1Station[,min(place_id)])
  
  print(paste(i,dim_e1,dim_e1Station, min_place_id_e1, min_place_id_e1Station))
  
  # prefer a station (take the place with the smallest place_id)
  if(dim_e1Station>0) e2 <- e1Station[place_id==min_place_id_e1Station,]
  if(dim_e1Station==0 ) e2 <- e1[place_id==min_place_id_e1,]
  
  coordinates2[[i]] <- e2
}

# bind the result as data.table
coordinates3 <- rbindlist(coordinates2, fill=TRUE)

# n24 <- coordinates3[24,geometry]
# is(n24)

coordinates4 <- coordinates3[,.(place_id, osm_id, osm_type, category, type, name, station_name_replace, geometry)]


# recreate a simple feature collection
# https://epsg.io/4326

p4s <- "EPSG:4326"
coordinates5 = st_sf(name = coordinates4$name, station_name_replace=coordinates4$station_name_replace, segment=1, geom = st_sfc(coordinates4$geometry), crs = p4s)
coordinates5
class(coordinates5)

save(coordinates5, file="coordinates5.Rdata")
# fwrite(coordinates5, file="coordinates5.csv", quote=TRUE)

# step 4
# create plot to check the output
ggplot(coordinates5) + geom_sf()

GER <- geo_lite_sf("Germany", points_only = FALSE)

ggplot(coordinates5) + geom_sf(data=GER) + geom_sf(col="red")

# compute distances
# https://luftlinie.org

coordinates5[98,]
coordinates5[10,]
st_distance(coordinates5[98,], coordinates5[10,])


dist_m <- st_distance(coordinates5, coordinates5)
range(dist_m)
row(dist_m)[dist_m==max(dist_m)]
col(dist_m)[dist_m==max(dist_m)]
coordinates5[117,]
coordinates5[106,]
st_distance(coordinates5[117,], coordinates5[106,])





