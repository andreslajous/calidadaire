library(sp)
library(rgdal)

lat <- c(19.635501, 19.154674, 19.272100, 
         19.576963, 19.371612, 19.468404, 
         19.3262, 19.487227, 19.266948,
         19.265346, 19.350258, 19.365313, 
         19.722186, 19.298819, 19.271222, 
         19.313357, 19.482473, 19.4827,
         19.411617, 19.291968, 19.384413,
         19.534727, 19.483781, 19.443319,
         19.403, 19.578792, 19.42461,
         19.404050, 19.176900, 19.460415,
         19.429071, 19.393734, 19.325146,
         19.532968, 19.452592, 19.250385,
         19.357357, 19.446203, 19.246459, 
         19.529077, 19.602542, 19.360794,
         19.304441, 19.658223, 19.525995)





lon <- c(-98.912003, -99.162459, -99.207658,
         -99.254133, -99.158969, -99.169794,
         -99.1761, -99.114229, -98.886088,
         -99.02604, -99.157101, -99.291705, 
         -99.198602, -99.185774, -99.203971, 
         -99.310635, -99.243524, -99.094517,
         -99.152207, -99.38052, -99.117641,
         -99.11772, -99.147312, -99.21536,
         -99.242062, -99.039644, -99.119594,
         -99.202603, -98.990189, -98.902853,
         -99.131924, -99.028212, -99.204136,
         -99.030324, -99.086095, -99.256462,
         -99.262865, -99.207868, -99.010564,
         -99.204597, -99.177173, -99.07388,
         -99.103629, -99.09659, -99.0824)  


estacion <- c("ACO", "AJU","AJM",
              "ATI", "BJU", "CAM",
              "CCA", "TEC", "CHO",
              "COR", "COY", "CUA", 
              "CUT", "DIC", "EAJ", 
              "EAL", "FAC", "GAM",
              "HGM", "INN", "IZT",
              "LPR", "LAA", "IBM",
              "LOM", "LLA", "MER",
              "MGH", "MPA", "MON",
              "MCM", "NEZ", "PED",
              "SAG", "SJA", "SNT",
              "SFE", "SHA", "TAH",
              "TLA", "TLI", "UIZ",
              "UAX", "VIF", "XAL") 

estaciones <- data.frame(estacion = estacion, lat = lat, lon = lon)
coords <- cbind(estaciones$lon, estaciones$lat)
sp <- SpatialPoints(coords)
spdf <- SpatialPointsDataFrame(coords, estaciones)

writeOGR(spdf, "estaciones","estaciones", driver="ESRI Shapefile")

marchas <- read_csv("marchas2014.csv", col_names = cols, skip = 1)

coords1 <- cbind(marchas$Longitud, marchas$Latitud)
sp <- SpatialPoints(coords1)
spdf1 <- SpatialPointsDataFrame(coords1, marchas)

writeOGR(spdf1, "marchas","marchas", driver="ESRI Shapefile")