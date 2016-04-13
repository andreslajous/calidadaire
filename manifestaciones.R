library(RJSONIO)
library(dplyr)
library(tidyr)
library(curl)
library(readr)
library(ggplot2)
library(lubridate)

list1 <- fromJSON("http://148.243.232.112:8080/opendata/anuales_horarios/contaminantes_2014.json")
page_2014 <- as.data.frame(do.call(rbind, list1$pollutionMeasurements$date), stringsAsFactors = FALSE)

nombres <- c("carbon", NA, NA, NA, "ozono", "PM10")
numeros <- c(1, 5, 6)

for(i in numeros)
{
  x <- data.frame(level = unlist(page_2014[[i]]))
  x$date <- rownames(x)
  rownames(x) <- seq(1:nrow(x))
  
  x <- x %>%
    separate(date, c("date", "station"), sep = "\\.", extra = "merge") %>%
    mutate(date = ymd_hms(date, truncated = 1)) %>%
    #filter(month(date) < max(month(date)))
    filter(year(date) < max(year(date)))
  
  x$level[x$level == ""] <- NA
  x$level <- as.numeric(as.character(x$level))
  
  x <- x %>%
    group_by(day = day(date), month = month(date), year = year(date), station) %>%
    summarise(max = max(level, na.rm = TRUE)) #%>%
  #     group_by(month, year) %>%
  #     summarise(max_avg = mean(max))
  
  assign(paste(nombres[i], "2014", sep = "_"), x)
}


carbon_2014 <- carbon_2014 %>% 
  mutate(fecha = dmy(paste(day, month, year, sep = "-"))) %>%
  rename(estacion = station)

ozono_2014 <- ozono_2014 %>% 
  mutate(fecha = dmy(paste(day, month, year, sep = "-"))) %>%
  rename(estacion = station)

PM10_2014 <- PM10_2014 %>% 
  mutate(fecha = dmy(paste(day, month, year, sep = "-"))) %>%
  rename(estacion = station)

# incidentes <- read_csv("data/viales2014.csv")
# 
# marchas <- incidentes %>%
#   filter(Asunto == "MNF - Manifestacion" | Asunto == "PLT - Planton"| Asunto == "MCH - Marcha") %>%
#   filter(!grepl("sin afectar la vial", Descripción), !grepl("sin afectar vial", Descripción)) %>%
#   select(Asunto, Latitud, Longitud, `Fecha/Hora de apertura`)
# 
# 
# marchas1 <- marchas %>%
#   filter(!is.na(Latitud) & !is.na(Longitud)) %>%
#   mutate(date = dmy_hm(`Fecha/Hora de apertura`)) %>%
#   separate(`Fecha/Hora de apertura`, c("fecha", "hora"), 10) %>%
#   mutate(fecha = dmy(fecha), day = wday(date, label = TRUE),
#          hour = hour(date),
#          month = month(date, label = TRUE, abbr = TRUE)) %>%
#   select(-hora)


cols <- names(marchas)
cols[1] <- "ID"
marchas <- read_csv("marchas2014.csv", col_names = cols, skip = 1)

estacion_m <- read_csv("marcha_estacion2014.csv") 

marchas <- left_join(marchas, estacion_m, by = "ID")

marchas_e <- marchas %>%
  group_by(fecha, estacion, day, month) %>%
  summarise(manifest = n()) %>%
  ungroup() %>%
  mutate(fecha = ymd(fecha)) 

marchas_e <- marchas_e %>%
  rename(wday = day, month_ab = month)

carbon <- left_join(carbon_2014, marchas_e, by = c("fecha", "estacion"))
ozono  <- left_join(ozono_2014, marchas_e, by = c("fecha", "estacion"))
PM10 <- left_join(PM10_2014, marchas_e, by = c("fecha", "estacion"))

carbon$manifest[is.na(carbon$manifest)] <- 0
ozono$manifest[is.na(ozono$manifest)] <- 0
PM10$manifest[is.na(PM10$manifest)] <- 0



carbon <- carbon %>%
  mutate(wday = wday(fecha), manifest_bi = ifelse(manifest > 0, 1, 0)) %>%
  filter(!is.na(max)) %>%
  select(-month_ab) 


ozono <- ozono %>%
  mutate(wday = wday(fecha), manifest_bi = ifelse(manifest > 0, 1, 0)) %>%
  filter(!is.na(max)) %>%
  select(-month_ab) 

PM10 <- PM10 %>%
  mutate(wday = wday(fecha), manifest_bi = ifelse(manifest > 0, 1, 0)) %>%
  filter(!is.na(max)) %>%
  select(-month_ab) 


a <- lm(max ~ manifest_bi + factor(estacion) + factor(month) + factor(wday), data = carbon)
b <- lm(max ~ manifest_bi + factor(estacion) + factor(month) + factor(wday), data = ozono)
c <- lm(max ~ manifest_bi + factor(estacion) + factor(month) + factor(wday), data = PM10)

cl   <- function(dat,fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum, na.rm = TRUE));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) }


cl(carbon, a, carbon$estacion)
cl(ozono, b, ozono$estacion)
cl(PM10, c, PM10$estacion)