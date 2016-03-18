library(RJSONIO)
library(dplyr)
library(tidyr)
library(curl)
library(readr)
library(ggplot2)
library(lubridate)

## loop that gets data form aire.df webpage for each year and turns list into data frame. outputs dataframes with "page" pattern by year
for(i in seq(2006,2016))
  {
    url <- paste0("http://148.243.232.112:8080/opendata/anuales_horarios/contaminantes_", i, ".json")
    url1 <- URLencode(url)
    json <- readLines(url1,  warn = "F")
    list <- fromJSON(json)
    page <- as.data.frame(do.call(rbind,  list$pollutionMeasurements$date), stringsAsFactors = FALSE)
    assign(paste("page", i, sep = "_"), page)
       
  }

## lists for loop sequences
nombres <- c("carbon", NA, NA, NA, "ozono", "PM10")
numeros <- c(1, 5, 6)

## loops that select CO, O3 and PM10 from larger datafrma, and outputs a dataframe with the average of the maximum for each day 
## excludes 2016 because cleaning is different
for(j in 2006:2015) {
  for(i in numeros){
    x <- data.frame(level = unlist(get(paste0("page_", j))[[i]])) ## unlist and take date from rownames. use "get" to turn char into name
    x$date <- rownames(x)
    rownames(x) <- seq(1:nrow(x))
    
    x <- x %>%
      separate(date, c("date", "station"), sep = "\\.", extra = "merge") %>%
      mutate(date = ymd_hms(date, truncated = 1)) %>%  
      filter(year(date) < max(year(date)))
    
    x$level[x$level == ""] <- NA
    x$level <- as.numeric(as.character(x$level))
    
    x <- x %>%
      group_by(day = day(date), month = month(date), year = year(date)) %>%
      summarise(max = max(level, na.rm = TRUE)) %>%
      group_by(month, year) %>%
      summarise(max_avg = mean(max))
    
    assign(paste(nombres[i], j, sep = "_"), x)
  }
}

## same loop as above, but for 2016 only
for(i in numeros)
{
  x <- data.frame(level = unlist(page_2016[[i]]))
  x$date <- rownames(x)
  rownames(x) <- seq(1:nrow(x))
  
  x <- x %>%
    separate(date, c("date", "station"), sep = "\\.", extra = "merge") %>%
    mutate(date = ymd_hms(date, truncated = 1)) %>%
    filter(month(date) < max(month(date)))
  
  x$level[x$level == ""] <- NA
  x$level <- as.numeric(as.character(x$level))
  
  x <- x %>%
    group_by(day = day(date), month = month(date), year = year(date)) %>%
    summarise(max = max(level, na.rm = TRUE)) %>%
    group_by(month, year) %>%
    summarise(max_avg = mean(max))
  
  assign(paste(nombres[i], "2016", sep = "_"), x)
}


## bind all year dataframes into a single dataframe by pollutant
carbon <- do.call(rbind, lapply(ls(pattern = "carbon_"), get))
ozono <- do.call(rbind, lapply(ls(pattern = "ozono_"), get))
PM10 <- do.call(rbind, lapply(ls(pattern = "PM10_"), get))



###function to generate plots for each polutant with lines by year.
## includes red line on august. in july 2015 the SCJN allowed for older cars to avoid HNC if they passed pollution test
cont_plot <- function(cont) {
  nombre  <- gsub("()","",sys.call()[2]) ## nombre and titles needed in order to change ggtitle depending on pollutant
  titles <- c(PM10 = "Promedio de máximos por mes de PM10 en la CDMX",
              carbon ="Promedio de máximos por mes de carbono en la CDMX", 
              ozono = "Promedio de máximos por mes de ozono en la CDMX")
  
  ggplot(cont, aes(month, max_avg, color = as.factor(year))) +
    geom_line() + scale_x_discrete() +
    theme_bw() +
    xlab("mes") + ylab("nivel") +
    geom_vline(aes(xintercept = 8), linetype=4, color = "red") +
    guides(color = guide_legend(title = "año")) +
    ggtitle(titles[nombre])
  
}

##call plots for each polllutant
cont_plot(PM10)
cont_plot(carbon)
cont_plot(ozono)

## extra stuff used while coding but not relevant for final code.

# # 
# list1 <- fromJSON("http://148.243.232.112:8080/opendata/anuales_horarios/contaminantes_2015.json")
# page_2016 <- as.data.frame(do.call(rbind, list1$pollutionMeasurements$date), stringsAsFactors = FALSE)
# 
# # 
# 
# contam <- function(cont2) {
#   
#   cont <- data.frame(level = unlist(cont2))
#   cont$date <- rownames(cont)
#   rownames(cont) <- seq(1:nrow(cont))
#   
#   cont <- cont %>%
#     separate(date, c("date", "station"), sep = "\\.", extra = "merge") %>%
#     mutate(date = ymd_hms(date, truncated = 1)) %>%
#     filter(year(date) < max(year(date)))
#   
#   cont$level[cont$level == ""] <- NA
#   cont$level <- as.numeric(as.character(cont$level))
#   
#   cont <- cont %>%
#     group_by(day = day(date), month = month(date), year = year(date)) %>%
#     summarise(max = max(level, na.rm = TRUE)) %>%
#     group_by(month, year) %>%
#     summarise(max_avg = mean(max))
#   
# }
# 
# 
# 
# for(i in 2006:2016)
#  {
#   x <- contam(get(paste0("page_", i))[[1]]);
#   assign(paste("carbon", i, sep = "_"), x)
#  }
# 
# for(i in 2006:2016)
# {
#   x <- contam(get(paste0("page_", i))[[5]]);
#   assign(paste("ozono", i, sep = "_"), x)
# }
# 
# for(i in 2006:2016)
# {
#   x <- contam(get(paste0("page_", i))[[6]]);
#   assign(paste("PM10", i, sep = "_"), x)
# }

