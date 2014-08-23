# Severe weather effects on health and economy

## Synopsis

## Data processing

### Libraries

First we need to load libraries that we will use later.


```r
library(ggplot2)
library(plyr)
library(stringr)
library(magrittr)
library(reshape2)
```

### Getting and reading data

The  U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database file is downloaded from the Course web site, but only if it doesn't exist. Then we read the comma-separated-value format. 


```r
data_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
filename <- "repdata_data_StormData.csv.bz2"

if (!file.exists(filename)){
	download.file(data_url,filename, method="curl")
}

data <- read.csv(bzfile(filename))
```

### Tidying data

The event types are made up of 985 different strings. 


```r
head(sort(unique(data$EVTYPE)),20)
```

```
##  [1]    HIGH SURF ADVISORY   COASTAL FLOOD          FLASH FLOOD          
##  [4]  LIGHTNING              TSTM WIND              TSTM WIND (G45)      
##  [7]  WATERSPOUT             WIND                  ?                     
## [10] ABNORMAL WARMTH        ABNORMALLY DRY         ABNORMALLY WET        
## [13] ACCUMULATED SNOWFALL   AGRICULTURAL FREEZE    APACHE COUNTY         
## [16] ASTRONOMICAL HIGH TIDE ASTRONOMICAL LOW TIDE  AVALANCE              
## [19] AVALANCHE              BEACH EROSIN          
## 985 Levels:    HIGH SURF ADVISORY  COASTAL FLOOD ... WND
```

As we can see, there are many spelling mistakes (for example "AVALANCE" instead of "AVALANCHE"), and unnecessarily detailed classes, so we need to fix that. First we make all values upper case, then change "/" and "&" to "AND" to make it easier on the eyes. We focus on the classes that are the most deadly.


```r
# Make column names lower case
colnames(data) <- colnames(data) %>% tolower()

# Make all categories upper case so that "Early Frost" and "EARLY FROST" is the same category.
# Replace & or / with AND. 
data$evtype <- data$evtype %>% toupper() %>% str_trim() %>% sub("[ ]*[$/][ ]*", " AND ", .)

data$evtype[data$evtype == "AVALANCE"] <- "AVALANCHE"

data$evtype <- data$evtype %>% sub("URBAN/SML STREAM FLDG", "URBAN/SMALL STREAM FLOODING",.)
data$evtype <- data$evtype %>% sub("WIND CHILL", "WINDCHILL",.)

# Group Thunderstorms together

data$evtype <- data$evtype %>% sub("TSTM", "THUNDERSTORM", .)
data$evtype <- data$evtype %>% sub("THUNDERSTORMW", "THUNDERSTORM WINDS", .)
data$evtype <- data$evtype %>% sub("THUDERSTORM", "THUNDERSTORM", .)
data$evtype <- data$evtype %>% sub("TUNDERSTORM", "THUNDERSTORM", .)
data$evtype <- data$evtype %>% sub("TUNDERSTORMS", "THUNDERSTORM", .)

data$evtype <- data$evtype %>% sub("TUNDERSTORM W INDS", "THUNDERSTORM WINDS", .)

data$evtype[grepl("THUNDER.*WIND", data$evtype ) & !grepl("HAIL", data$evtype ) & !grepl("LIGHTNING", data$evtype ) & !grepl(
	"FLOOD",data$evtype) & !grepl("TREE",data$evtype) & !grepl("AWNING", data$evtype)] <- "THUNDERSTORM WINDS"

data$evtype <- data$evtype %>% sub("UNSEASONABLE", "UNSEASONABLY",.)

# Remove names for tropical storms

data$evtype[grepl("TROPICAL STORM", data$evtype)] <- "TROPICAL STORM"

data$evtype <- data$evtype %>% sub("TORNADOES", "TORNADO", .)
data$evtype <- data$evtype %>% sub("TORNDAO", "TORNADO", .)

# Tornado and or waterspout
data$evtype[grepl("^TORNADO F[0-5]", data$evtype)] <- "TORNADO"

data$evtype[grepl("WATER.*SPOUT", data$evtype) &  grepl("TORNADO", data$evtype)] <- "TORNADO/WATERSPOUT"
data$evtype[grepl("WATER.*SPOUT", data$evtype) &  !grepl("TORNADO", data$evtype) & !grepl("DUST",data$evtype)] <- "WATERSPOUT"

# Remove "summary of may 21 " types of events
data <- data[!grepl("^SUMMARY", data$evtype),]

data$evtype[grepl("^HAIL.[0-5]*", data$evtype)] <- "HAIL"
data$evtype[grepl("^HIGH WIND.*[0-5]*", data$evtype)] <- "HIGH WIND"

data$evtype[grepl("^WILD.*FIRE", data$evtype)] <- "WILDFIRE"

data$evtype[data$evtype == "RIP CURRENT"] <- "RIP CURRENTS"
data$evtype[data$evtype %in% c("EXTREME HEAT", "EXCESSIVE HEAT", "HEAT WAVE", "RECORD HEAT")] <- "HEAT"
data$evtype[data$evtype == "DENSE FOG"] <- "FOG"
data$evtype[data$evtype == "STRONG WIND"] <- "WIND"
data$evtype[data$evtype == "EXTREME COLD AND WINDCHILL"] <- "COLD AND WINDCHILL"
data$evtype[data$evtype == "HEAVY SURF"] <- "HIGH SURF"
data$evtype[data$evtype == "URBAN AND SML STREAM FLD"] <- "FLOOD"
data$evtype[data$evtype %in% c("WINTER WEATHER AND MIX", "WINTRY MIX", "WINTRY WEATHER MIX")] <- "WINTER WEATHER"

data$evtype[grepl("FLOOD",data$evtype) & !grepl("THUNDER", data$evtype) &  !grepl("RAIN", data$evtype) & !grepl("WIND", data$evtype) & !grepl("LANDSLIDE", data$evtype)] <- "FLOOD"
```

The event types are now made up of 548 different strings. Would be nicer to have them strings properly capitalized.


```r
# from ?toupper

simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

data$evtype <- data$evtype %>% tolower() %>% sapply(., simpleCap)
```

## Most harmful events with respect to population health

We would like to find the most harmful event types with respect to human health. We sum together injuries and fatalities, so we disregard the seriousness of fatalities versus injuries.


```r
a <- aggregate(data=data, cbind(fatalities,injuries)~EVTYPE,FUN=sum)
```

```
## Error: object 'EVTYPE' not found
```

```r
# Sum fatalities and injuries
a <- transform(a, sum=injuries+fatalities)
```

```
## Error: object 'injuries' not found
```

```r
# Sort by sum
a <- a[order(-a$sum),]
```

```
## Error: invalid argument to unary operator
```

```r
# Remove sum
a$sum <- NULL

# Make the factor reflect the order
a$evtype <- factor(a$evtype, a$evtype)
```

```
## Error: replacement has 0 rows, data has 150
```

```r
# Pick out the 50 worst event types
a <- head(a,50)

# Melt so that we can color after injuries and fatalities
a <- melt(a, id.vars="evtype")
```

```
## Error: id variables not found in data: evtype
```

```r
ggplot(data=a, aes(x = evtype, y = value, fill=variable)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_log10() 
```

```
## Error: object 'evtype' not found
```

# Results

