# Severe weather effects on health and economy

## Data processing
```{r}
library(ggplot2)
library(plyr)
library(stringr)
library(magrittr)

data_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
filename <- "repdata_data_StormData.csv.bz2"

if (!file.exists(filename)){
	download.file(data_url,filename, method="curl")
}

cat("Reading data... ")
data <- read.csv(bzfile(filename))

cat("Finished reading data.")

# Many different levels
length(unique(data$EVTYPE))

# Make all categories upper case so that "Early Frost" and "EARLY FROST" is the same category.
# Replace & or / with AND
data$EVTYPE <- data$EVTYPE %>% toupper() %>% str_trim() %>% sub("[ ]*[$/][ ]*", " AND ", .)

# Group Thunderstorms together
unique(data$EVTYPE [grepl("THUNDER.*WIND", data$EVTYPE ) & !grepl("HAIL", data$EVTYPE ) & !grepl("LIGHTNING", data$EVTYPE ) & !grepl("FLOOD",data$EVTYPE) & !grepl("TREE",data$EVTYPE) & !grepl("AWNING", data$EVTYPE)])

data$EVTYPE <- data$EVTYPE %>% sub("URBAN/SML STREAM FLDG", "URBAN/SMALL STREAM FLOODING",.)
data$EVTYPE <- data$EVTYPE %>% sub("WIND CHILL", "WINDCHILL",.)

data$EVTYPE <- data$EVTYPE %>% sub("TSTM", "THUNDERSTORM", .)
data$EVTYPE <- data$EVTYPE %>% sub("THUNDERSTORMW", "THUNDERSTORM WINDS", .)
data$EVTYPE <- data$EVTYPE %>% sub("THUDERSTORM", "THUNDERSTORM", .)
data$EVTYPE <- data$EVTYPE %>% sub("TUNDERSTORM", "THUNDERSTORM", .)
data$EVTYPE <- data$EVTYPE %>% sub("TUNDERSTORMS", "THUNDERSTORM", .)

data$EVTYPE <- data$EVTYPE %>% sub("TUNDERSTORM W INDS", "THUNDERSTORM WINDS", .)

data$EVTYPE[grepl("THUNDER.*WIND", data$EVTYPE ) & !grepl("HAIL", data$EVTYPE ) & !grepl("LIGHTNING", data$EVTYPE ) & !grepl(
	"FLOOD",data$EVTYPE) & !grepl("TREE",data$EVTYPE) & !grepl("AWNING", data$EVTYPE)] <- "THUNDERSTORM WINDS"

data$EVTYPE <- data$EVTYPE %>% sub("UNSEASONABLE", "UNSEASONABLY",.)

# Remove names for tropical storms

data$EVTYPE[grepl("TROPICAL STORM", data$EVTYPE)] <- "TROPICAL STORM"

data$EVTYPE <- data$EVTYPE %>% sub("TORNADOES", "TORNADO", .)
data$EVTYPE <- data$EVTYPE %>% sub("TORNDAO", "TORNADO", .)

# Tornado and or waterspout
data$EVTYPE[grepl("^TORNADO F[0-5]", data$EVTYPE)] <- "TORNADO"

data$EVTYPE[grepl("WATER.*SPOUT", data$EVTYPE) &  grepl("TORNADO", data$EVTYPE)] <- "TORNADO/WATERSPOUT"
data$EVTYPE[grepl("WATER.*SPOUT", data$EVTYPE) &  !grepl("TORNADO", data$EVTYPE) & !grepl("DUST",data$EVTYPE)] <- "WATERSPOUT"

# Remove "summary of may 21 " types of events
data <- data[!grepl("^SUMMARY", data$EVTYPE),]

data$EVTYPE[grepl("^HAIL.[0-5]*", data$EVTYPE)] <- "HAIL"
data$EVTYPE[grepl("^HIGH WIND.*[0-5]*", data$EVTYPE)] <- "HIGH WIND"

data$EVTYPE[grepl("^WILD.*FIRE", data$EVTYPE)] <- "WILDFIRE"

data$EVTYPE[data$EVTYPE == "RIP CURRENT"] <- "RIP CURRENTS"
data$EVTYPE[data$EVTYPE %in% c("EXTREME HEAT", "EXCESSIVE HEAT", "HEAT WAVE", "RECORD HEAT")] <- "HEAT"
data$EVTYPE[data$EVTYPE == "DENSE FOG"] <- "FOG"
data$EVTYPE[data$EVTYPE == "STRONG WIND"] <- "WIND"
data$EVTYPE[data$EVTYPE == "EXTREME COLD AND WINDCHILL"] <- "COLD AND WINDCHILL"
data$EVTYPE[data$EVTYPE == "HEAVY SURF"] <- "HIGH SURF"
data$EVTYPE[data$EVTYPE == "URBAN AND SML STREAM FLD"] <- "FLOOD"
data$EVTYPE[data$EVTYPE %in% c("WINTER WEATHER AND MIX", "WINTRY MIX", "WINTRY WEATHER MIX")] <- "WINTER WEATHER"

data$EVTYPE[grepl("FLOOD",data$EVTYPE) & !grepl("THUNDER", data$EVTYPE) &  !grepl("RAIN", data$EVTYPE) & !grepl("WIND", data$EVTYPE) & !grepl("LANDSLIDE", data$EVTYPE)] <- "FLOOD"

# Got it down to 800

length(unique(data$EVTYPE))

```

```{r}
a <- aggregate(data=data, FATALITIES+INJURIES~EVTYPE,FUN=sum)
a <- a[order(-a[,"FATALITIES + INJURIES"]),]

colnames(a) <- c("Event_type", "Injured_Or_Killed")

b <- head(a[order(-a$Injured_Or_Killed),],50)
b$Event_type <- factor(b$Event_type, b$Event_type)
ggplot(data=b, aes(x = Event_type, y = Injured_Or_Killed)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_log10() 
```

