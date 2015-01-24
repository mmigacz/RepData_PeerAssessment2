---
title: "Economic and public health impact of storms in the USA, 1950 - 2011"
author: "Maciej Migacz"
date: "24 January 2015"
output: html_document
---

#Synopsis

The basic goal of this report is to explore the [U.S. National Oceanic and Atmospheric Administration’s (NOAA) storm database](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) to answer the following questions:

1. Across the United States, which types of events are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?

Preliminary analysis of the source data showed that the data are relatively of poor quality. The problem is particularly ambiguous names of event types, which are not organized in consistent taxonomy. The basic preprocessing reduces the impact of these issues.  

The report shows that tornados, heats and floods are the leading cause of fatalities and injuries. Floods, hurricanes and tornados are by far the greatest causes of economic damage, including crops and property.

The report bases on the following data:

- [U.S. NOAA storm database](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)
- National Weather Service [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)
- National Climatic Data Center Storm Events [FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)


#Data Processing
Initial preparations - loading required packages, loading data. Data processing and analysis use **dplyr**, diagrams are prepared with **ggplot2** package.


```r
# include required libraries
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
```

##Data load

The analysis starts from downloading required data set, and loading it into memory. *Note* the download and read are slow and are cached for subsequent reuse. 

```r
srcF <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
dstF <- "repdata-data-StormData.csv.bz2"
if(!file.exists(dstF)){
  download.file(srcF, destfile = dstF, method = "curl", mode="wb")
}
data <- read.csv(dstF)
```

##Subseting and cleaning data


The data source contains **902297** events, categorized into **985** distinct types.

At the first step, we select from the initial data set only these columns, which are necessery to this analysis:

- EVTYPE - the type of event being reported
- FATALITIES, INJURIES - direct fatalities and injuries arising from the event
- PROPDMG, PROPDMGEXP - property damage arising from the event
- CROPDMG, CROPDMGEXP - crop damage arising from the event


```r
# Select columns, which are used in the analysis
subset <- select(data, EVTYPE, FATALITIES:CROPDMGEXP)
```

The real number of distinct event types is much lower than **985**. Inspection of the reported event types shows numerous inconsistencies, such as using adjectives, shortcuts and synonyms of the same name. Therefore we prepared a set of rules to remove ambiguities and reduce the set of event types.


```r
# Upper case all event type names, remove puctuations and digits
subset$EVTYPE <- toupper(subset$EVTYPE) 
subset$EVTYPE <- gsub("[[:punct:]]", "", subset$EVTYPE)
subset$EVTYPE <- gsub("[[:digit:]]", "", subset$EVTYPE)

# Unify THUNDERSTORM naming
subset$EVTYPE <- gsub("^THU.*", "THUNDERSTORM", subset$EVTYPE) 
subset$EVTYPE <- gsub(".*TSTM.*", "THUNDERSTORM", subset$EVTYPE)

# Unify HEIL naming
subset$EVTYPE <- gsub(".*HEIL.*", "HEIL", subset$EVTYPE)

# Unify SNOW naming
subset$EVTYPE <- gsub(".*SNOW.*", "SNOW", subset$EVTYPE) 
subset$EVTYPE <- gsub(".*BLIZZARD.*", "SNOW", subset$EVTYPE) 

# Unify HEAT naming
subset$EVTYPE <- gsub(".*HEAT.*", "HEAT", subset$EVTYPE) 
subset$EVTYPE <- gsub(".*WARM.*", "HEAT", subset$EVTYPE) 
subset$EVTYPE <- gsub(".*HOT.*", "HEAT", subset$EVTYPE)

# Unify COLD naming
subset$EVTYPE <- gsub(".*COOL.*", "COLD", subset$EVTYPE) 
subset$EVTYPE <- gsub(".*COLD.*", "COLD", subset$EVTYPE) 

# Unify WIND naming
subset$EVTYPE <- gsub(".*WIND.*", "WIND", subset$EVTYPE) 

# Unify FLOOD naming
subset$EVTYPE <- gsub("^FLOOD.*", "FLOOD", subset$EVTYPE) 
subset$EVTYPE <- gsub(".*FLOOD.*", "FLOOD", subset$EVTYPE) 

# Unify hurrican/typhoon/tropical naming
subset$EVTYPE <- gsub("^HURR.*", "HURRICANE", subset$EVTYPE) 
subset$EVTYPE <- gsub("^TYPHOON.*", "TYPHOON", subset$EVTYPE) 
subset$EVTYPE <- gsub("^TROPICALS.*", "TROPICAL STORM", subset$EVTYPE)

# Replace chains of whitespaces, trim leading and trailing whitespaces
subset$EVTYPE <- gsub("\\s+", " ", subset$EVTYPE)
subset$EVTYPE <- gsub("^\\s+|\\s+$", "", subset$EVTYPE)
```



The above rules reduce number of distinct event types to **985**. This data cleansing is far from complete and would lead to potential errors in the ranking of event types with few occurrences. However, we applied corrections to the most often occured event types and now their quality seems to be acceptable to answer the questions.

## Prepare data for health impact analysis

To find most harmful types of events with respect to population health we do the following:

- filter events without fatalities to reduce the number of distinct event types reported in the data
- group events by event types
- summarize events to determine the total number of fatalities and injuries of each event type
- sort data descending order of fatalities and injuries
- take the top 5 event types with the highest fatalities and injuries numbers


```r
impactHealth <- subset %>% 
    filter(FATALITIES > 0) %>%
    group_by(EVTYPE) %>% 
    summarize(fatalities = sum(FATALITIES), injuries = sum(INJURIES)) %>% 
    arrange(desc(fatalities), desc(injuries))

impactHealthTop <- head(impactHealth, 5)
```

## Prepare data for economic impact analysis

To determine the events that have the greatest economic impact, we execute the following sequence:

- select EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP columns
- calculate property demage and add it as a new column **propDamage**, as PROPDMG x decoded magnitude
- calculate crop demage and add it as a new column **cropDamage**, as CROPDMG x decoded magnitude
- sum propDamage and cropDamage to calculate **totalDamage**
- select calculated columns
- group rows by event type, and summarise each damage value column
- sort results descending by totalDamage
- take the top 5 event types causing the highest damage costs


```r
impactEconomic <- subset %>% 
  select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>%
  mutate(propDamage = PROPDMG * ifelse(PROPDMGEXP == "K", 1000, 
                                           ifelse(PROPDMGEXP == "M", 1000000, 
                                                  ifelse(PROPDMGEXP == "B", 1000000000, 0))), 
         cropDamage = CROPDMG * ifelse(CROPDMGEXP == "K", 1000, 
                                           ifelse(CROPDMGEXP == "M", 1000000, 
                                                  ifelse(CROPDMGEXP == "B", 1000000000, 0))),
         totalDamage = propDamage + cropDamage
         ) %>%
  select(EVTYPE, propDamage, cropDamage, totalDamage) %>%
  group_by(EVTYPE) %>% 
  summarise_each(funs(sum)) %>%
  arrange(desc(totalDamage))


impactEconomicTop <- head(impactEconomic, 5)
```

#Results

## Impact of storms events on public health

The first 10 event types with respect to fatalities and injuries

```r
head(impactHealth, 10)
```

```
## Source: local data frame [10 x 3]
## 
##          EVTYPE fatalities injuries
## 1       TORNADO       5633    60187
## 2          HEAT       3178     6495
## 3         FLOOD       1525     3326
## 4     LIGHTNING        817      649
## 5  THUNDERSTORM        746      835
## 6          WIND        481      476
## 7          COLD        436      265
## 8   RIP CURRENT        368       87
## 9          SNOW        269      974
## 10    AVALANCHE        224       79
```
Tornados, heat and flood related events cause the largest number both of deaths and injuries during the period of studies 

The top five causes of fatality are shown graphically in the following figure


```r
ihf <- rbind(
  impactHealthTop %>% select(EVTYPE, COUNTER = fatalities) %>% mutate(IMPACT = "fatalities"),
  impactHealthTop %>% select(EVTYPE, COUNTER = injuries) %>% mutate(IMPACT = "injuries")
  )
ihf$EVTYPE <- factor(ihf$EVTYPE, levels=unique(ihf$EVTYPE))

ggplot(data = ihf, aes(x = EVTYPE, y=COUNTER, fill = IMPACT)) + 
  geom_bar(stat="identity") + 
  labs(title = "Top 5 most harmful storm event types, 1950-2011") +
  labs(x = "Storm event type", y = "Number of fatalities and injuries")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

## Economic impact of storms

The first 10 event types with respect to total crop and property demage.

```r
head(impactEconomic, 10)
```

```
## Source: local data frame [10 x 4]
## 
##            EVTYPE   propDamage  cropDamage  totalDamage
## 1           FLOOD 167518165320 12267249100 179785414420
## 2       HURRICANE  84636180010  5495292800  90131472810
## 3         TORNADO  56925660480   414953110  57340613590
## 4     STORM SURGE  43323536000        5000  43323541000
## 5            HAIL  15727918720  3025627450  18753546170
## 6         DROUGHT   1046106000 13972566000  15018672000
## 7    THUNDERSTORM  11365850630  1227959900  12593810530
## 8       ICE STORM   3944927810  5022113500   8967041310
## 9  TROPICAL STORM   7703890550   678346000   8382236550
## 10           WIND   6250644390   924720550   7175364940
```
Floods, hurricanes and tornados related events cause are the most costly in economic terms. Floods and tornados are also present in the top of health imact rank.


The top five event types with the highest economic impact are shown graphically in the following figure:


```r
impactEconomicTop$EVTYPE <- factor(impactEconomicTop$EVTYPE, levels=unique(impactEconomicTop$EVTYPE))

ggplot(data = impactEconomicTop, aes(x = EVTYPE, y=totalDamage * 1e-9)) + 
  geom_bar(stat="identity", colour="red", fill="red") + 
  labs(title = "Top 5 economical damage storm event types, 1950-2011") +
  labs(x = "Storm event type", y = "Total costs of damage (billion $)")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
