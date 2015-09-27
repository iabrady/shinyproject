dataset <- read.csv("all_month.csv", head=TRUE, sep=",")
earthquake <- dataset[dataset$type == 'earthquake',]
states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
states_acrn <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
places <- earthquake$place

countries = list()
for (i in 1:length(places)) {
  tokens <- unlist(strsplit(toString(places[i]), ", "))
  country <- tokens[length(tokens)]
  if(country %in% states) {countries[i] <- 'US'}
  else {countries[i] <- tokens[length(tokens)]}
}
earthquake$country <- unlist(countries)
us_states = list()
for (i in 1:length(places)) {
  tokens <- unlist(strsplit(toString(places[i]), ", "))
  state <- tokens[length(tokens)]
  index <- match(state, states)
  if(!is.na(index)) {us_states[i] <- states_acrn[index]}
  else {us_states[i] <- NA}
}
earthquake$us_states <- unlist(us_states)

us_set <- earthquake[earthquake$country == 'US',]
us_set$mag <- as.numeric(us_set$mag)
us_set$time <- as.Date(us_set$time)
us_dataset <- subset(us_set, mag >= 3.0)

mag <- as.numeric(us_dataset$mag)
class <- rep("minor", length(mag))
for (i in 1:length(mag)){
  if (mag[i] >= 4 & mag[i] < 5) {class[i] <- "light"}
  if (mag[i] >= 5 & mag[i] < 6) {class[i] <- "moderate"}
  if (mag[i] >= 6 & mag[i] < 7) {class[i] <- "strong"}
  if (mag[i] >= 7 & mag[i] < 8) {class[i] <- "major"}
  if (mag[i] >= 8) {class[i] <- "great"}
}
us_dataset$class <- class
us_dataset$occurrence = rep(1, length(us_dataset$mag))
df <- data.frame(us_dataset$time, us_dataset$mag, us_dataset$us_states, us_dataset$class, us_dataset$occurrence)
colnames(df) <- c("Date", "Mag", "State", "Class", "Occurrences")
occ <- aggregate(Occurrences ~ State, df, sum)
min_mag <- aggregate(Mag ~ State, df, min)
max_mag <- aggregate(Mag ~ State, df, max)
mean_mag <- aggregate(Mag ~ State, df, mean)
state_stat <- occ
state_stat$MinimalMag <- min_mag$Mag
state_stat$MaximalMag <- max_mag$Mag
state_stat$MeanMag <- mean_mag$Mag

occurrencesByClass <- aggregate(df$Occurrences, list(Date=df$Date, Class=df$Class), sum)
maxMagByClass <- aggregate(df$Mag, list(Date=df$Date, Class=df$Class), max)
minMagByClass <- aggregate(df$Mag, list(Date=df$Date, Class=df$Class), min)
meanMagByClass <- aggregate(df$Mag, list(Date=df$Date, Class=df$Class), mean)
class_stat <- occurrencesByClass
colnames(class_stat) <- c("Date", "Class", "Occurrences")
class_stat$MinimalMag <- minMagByClass$x
class_stat$MaximalMag <- maxMagByClass$x
class_stat$MeanMag <- meanMagByClass$x

summaryByState <- function(dt, state){
  if (state == "ALL"){
    Occurrences <- c(sum(dt$Occurrences))
    MinimalMag <- c(min(dt$MinimalMag))
    MaximalMag <- c(max(dt$MaximalMag))
    MeanMag <- c(mean(dt$MeanMag))
    rs <- data.frame(Scope=c("All"), Occurrences=Occurrences, MinimalMag=MinimalMag, MaximalMag=MaximalMag, MeanMag=MeanMag)
  } else {
    rs <- dt[dt$State == state,]
  }
  rs
}

usTable <- function() {
  us_set
}