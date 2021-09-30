
# REQUIRED LIBRARIES

library(dplyr);
library(ggplot2);
library(mclust);
library(lubridate);
library(mapdata);
library(maps);
library(plotly)

# READ DATA

dataUfo <- read.csv(file="data/scrubbed.csv", header=T);

dataUfoClear <- dataUfo %>%
  dplyr::select(latitude, longitude, shape, country,
                datetime, date.posted, city, state,
                duration = duration..seconds.);

# DATA CLEAN

dataUfoClear$latitude <- as.numeric(as.character(dataUfoClear$latitude));
dataUfoClear$longitude <- as.numeric(as.character(dataUfoClear$longitude));
dataUfoClear$country <- as.factor(dataUfo$country);
dataUfoClear$datetime <- mdy_hm(dataUfoClear$datetime);
dataUfoClear$date.posted <- mdy(dataUfoClear$date.posted);
dataUfoClear$duration <- as.numeric(as.character(dataUfoClear$duration));

# DATA USA

dataUfoClear <- na.omit(dataUfoClear);
dataUfoUSA <- filter(dataUfoClear, country=="us" & !(state %in% c("ak", "hi", "pr")));

head(dataUfo);
head(dataUfoClear)

# COUNTRY MOST UFO SIGHTINGS

levels(dataUfoClear$country) <- c("Rest of the world", "Australia", "Canada", "Germany", "Great Britain", "United States");
ggplot(dataUfoClear, aes(x=reorder(country, country, FUN=length), fill=country)) +
  stat_count() + 
  theme_bw() + 
  scale_fill_brewer(palette="Dark2") +
  labs(x = "Country", y = "Number of sightings", 
     title="Most UFO sightings by Country", 
     subtitle = "United States and Rest of the world")



# HOW DENSE SIGHTINGS AROUND THE WORLD

ggplot(dataUfoClear, aes(x=longitude, y=latitude, colour=country)) + 
  borders("world", colour="gray", fill="seashell3") +
  geom_point(shape=15) +
  theme_bw() + 
  labs(x = "Longitude", y = "Latitude", 
       title="Map UFO sightings around the world", 
       subtitle = "United States and Rest of the world")

# SIGHTINGS IN UNITED STATES

ggplot(dataUfoUSA, aes(x=reorder(state, state, FUN=length), fill=state)) + 
  stat_count() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, size=9, hjust=1)) + 
  labs(x = "State", y = "Number of sightings", 
       title="UFO sightings in United States", 
       subtitle = "Sightings by state")

# MAP SIGHTINGS IN UNITED STATES

ggplot(dataUfoUSA, aes(x=longitude, y=latitude, colour=state)) + 
  geom_point(size=1,stroke=0, shape=15) +
  borders("state", colour="gray", fill="seashell3") +
  geom_point(size=0.5,stroke=0, shape=15) +
  theme_bw() + 
  labs(x = "Longitude", y = "Latitude", 
       title="Map UFO sightings in United States", 
       subtitle = "Sightings by state")

# MOST UFO SHAPES THAT APPEAR

ggplot(dataUfoClear, aes(x=reorder(shape, shape, FUN=length), fill=shape)) + 
  geom_bar(show.legend=F) +
  coord_flip() +
  theme_bw() + 
  labs(x = "Shape", y = "Number of sightings", 
       title="Most typical UFO shapes that appear", 
       subtitle = "UFO shapes seen around the world")

# CORRELATION BETWEEN THE TIME AND SIGHTINGS

ggplot(dataUfoClear, aes(x=hour(datetime))) + 
  geom_histogram(bins=24, aes(fill=..count..)) +
  theme_bw() + 
  scale_fill_gradient(low = "palegreen", high = "palegreen4") +
  labs(x = "Hour of the day", y = "Number of sightings", 
       title="Correlation between daytime / UFO sightings", 
       subtitle = "Sightings during the day")

# CORRELATION BETWEEN THE TIME AND UFO SHAPE

shapesDaytime <- 
  dataUfoClear %>% 
  group_by(hour=hour(datetime), shape, duration) %>% 
  summarize(count=n());
  
ggplot(shapesDaytime, aes(x=hour, y=shape)) + 
  geom_point(aes(color=count, size=count)) + 
  scale_colour_gradient(low = "palegreen", high="palegreen4") +
  labs(x = "Hour of the day", y = "UFO Shape", 
       title="Correlation between daytime / UFO Shape", 
       subtitle = "Sightings during the day")

chisq.test(dataUfoClear$shape, hour(dataUfoClear$datetime), simulate.p.value=T);


# SIGHTINGS BY YEAR

sightingsYear <- 
  dataUfoClear %>% group_by(year=year(datetime)) %>% 
  summarize(count=n());

# REPORTS BY YEAR

reportsYear <- 
  dataUfoClear %>% group_by(year=year(date.posted)) %>% 
  summarize(count=n());

ggplot(sightingsYear, aes(x=year, y=count)) + 
  geom_line(size=1, colour="palegreen4") + 
  geom_line(data=reportsYear, aes(y=count), size=1, colour="red") + 
  geom_smooth(method="lm") +
  labs(x = "Year", y = "red = reports, green = sightings", 
       title="UFO sightings / UFO reports by year", 
       subtitle = "Sightings last century")

