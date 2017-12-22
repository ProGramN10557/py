library(hflights)
data(hflights)
install.packages("hflights")
library(hflights)
head(hflights)
tail(hflights)
flights<-tbl_df(hflights) # local table
str(hflights)
summary(hflights)
flights
data.frame(head(flights)) # check all columns
flights %>%
  filter(Month==1,DayofMonth==1) # AND condition

flights %>%
  filter(UniqueCarrier=="AA" | UniqueCarrier=="UA")

flights %>%
  select(DepTime,ArrTime,FlightNum)

flights %>%
  select(UniqueCarrier,DepDelay) %>%
  filter(DepDelay>60)

flights %>%
  select(UniqueCarrier,DepDelay) %>%
  arrange(DepDelay)

flights %>%
  select(UniqueCarrier,DepDelay) %>%
  arrange(desc(DepDelay))

flights %>%
  select(Distance,AirTime) %>%
  mutate(Speed=Distance/AirTime*60)
  
flights <- flights %>% mutate(Speed=Distance/AirTime*60) # store the new variable

flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE))

flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(mean), Cancelled, Diverted)

glimpse(flights)
flights %>%
  group_by(Month,DayofMonth) %>%
  summarise(flight_count=n()) %>%
  arrange(desc(flight_count))

flights %>%
  group_by(Dest) %>%
  summarise(flight_count = n(), plane_count = n_distinct(TailNum))
            



































