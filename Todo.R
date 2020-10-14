# Code to add to the book 

### Joining tables
agg <- dat %>%
  group_by(activity_recoded,deviceId) %>%
  summarise(total_distance=sum(distance,na.rm = T),
            avgPower = mean(avgPower,na.rm=T))
dat %>%
  select(distance,activityId,activity_recoded,deviceId) %>%
  left_join(agg,by=c("activity_recoded","deviceId")) %>%
  mutate(distance_pct = 100*distance/total_distance) %>%
  head()