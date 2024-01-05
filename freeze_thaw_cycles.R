library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# A few variables
nn <- 4 # Number of continuous hours
na_hours <- 96

# Load the data
dat <- readRDS("met1991_2023.Rds")

# Identify continuous hourly gaps
dates.all <- seq(min(dat$Date_EST), max(dat$Date_EST), by = "1 hour")
dat_complete <- dat %>% 
  select(Date_EST, Temp) %>% 
  complete(Date_EST = dates.all,
           fill = list(Temp = NA)) %>% 
  mutate(year = year(Date_EST), 
         month = month(Date_EST, label = TRUE),
         day = day(Date_EST),
         hour = hour(Date_EST)) 

# Identify all months with at least 4 days worth of missing values
months_na <- dat_complete %>% 
  group_by(year, month) %>% 
  summarise(missing = sum(is.na(Temp))) %>% 
  filter(missing > na_hours)

# Count the number of times temperatures have flipped
# (4 continuous hours below freezing followed by 4 continuous
# hours above freezing, etc ...)
dat_flip <- dat_complete %>% 
  mutate(bin = ifelse(Temp >= 32, 1, 0)) %>% 
  group_by(year , month ) %>% 
  reframe(rl = list(rle(bin)),
          bin = rl[[1]]$values,
          cnt = rl[[1]]$lengths) %>% 
  ungroup() %>% 
  select(-rl) %>% 
  mutate(cnt_lag = lag(cnt),
         bin_lag = lag(bin),
         flip = case_when(cnt >= nn & cnt_lag >= nn & bin != bin_lag ~ TRUE,
                          TRUE ~ FALSE)) %>% 
  group_by(year, month) %>% 
  summarize(flip= sum(flip)) %>% 
  filter(flip !=0)

# Remove all months with at least na_hours worth of missing values
dat_flip_no_na <- dat_flip %>% 
  anti_join(months_na, by=c("year","month"))

# Plot the trends
ggplot() +
  geom_point(data= dat_flip_no_na, aes(x = year, y = flip)) + 
  geom_smooth(data= dat_flip_no_na, aes(x = year, y = flip),
              method = loess, se = FALSE, 
              method.args=list(family="symmetric", degree = 1)) +
  geom_rect(data = months_na, aes(xmin=year-0.5 , xmax =year+0.5, ymin=-Inf, ymax=Inf), 
            fill = "grey", alpha = 0.3) +
  xlab("year") + ylab("count") +
  facet_wrap(~month, nrow = 3, drop = FALSE) +
  ggtitle("Freeze-thaw cycle count") +
  theme(plot.title = element_text(color = "grey40"))