

library(tidyverse)
library(janitor)
library(lubridate)
library(cowplot)


no_shadeB <- read_csv("data-raw/logger40-20C-2018-12-02_A5026B_no_shade.csv", skip = 1) %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(time)) %>% 
	rename(temperature = number_1_o_c) %>% 
	mutate(day = day(date_time)) %>% 
	mutate(month = month(date_time)) %>% 
	unite(month, day, col = "month_day") %>% 
	mutate(date = date(date_time)) %>% 
	mutate(hour = hour(date_time)) %>% 
	mutate(shade = "no") %>% 
	filter(date > ymd("2018-12-01")) %>% 
	mutate(logger_ID = "B")

no_shade7 <- read_csv("data-raw/logger40-20C-2018-12-02_A50267_no_shade.csv", skip = 1) %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(time)) %>% 
	rename(temperature = number_1_o_c) %>% 
	mutate(day = day(date_time)) %>% 
	mutate(month = month(date_time)) %>% 
	unite(month, day, col = "month_day") %>% 
	mutate(date = date(date_time)) %>% 
	mutate(hour = hour(date_time)) %>% 
	mutate(shade = "no") %>% 
	filter(date > ymd("2018-12-01")) %>% 
	mutate(logger_ID = "7")


no_shadeB %>%
	mutate(daytime = NA) %>% 
	mutate(daytime = ifelse(hour > 9 & hour < 15.5, "night", "day")) %>% 
	filter(temperature < 20) %>% 
	# filter(date > ymd("2018-11-28")) %>% 
	ggplot(aes(x = date_time, y = temperature)) + geom_point() 

shadeB <- read_csv("data-raw/logger40-20C-2018-12-03_A5026B_shade.csv", skip = 1) %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(time)) %>% 
	rename(temperature = number_1_o_c) %>% 
	mutate(day = day(date_time)) %>% 
	mutate(month = month(date_time)) %>% 
	unite(month, day, col = "month_day") %>% 
	mutate(date = date(date_time)) %>% 
	mutate(hour = hour(date_time)) %>% 
	mutate(shade = "yes") %>% 
	filter(date > ymd("2018-12-02")) 


shadeB %>%
	mutate(daytime = NA) %>% 
	mutate(daytime = ifelse(hour > 9 & hour < 15.5, "night", "day")) %>% 
	filter(temperature < 20) %>% 
	# filter(date > ymd("2018-11-28")) %>% 
	ggplot(aes(x = date_time, y = temperature)) + geom_point() 



all_B_logger <- bind_rows(no_shadeB, shadeB)


all_B_logger %>% 
	filter(temperature < 20) %>% 
	ggplot(aes(x = date_time, y = temperature, color = shade)) + geom_point()


both_no_shades <- bind_rows(no_shade7, no_shadeB)


both_no_shades %>% 
	filter(temperature < 20) %>% 
	ggplot(aes(x = date_time, y = temperature, color = logger_ID)) + geom_line()
