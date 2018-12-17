
## load packages
library(tidyverse)
library(cowplot)
library(readxl)
library(janitor)
library(lubridate)



temp_files <- c(list.files("data-raw/ICEWATERTEST", full.names = TRUE))

temp_files <- temp_files[grepl(".csv", temp_files)]

names(temp_files) <- temp_files %>% 
	gsub(pattern = ".csv$", replacement = "")


logger_109 <- read_csv("data-raw/ICEWATERTEST/AN109_ICEWATERTEST.csv", skip = 1)

all_temps <- map_df(temp_files, read_csv, .id = "file_name", skip = 2, col_names = FALSE) %>% 
	clean_names() %>% 
	select(1:4)


all_temps2$date_time[[3]]

all_temps2 <- all_temps %>% 
	separate(file_name, into = c("path", "logger"), sep = c("AN")) %>% 
	separate(logger, into = c("logger_id", "other"), sep = "_") %>% 
	select(-x1) %>% 
	rename(date_time = x2) %>% 
	rename(temperature = x3) %>% 
	filter(!is.na(temperature)) %>% 
	mutate(date_time = mdy_hms(date_time)) %>% 
	filter(date_time > ymd_hms("2018-12-13 10:55:00")) %>% 
	filter(date_time < ymd_hms("2018-12-13 11:30:00")) 


all_temps2 %>% 
	ggplot(aes(x = date_time, y = temperature, color = logger_id)) + geom_line() +
	ylab("Temperature (°C)") + xlab("Time") 


temps_sum <- all_temps2 %>% 
	group_by(logger_id) %>% 
	summarise(mean_temp = mean(temperature))


### now filter test

temp_files <- c(list.files("data-raw/FILTERTEST", full.names = TRUE))

temp_files <- temp_files[grepl(".csv", temp_files)]

names(temp_files) <- temp_files %>% 
	gsub(pattern = ".csv$", replacement = "")

all_temps <- map_df(temp_files, read_csv, .id = "file_name", skip = 2, col_names = FALSE) %>% 
	clean_names() %>% 
	select(1:4)

all_temps3 <- all_temps %>% 
	separate(file_name, into = c("path", "logger"), sep = c("AN")) %>% 
	separate(logger, into = c("logger_id", "filter"), sep = 3) %>% 
	select(-x1) %>% 
	rename(date_time = x2) %>% 
	rename(temperature = x3) %>% 
	filter(!is.na(temperature)) %>% 
	mutate(date_time = mdy_hms(date_time)) %>% 
	filter(date_time > ymd_hms("2018-12-13 15:55:00"))%>% 
	filter(date_time < ymd_hms("2018-12-13 16:20:00")) 


all_temps3 %>% 
	ggplot(aes(x = date_time, y = temperature, color = filter)) + geom_line(size = 3) +
	ylab("Temperature (°C)") + xlab("Time")

all_temps4 <- left_join(all_temps3, temps_sum, by = "logger_id") %>% 
	mutate(temp_corrected = temperature - mean_temp)

all_temps4 %>% 
	ggplot(aes(x = date_time, y = temp_corrected, color = filter)) + geom_line(size = 3) +
	ylab("Temperature (°C)") + xlab("Time")

all_temps4 %>% 
	ggplot(aes(x = date_time, y = temperature, color = filter)) + geom_line(size = 3) +
	ylab("Temperature (°C)") + xlab("Time")

all_temps4 %>% 
	group_by(filter, logger_id) %>% 
	summarise(mean_temperature = mean(temp_corrected)) %>% 
	ggplot(aes(x = filter, y = mean_temperature, color = filter)) + geom_point(size = 3) +
	ylab("Temperature (°C)") + xlab("Time") +
	theme(axis.text.x = element_text(angle = 90, hjust = 1))


