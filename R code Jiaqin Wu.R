# R Code #
# Jiaqin Wu #


# Task 1 #
library(tidyverse)
library(dplyr)

date1 <- x <- data.frame(cbind(c("Costco", "Walt Disney World", "Universal Orlando", "Apple",
"Sam's Club", "Publix", "Starbucks", " CVS", "Kohl's", "Home Depot", "Target", "Trade Joe's", 
"PCC", "Walmart", "Best Buy", "Macy's", "Whole Foods", "Kroger", "Walgreens", "Lowe's", 
"Dollar General", "Nevada Gaming Control Board", "Office Depot", "McDonald's", "Aldi"),
c("2021-05-14", "2021-05-14", "2021-05-14", "2021-05-14", "2021-05-14", "2021-05-15", 
  "2021-05-17", "2021-05-17", "2021-05-17", "2021-05-17", "2021-05-17", "2021-05-17", 
  "2021-05-17", "2021-05-18", "2021-05-18", "2021-05-18", "2021-05-19", "2021-05-19", 
  "2021-05-19", "2021-05-19", "2021-05-19", "2021-05-20", "2021-05-20", "2021-05-21", 
  "2021-05-26")))

# rename columns in the dataframe
date1 <- date1 %>%
  rename("company name" = X1) %>%
  rename("date of reversal" = X2)

class(date1$`date of reversal`)
date1 <- date1 %>%
  mutate(date of reversal = ymd(date of reversal))

# Output the CSV
write_csv(date1, "25 major US companies.csv")


# Task 2 #
library(ggplot2)

#read retail data
retail_data <- read_csv("Desktop/IPAL/capstone/retail_data_export_w_fips.csv")

#data transformation and cleanup
retail_data_average <- retail_data %>%
  group_by(dayofmonth) %>%
  summarize(aft=mean(daily_visitors,na.rm=T)) %>%
  filter(dayofmonth %in% c(4:31))

#make a time series plot
retail_data_average %>%
  ggplot(aes(x=dayofmonth,y=aft))+
  geom_point(size=2,shape=10,color="black")+
  geom_line(color="red")+
  geom_vline(aes(xintercept=18),color="black")+
  annotate("text",x=18,y=175,label="Policy Reverse on May 18",color="blue")+
  labs(
    title="Average Foot Traffic(By day) in Walmart Stores(May 4-May 31, 2021)",
    x="Day of Month (May 2021)",
    y="Average Foot Traffic (By day)",
  )+
  theme(panel.background = element_rect(fill="grey"))


# Task 3 #
install.packages("sf")
install.packages("rgdal")
install.packages("cdlTools")
install.packages("lubridate")
library(sf)
library(rgdal)
library(cdlTools)
library(lubridate)

#read more data
unzip("Desktop/IPAL/capstone/tl_2016_us_county.zip")
county_data <- st_read("Desktop/IPAL/capstone/tl_2016_us_county/tl_2016_us_county.shp")

# Make retail data into shape file with geometry
retail_data <- retail_data %>%
  rename(longitude=INTPTLAT,latitude=INTPTLON)

retail_data_geom <- st_as_sf(
  retail_data,
  coords = c("longitude", "latitude"),
  crs = 4326)

#set the crs of county_data
county_data <- st_transform(county_data, 4326)

#Join retail_data and county_data by geometry
retail_merged <- st_join(
  retail_data_geom,  
  county_data,     
  join = st_within  
)

#convert fips to state name
retail_merged <- retail_merged %>%
  mutate(statename = fips(retail_merged$STATEFP.x, to='Name'))

#data transformation and cleanup
retail_data_average_state <- retail_merged %>%
  select(daily_visitors,dayofmonth,statename)
retail_data_average_state <- aggregate(retail_data_average_state,
                                       by=list(retail_data_average_state$statename,
                                               retail_data_average_state$dayofmonth), 
                                       FUN=mean, na.rm=TRUE)
retail_data_average_state <- retail_data_average_state %>%
  rename(state=Group.1)

#make a time series plot
retail_data_average_state %>%
  ggplot(aes(x=dayofmonth,y=daily_visitors,color=state))+
  geom_line()+
  labs(
    title="Average Foot Traffic of Walmart for each state(May 4-May 31,2021)",
    x="Day of Month (May 2021)",
    y="Average Foot Traffic (By day)",
    color="STATE"
  )+
  geom_vline(aes(xintercept=18),color="black")+
  annotate("text",x=18,y=270,label="Policy Reverse on May 18",color="blue")+
  theme(panel.background = element_rect(fill="grey"),
        plot.title = element_text(size=9),
        legend.position = "bottom",
        legend.key.size = unit(12,"pt"))


# Task 4 #
retail_data <- read_csv("Desktop/IPAL/capstone/retail_data_export_w_fips.csv")

# Create new dummy variable (0 before 5.18, 1 after 5.18)
post <- (data$dayofmonth>17)*1
retail_merged<-cbind(data,post)
retail_merged
# Run the Linear Regression #
regression1 <- lm(daily_visitors~post,data=retail_merged)
summary(regression1)


# Task 5 #
# read additional data and combine it
week26 <- read.csv("Desktop/IPAL/capstone/county_week26_data_fixed.csv",header=TRUE)
week26 <- week26 %>%
  rename(fips=FIPS Code)
vote2020 <- read.csv("Desktop/IPAL/capstone/vote2020.csv",header=TRUE)
vote2020 <- vote2020 %>%
  rename(fips=county_fips)
retail_merged <- retail_merged %>%
  rename(fips=fips.x)

retail_merged_1 <- left_join(retail_merged,vote2020,by="fips")
retail_merged_1 <- left_join(retail_merged_1,week26,by="fips")

#transform and clean up data
retail_merged_2 <- retail_merged_1 %>%
  mutate("trump_vs" = per_gop*100)%>%
  mutate("vaccine_hes" = 'Estimated hesitant'*100)%>%
  mutate("dem_vs" = per_dem*100)%>%

# Run the Linear Regression #
model1 <- lm(daily_visitors ~ post + trump_vs + post*trump_vs, data=retail_merged_2)
summary(model1)
model2 <- lm(daily_visitors ~ post + vaccine_hes + post*vaccine_hes, data=retail_merged_2)
summary(model2)
model3 <- lm(daily_visitors ~ post+ trump_vs + vaccine_hes + post*trump_vs + post*vaccine_hes, data=retail_merged_2)
summary(model3)
model4 <- lm(daily_visitors ~ post + dem_vs + post*dem_vs, data = retail_merged_2)
summary(model4)
model5 <- lm(daily_visitors ~ post + vaccine_hes + post*vaccine_hes + factor(statename), data=retail_merged_2)
summary(model5)






