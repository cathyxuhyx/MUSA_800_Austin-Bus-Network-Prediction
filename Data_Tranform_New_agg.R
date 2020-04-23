#Transform the three year's data into the same format
New <- read.csv("C:/Upenn/Practicum/Data/Stop Data _2017_2019.csv")
#Only keep the data for weekdays by selecting service day
data_new <- New %>%
  subset(New$SERVICE_DAY == 1)

data_2017 <- read.csv("C:/Upenn/Practicum/Data/Stop Data January - June 2017 .csv")
data_new_2017 <-data_2017%>%
  subset(data_2017$SERVICE_DAY == 1)
#Get three year's data combines
data_all <- rbind(data_new, data_new_2017)
#In order to change the months into (-6,6), each year's data needs to be extracted
data.2017 <- data_all %>%
  subset(data_all$YEAR_ID == 2017)
data.2018 <- data_all %>%
  subset(data_all$YEAR_ID == 2018)
data.2019 <- data_all %>%
  subset(data_all$YEAR_ID == 2019)
#Create the month column and before column for future visualization
data.2018$MONTH_ID <- as.numeric(data.2018$MONTH_ID)
data.2018$Month <- data.2018$MONTH_ID - 6
data.2018$Before <- ifelse(data.2018$Month < 0, 1, 0)

data.2019$MONTH_ID <- as.numeric(data.2019$MONTH_ID)
data.2019$Month <- data.2019$MONTH_ID - 6
data.2019$Before <- ifelse(data.2019$Month < 0, 1, 0)

data.test <- rbind(data.2018, data.2019)
data.test$YEAR_ID <- as.factor(data.test$YEAR_ID)
data.test$AVERAGE_ON <- as.numeric(data.test$AVERAGE_ON)

###Add 2017 data
data.2017$MONTH_ID <- as.numeric(data.2017$MONTH_ID)
data.2017$Month <- data.2017$MONTH_ID - 6
data.2017$Before <- ifelse(data.2017$Month < 0, 1, 0)

data_final <- rbind(data.2017,data.test)
data_final$AVERAGE_ON <- as.numeric(data_final$AVERAGE_ON)

#Create the annual ridership plot
plot.all<-
  as.data.frame(data_final) %>%
  group_by(Month, YEAR_ID) %>% 
  summarize(BOARD_Count = sum(AVERAGE_ON), Time = as.factor(max(Before))) %>%
  ggplot(aes(x=Month,y=BOARD_Count,  colour = Time, linetype = YEAR_ID)) + 
  geom_point() + stat_smooth(size=1) +
  plotTheme() +
  ylim(70000,170000) +
  labs(title="System-wide Ridership on an average weekday among all the stops in Austin",
       subtitle="CapRemap Redesign Implemented Month in Blue", x="Month", y="Average Daily Ridership")+ 
  geom_vline(xintercept = 0, color = "blue")+
  scale_colour_manual(values = c("#E7B800", "#0072B2"), name="Time Period (Before,After)", breaks=c("0", "1"), labels=c("After", "Before"))+
  # scale_color_brewer(palette = "YlGnBu")
  scale_linetype_manual(values=c("dotted", "solid", "dashed"))

plot.all

write.csv(data_final, "C:/Upenn/Practicum/Data/Final.csv")
