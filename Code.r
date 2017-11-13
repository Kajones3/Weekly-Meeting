
#Load the beginning
salesforce <- read.csv("October.csv", header = T, stringsAsFactors = T)
dials <- read.csv("oct_dials.csv", header = T, stringsAsFactors = T)
talk <- read.csv("oct_talk.csv", header = T, stringsAsFactors = T)
library(tidyverse)

#Change the date
salesforce$Created.Date <- as.Date(salesforce$Created.Date, "%m/%d/%Y")
salesforce$Contacted.Date <- as.Date(salesforce$Contacted.Date, "%m/%d/%Y")
salesforce$Scheduled.Date <- as.Date(salesforce$Scheduled.Date, "%m/%d/%Y")
salesforce$Showed.Date <- as.Date(salesforce$Showed.Date, "%m/%d/%Y")
salesforce$Enrollment.Date <- as.Date(salesforce$Enrollment.Date, "%m/%d/%Y")
str(salesforce)

#Split time and date in dials
names(dials)[names(dials)=="Created.Date.and.Time"] <- "Date"
dials$Date <- as.Date(dials$Date, format = "%m/%d/%Y")
dials$Create.Date.and.Time <- as.POSIXct(as.character(dials$Create.Date.and.Time), format = "%m/%d/%Y%H:%M")
dials$time <- format (dials$Create.Date.and.Time, "%T")

#Create new variables
salesforce$status <- "(0) Other"
salesforce$status[salesforce$Contacted.Date > 0] <- "(1) Contacted" 
salesforce$status[salesforce$Scheduled.Date > 0] <- "(2) Scheduled"
salesforce$status[salesforce$Showed.Date > 0] <- "(3) Showed"
salesforce$status[salesforce$Admissions.Status == "Closed - Enrolled"] <- "(4) Enrolled"
salesforce$status <- as.factor(salesforce$status)
summary(salesforce$status)
summary(salesforce$Created.Date)



#Let's separate the weeks
salesforce$week <- "other"
salesforce$week[salesforce$Created.Date <= "2017-10-7"] <- "Week 1"
salesforce$week[salesforce$Created.Date > "2017-10-7" & salesforce$Created.Date <= "2017-10-14"] <- "Week 2"
salesforce$week[salesforce$Created.Date > "2017-10-14" & salesforce$Created.Date <= "2017-10-21"] <- "Week 3"
salesforce$week[salesforce$Created.Date > "2017-10-21" & salesforce$Created.Date <= "2017-10-28"] <- "Week 4"
salesforce$week[salesforce$Created.Date > "2017-10-28" & salesforce$Created.Date <= "2017-11-05"] <- "Week 5"
salesforce$week <- as.factor(salesforce$week)
summary(salesforce$week)


dials$week <- "other"
dials$week[dials$Date <= "2017-10-7"] <- "Week 1"
dials$week[dials$Date > "2017-10-7" & dials$Date <= "2017-10-14"] <- "Week 2"
dials$week[dials$Date > "2017-10-14" & dials$Date <= "2017-10-21"] <- "Week 3"
dials$week[dials$Date > "2017-10-21" & dials$Date <= "2017-10-28"] <- "Week 4"
dials$week[dials$Date > "2017-10-28" & dials$Date <= "2017-11-05"] <- "Week 5"
dials$week <- as.factor(dials$week)
summary(dials$week)


#What have we trended over the past few weeks in dials.
dials %>%
  group_by(Assigned, week) %>%
  summarize(count = n()) %>%
  filter(count > 50) %>%
  ggplot(aes(x = week, y = count)) +
  geom_boxplot()+
  text(x=fivenum(Assigned), labels =fivenum(Assigned), y=1.25)+
  labs(title = "Contact Center Dials", x = "Week of Calling", y = "# of Dials")


dials %>%
  group_by(Lead.Owner, Call.Duration...Seconds) %>%
  summarize(call = sum(Call.Duration...Seconds)) 



#%>%
  filter(count) %>%
  ggplot(aes(x = week, y = count)) +
  geom_boxplot()+
  text(x=fivenum(Assigned), labels =fivenum(Assigned), y=1.25)+
  labs(title = "Contact Center Dials", x = "Week of Calling", y = "# of Dials")



ggplot(dials, aes(week)) +
  geom_bar(aes(fill = dials$Assigned))

ggplot(dials, aes(Date)) +
  geom_line(aes(), stat="count")+
  geom_smooth(stat="count")

