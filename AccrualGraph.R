# Program to create accrual graphs for TREAT Studies
# Created: 02/10/2016
# Author: Andrew Borst

demog <- read.delim("~/R/Data/Demographics.txt")

# Start -------------------------------------------------------------------
library(ggplot2)
library(scales)

#Build monthly buckets of actual accrual
startdate = as.Date("2013-06-01")
enddate = Sys.Date()
mnths = seq(startdate, enddate, by="month")
mnths = append(mnths, enddate)

#ondate = as.Date(demog$ON_STUDY_DATE,format="%m/%d/%Y")
# create variables of the week and month of each observation:
demog$Month <- as.Date(cut(as.Date(demog$ON_STUDY_DATE,format="%m/%d/%Y"),breaks = "month"))
demog$Count = 1


Accrual = c(0,cumsum(as.vector(unlist(tapply(demog$Count,demog$Month,sum)))))

acc = data.frame(mnths,Accrual)

# graph by month:
ggplot(data = acc, aes(mnths, Accrual)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line") 
# + # or "line"
#   scale_x_date(labels = date_format("%Y-%m"),
#     breaks = date_breaks("4 month")) # custom x-axis labels

