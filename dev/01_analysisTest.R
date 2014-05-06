############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")

############################################################################################
###  1 - Time series unemployement chart
############################################################################################

#### A) Using WB unemployment

library(WDI)
library(rCharts)
library(plyr)

countries.iso2 <- c('AT', "CH", "DE", "ES", "FR", "IT", "PT", "GB" )


unempl <- WDI(country = c(countries.iso2, 'US', 'JP', 'BR', 'RU', 'CN'), indicator = "SL.UEM.1524.ZS",
  start = 1991,  end = 2012, extra = FALSE, cache = NULL)

#Clean up the data a bit
unempl <- rename(unempl, replace = c("SL.UEM.1524.ZS" = "unemployment"))
unempl$unemployment <- round(unempl$unemployment, 1)


# Create the chart
unemplPlot <- nPlot(
	unemployment ~ year,
	data = unempl,
	group = "country",
	type = "lineChart")

# Add axis labels and format the tooltip
unemplPlot$yAxis(axisLabel = "Youth (15-24) unemployement in %", width = 62)
unemplPlot$xAxis(axisLabel = "Year")
unemplPlot$chart(tooltipContent = "#! function(key, x, y){
      return '<h3>' + key + '</h3>' +
      '<p>' + y + ' in ' + x + '</p>'
      } !#")
unemplPlot



ids <- unique(unempl$iso2c)
country.selec <- as.logical(!ids %in% c('AT','CH','DE','ES','PT','GB'))
unemplPlot$set(disabled = country.selec)
unemplPlot$publish("line chart World Bank youth unemployment", host = "rpubs")

# unemplPlot$chart(color = c('brown', 'blue', '#594c26', 'green'))


# #### B) Get the OECD NEET values from statExtract 20-24 years (EDUGPS_EAG2013_C_Data_6c003e5f-11c5-46e8-89f0-5604da11ab48.csv)
# countries.full <- c('Austria', "Brazil", "Switzerland", "Greece", "Germany", "Spain", "France", "Italy",
# 	'JPN', "Portugal", "Russian Federation", "United Kingdom", "United States")
# data.read <- read.csv("EDUGPS_EAG2013_C_Data_6c003e5f-11c5-46e8-89f0-5604da11ab48_cleaned.csv", stringsAsFactors = F)
# #reformat columns
# data.read$Country <- as.factor(data.read$Country)
#
# neet <- data.read[data.read$Country %in% countries.full,]
#
# # Create the chart
# unemplPlot2 <- nPlot(
# 	Value ~ Time,
# 	data = neet,
# 	group = "Country",
# 	type = "lineChart")
# # Add axis labels and format the tooltip
# unemplPlot2$yAxis(axisLabel = "Youth (20-24) neither employed nor in education or in training[%]", width = 62)
# unemplPlot2$xAxis(axisLabel = "Year")
# unemplPlot2$chart(tooltipContent = "#! function(key, x, y){
#       return '<h3>' + key + '</h3>' +
#       '<p>' + y + ' in ' + x + '</p>'
#       } !#")
# unemplPlot2


#### C) Get the eurostat data
library(SmarterPoland)
empl.data <- getEurostatRCV("yth_empl_010") #http://epp.eurostat.ec.europa.eu/portal/page/portal/youth/data/database

countries.iso2 <- c('AT', "CH", "EL", "DE", "ES", "FR", "IT", "PT", "GB" )
age <- "Y25-29"

save(empl.data, file = "eurostat_youthEmpl.RData")



# filter data
myfilter <- function(data, age = "") {
	data <- data[data$sex == 'T',]
	data <- data[data$unit == 'PC',]
	data$time <- as.numeric(as.character(data$time))
	if(!is.null(age)) data <- data[data$age == age,]

	data[,!(names(data) %in% c('sex', 'unit'))]
}
empl.data  <- myfilter(empl.data, age = age)
empl <- empl.data[empl.data$geo %in% countries.iso2 & empl.data$isced97 == 'TOTAL' ,]


# Create the chart
emplPlot <- nPlot(
	value ~ time,
	data = empl,
	group = "geo",
	type = "lineChart")
# Add axis labels and format the tooltip
emplPlot$yAxis(axisLabel = "Youth (25-29) employed [%]", width = 62)
emplPlot$xAxis(axisLabel = "Year")
emplPlot$chart(tooltipContent = "#! function(key, x, y){
      return '<h3>' + key + '</h3>' +
      '<p>' + y + ' in ' + x + '</p>'
      } !#")
emplPlot



############################################################################################
###  2 - General youth unemployment by education levels
############################################################################################


# http://epp.eurostat.ec.europa.eu/portal/page/portal/youth/data/database#
# EU28 20-29Y: secondary education (52.6%), upper secondary (42.1%), tiertary (28.6%)


############################################################################################
###  3 - evolution of the tertiary education over time
############################################################################################



