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
unemplPlot$yAxis(axisLabel = "Youth (age 15-24) unemployement in %", width = 55)
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


############################################################################################
###  2 - General youth unemployment by education levels
############################################################################################


# http://epp.eurostat.ec.europa.eu/portal/page/portal/youth/data/database#
# EU28 20-29Y: secondary education (52.6%), upper secondary (42.1%), tiertary (28.6%)


############################################################################################
###  3 - evolution of the tertiary education over time
############################################################################################

# countries.full <- c('Austria', "Switzerland", "Germany", "Spain", "France", "Italy", "Portugal", "United Kingdom",
# 	"Japan", "United States")
# edu.data <- read.csv("data/EDUGPS_EAG2013_A_Data_6e1ad19b-7915-42ed-9f05-0e713b14060f.csv")
#
# tertiary <- edu.data[edu.data[,1] == "Attained a tertiary education degree, 25-34 year-olds (%)",]
# tert <- tertiary[tertiary$Country %in% countries.full,]
#
# tertPlot <- nPlot(
# 	Value ~ Time,
# 	data = tert,
# 	group = "Country",
# 	type = "lineChart")
#
# # Add axis labels and format the tooltip
# tertPlot$yAxis(axisLabel = "Youth (age 25-34) with tertiary education in %", width = 55)
# tertPlot$xAxis(axisLabel = "Year")
# tertPlot$chart(tooltipContent = "#! function(key, x, y){
#       return '<h3>' + key + '</h3>' +
#       '<p>' + y + ' in ' + x + '</p>'
#       } !#")
#
# ids <- unique(tert$Country)
# country.selec2 <- as.logical(!ids %in% c('Austria','Switzerland','Germany','Spain','Portugal','United Kingdom'))
# tertPlot$set(disabled = country.selec2)
# tertPlot$publish("line chart OECD youth tertiary education", host = "rpubs")


## variant with WB data, by regions
tert2 <- WDI(indicator = "SE.TER.ENRR", start = 1970, end = 2011, extra = FALSE, cache = NULL)

tert2 <- tert2[tert2[,1] %in% c("EU", "ZJ", "8S", "1W", "ZQ", "XU", "Z4"),]
#Clean up the data a bit
tert2 <- rename(tert2, replace = c("SE.TER.ENRR" = "tertiary"))
tert2$tertiary <- round(tert2$tertiary, 1)
tert2$country<- gsub(" \\(all income levels\\)", "", tert2$country)

# Create the chart
tertPlot2 <- nPlot(
	tertiary ~ year,
	data = tert2,
	group = "country",
	type = "lineChart")

# Add axis labels and format the tooltip
tertPlot2$yAxis(axisLabel = "Tertiary education enrollement in % 5 years from secondary school leaving", width = 62)
tertPlot2$xAxis(axisLabel = "Year")
tertPlot2$chart(tooltipContent = "#! function(key, x, y){
    return '<h3>' + key + '</h3>' +
    '<p>' + y + ' in ' + x + '</p>'
    } !#")
tertPlot2


############################################################################################
###  3 - evolution of the tertiary education over time
############################################################################################

