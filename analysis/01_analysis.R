############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")
font <- "Archivo Narrow"


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
ids <- unique(unempl$iso2c)
country.selec <- as.logical(!ids %in% c('AT','CH','DE','ES','PT','GB'))
unemplPlot$set(disabled = country.selec)
unemplPlot$publish("line chart World Bank youth unemployment", host = "rpubs")
unemplPlot


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
tertPlot2$yAxis(axisLabel = "Tertiary education enrollement 5 years from secondary school leaving [%]", width = 62)
tertPlot2$xAxis(axisLabel = "Year")
tertPlot2$chart(tooltipContent = "#! function(key, x, y){
    return '<h3>' + key + '</h3>' +
    '<p>' + y + ' in ' + x + '</p>'
    } !#")
#tertPlot2
tertPlot2$publish("line chart World Bank youth tiertary education", host = "rpubs")


############################################################################################
###  4 - tertiary education vs NEET
############################################################################################

eduneet.data <- read.csv("data/eduAttainment_NEET-refined-csv.csv")
country2regions <- read.csv("~/swissinfo/_helpers/country-regions.csv")

eduneet.data$value <- as.numeric(as.character(eduneet.data$value))
eduneet.data$NEET <- as.numeric(as.character(eduneet.data$NEET))


countries.iso3 <- c('AUT', 'FRA', 'DEU', 'GRC', 'ITA', 'JPN', 'PRT', 'ESP', 'CHE', 'USA', 'BRA','UKM')
names(countries.iso3) <- ifelse(country2regions[match(countries.iso3, country2regions[,1]), 2] == 'ECS' |
	is.na(country2regions[match(countries.iso3, country2regions[,1]), 2]), 'EU', 'non-EU')

eduneet <- eduneet.data[eduneet.data$ISO3 %in% countries.iso3 & eduneet.data$year == 2011,]

eduneet$Educational.attainment <- reorder(eduneet$Educational.attainment,
	match(eduneet$Educational.attainment, unique(as.character(eduneet$Educational.attainment))))


myreshape <- function(eduneet, eduLevels =
	as.character(unique(eduneet$Educational.attainment)[unique(eduneet$Educational.attainment) != "Total"])) {

		do.call(rbind, lapply(eduLevels, function(ed) {
			temp <- cbind(eduneet[eduneet$Educational.attainment==ed, c('Country', 'ISO3', 'Educational.attainment', 'value')],
				NEET = eduneet[eduneet$Educational.attainment=='Total','NEET'])
			cbind(temp, region = names(countries.iso3)[match(temp$ISO3,countries.iso3)])
		}))
}
xlabel <- "% of 15-29 year-olds neither in employment nor in education or training (NEET)"
eduneet2 <- myreshape(eduneet)
pdf("eduVsneetByEduLevel.pdf", width = 13, height = 10, family = font)
ggplot(eduneet2, aes(x = NEET, y = value, label = Country, group = region)) +
  geom_point(size = 4, alpha = 0.7, aes(color = region, shape = `Educational.attainment`)) + geom_text(aes(label = Country), hjust=0, vjust=0, size = 3, alpha = 0.8) +
  ggtheme_ygrid + ylab("% of 25-34 year-olds who attained that level of education") + xlab(xlabel)  + ggtitle("Youth unemployment (NEET) vs education level by country") +
  facet_wrap (~ `Educational.attainment`) + theme(legend.position = "none", panel.border = element_rect(linetype = "dashed", colour = "grey"))

# ggplot(eduneet2, aes(x = NEET, y = value, label = Country, group = region)) +
# 	geom_point(size = 3, alpha = 0.8, aes(color = region)) + geom_text(aes(label = Country), hjust=0, vjust=0, size = 2, alpha = 0.8) +
# 	ggtheme_ygrid + ylab("% of 25-34 year-olds who attained that level of education") + xlab(xlabel)  +
# 	facet_wrap (~ `Educational.attainment`, nrow = 3) + theme(legend.position = "none", panel.border = element_blank())

dev.off()



# tertvsneet <- cbind(eduneet[eduneet$Educational.attainment=='Tertiary education', c('Country', 'ISO3', 'value')],
# 	NEET = eduneet[eduneet$Educational.attainment=='Total','NEET'])
# tertvsneet <- cbind(tertvsneet, region = names(countries.iso3)[match(tertvsneet$ISO3,countries.iso3)])
#
# secondvsneet<- cbind(eduneet[eduneet$Educational.attainment=='Upper secondary or post-secondary non-tertiary',c('Country', 'ISO3', 'value')],
# 	NEET = eduneet[eduneet$Educational.attainment=='Total','NEET'])
# secondvsneet <- cbind(secondvsneet, region = names(countries.iso3)[match(secondvsneet$ISO3,countries.iso3)])
#
# #ylabel <- "25-34 year"
#
# ggplot(tertvsneet, aes(x = NEET, y = value, label = Country, group = region)) +
#   geom_point(size = 5, alpha = 0.8, aes(color = region)) + geom_text(aes(label = Country), hjust=0, vjust=0, size = 2, alpha = 0.4) +
#   ggtheme_ygrid + ylab("% of 25-34 year-olds who attained tertiarty education") + xlab(xlabel)
#
# ggplot(secondvsneet, aes(x = NEET, y = value, label = Country, group = region)) +
# 	geom_point(size = 5, alpha = 0.8, aes(color = region)) + geom_text(aes(label = Country), hjust=0, vjust=0, size = 2, alpha = 0.4) +
# 	ggtheme_ygrid + ylab("% of 25-34 year-olds who attained Upper secondary (non-tertiary) education") + xlab(xlabel)


# tertvsneetPlot <- nPlot(value ~ NEET, group = 'Country', data = tertvsneet, type = 'scatterChart')
# tertvsneetPlot$chart(size = '#! function(d){return d.size} !#',
# 	tooltipContent = "#! function(key, x, y){
#       return '<h3>' + key + '</h3>' +
#       '<p>' + y + ' in ' + x + '</p>'
#       } !#")
# tertvsneetPlot$yAxis(axisLabel = 'Youth (age 25-34) with tertiary education [%]')
# tertvsneetPlot$xAxis(axisLabel = 'Youth effective unemployment (age 20-24) neither employed nor in education or training [%]')
# tertvsneetPlot$chart(showControls = FALSE)
#
# ids <- unique(tertvsneet$ISO3)
# country.selec3 <- as.logical(!ids %in% c('AUT','CHE','DEU','ESP','PRT','UKM'))
# tertvsneetPlot$set(disabled = country.selec3)
# tertvsneetPlot
# tertvsneetPlot$publish("scatter chart OECD youth unemployment vs tertiery education", host = "rpubs")


############################################################################################
###  5 - detailed eduction level vs unemployment
############################################################################################

#eduneet.sub <- reshape(eduneet, v.names = "Educational.attainment",  idvar = "Country", timevar = "year", direction = "wide")
neet <- eduneet
neet$edu <- reorder(eduneet$Educational.attainment,
	match(eduneet$Educational.attainment, unique(as.character(eduneet$Educational.attainment))))

ggplot(neet, aes(`edu`, NEET, fill = edu)) + geom_bar(width = 0.8) + facet_wrap (~ Country) + xlab("") +
	ggtheme_ygrid + theme(axis.text.x = element_blank(),  axis.ticks.x = element_blank())
