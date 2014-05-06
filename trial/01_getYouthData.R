############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")

# to get eurostat data
library(SmarterPoland)
# to get World Bank data
library(WDI)


countries <- c('AT',"BE", "EL", "DE", "ES", "FI", "FR", "IT", "PT", "UK", "SE", "IE", "NL", "CH")

############################################################################################
###  Plot evolution of tertiary education over time (WB data)
############################################################################################


tert <- WDI(country = c(countries, 'US', 'JP', 'BR', 'RU', 'CN'), indicator = "SE.TER.ENRR",
  start = 1970, extra = FALSE, cache = NULL)

library(rCharts)
library(plyr)

#Clean up the data a bit
tert <- rename(tert, replace = c("SE.TER.ENRR" = "tertiary"))

tert$le <- round(tert$le, 2)
tert$region <- as.factor(sample(1:5, nrow(tert), replace = T))

# Create the chart
tertPlot <- nPlot(
tertiary ~ year,
data = tert,
group = "country",
type = "lineChart")

# Add axis labels and format the tooltip
tertPlot$yAxis(axisLabel = "Tertiary education in %", width = 62)

tertPlot$xAxis(axisLabel = "Year")

tertPlot$chart(tooltipContent = "#! function(key, x, y){
      return '<h3>' + key + '</h3>' +
      '<p>' + y + ' in ' + x + '</p>'
      } !#")

tertPlot


############################################################################################
###  Plot scatterplots education vs unemployement (eurostat, for different young age groups)
############################################################################################

# get employement and education statistics
empl.data <- getEurostatRCV("yth_empl_010") #http://epp.eurostat.ec.europa.eu/portal/page/portal/youth/data/database
edu.data <- getEurostatRCV("yth_demo_040")  #http://epp.eurostat.ec.europa.eu/portal/page/portal/youth/data/database

save(empl.data, edu.data, file = "eurostat_edu.RData")
empl2.data <- getEurostatRCV("yth_empl_160")


# filter data
myfilter <- function(data, age = NULL) {
	data <- data[data$sex == 'T',]
	data <- data[data$unit == 'PC',]
	data$time <- as.numeric(as.character(data$time))
	if(!is.null(age)) data <- data[data$age == age,]

	data[,!(names(data) %in% c('sex', 'unit'))]
}



empl.data <- empl.data[empl.data$time >= 2004 & empl.data$isced97 != 'NRP',]

edu.data  <- myfilter(edu.data)
#write.csv(edu.data, "education_OECD__yth_demo_040.csv", row.names = FALSE)

ggplot(subset(edu.data, geo %in% countries), aes(x = time, y = value, group = geo, color = geo)) +
  geom_line() + facet_grid(age ~ isced97, scales = "free_y")


# chart the type of edu vs unemployement by age groups


# get only the first and last year
pdf("eduTypeVsEmployement.pdf", width = 17, height = 12)
countries <- c(countries, "EU28" )
for (foo in c(max, min)) {
	empl <- empl.data[empl.data$time == foo(empl.data$time),]
	edu <- edu.data[edu.data$time == foo(edu.data$time),]
		#browser()
	year <- unique(c(empl$time, edu$time))

	stopifnot(length(year) == 1)
	#countries <- intersect(empl$geo, edu$geo)

	em <- empl[empl$isced97 != "TOTAL",]
	colnames(em)[colnames(em)=='value'] <- 'employement'
	ed <- edu[edu$geo %in% countries, c('geo','value', 'age','isced97')]
	colnames(ed)[colnames(ed)=='value'] <- 'education'

	data <- merge(em, ed, by=c('age', 'isced97', 'geo'))

	print(ggplot(data, aes(x = employement, y = education, label = geo)) + ggtitle(year) +
	  geom_point() + geom_text(aes(label=geo),hjust=0, vjust=0, size = 3, alpha = 0.5) + facet_grid(age ~ isced97))
}
dev.off()

# chart tertiaryEdu vs total employement
pdf("tertiaryEduVsEmployement.pdf")
sapply(unique(as.character(empl$age)), function(age) {
	em <- empl[empl$isced97== "TOTAL" & empl$age == age,]
	ed <- edu[edu$isced97== "ED5_6" & edu$age == age,]

	data <- cbind(em[match(countries, em$geo), c('geo','value', 'age')], tiertaryEdu = ed[match(countries, ed$geo), 'value'])

	print(ggplot(data, aes(x= value, y= tiertaryEdu, label=geo)) +
	  geom_point() + geom_text(aes(label=geo),hjust=0, vjust=0) + ggtitle(age))
})
dev.off()

