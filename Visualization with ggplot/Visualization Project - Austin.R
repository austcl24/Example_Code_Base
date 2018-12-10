#############################################################
# DS 745 Visualization Project
# Chris Austin
# September 27 2018
#############################################################

require(tidyr)
require(dplyr)
require(ggplot2)
require(RColorBrewer)

# Format from World Bank needs work to put data in observational data frame
# structure based on Country/Year.
dataNames <- c("CountryName", "CountryCode", "SeriesName", "SeriesCode", "Data2010", "Data2011", "Data2012", "Data2013", "Data2014", "Data2015")

# Limit data read to 5426 rows due to the disclaimers at the bottom of the CSV.
wbData <- read.csv("WorldBankData.csv", stringsAsFactors = FALSE, col.names = dataNames)

# Add a year column and give Dataxxxx columns a common name. Place into
# intermediate data frames so that it can be tidied easier.
data0 <- with(wbData, data.frame(CountryName, CountryCode, SeriesCode, Year = 2010, Data = Data2010, stringsAsFactors = FALSE))
data1 <- with(wbData, data.frame(CountryName, CountryCode, SeriesCode, Year = 2011, Data = Data2011, stringsAsFactors = FALSE))
data2 <- with(wbData, data.frame(CountryName, CountryCode, SeriesCode, Year = 2012, Data = Data2012, stringsAsFactors = FALSE))
data3 <- with(wbData, data.frame(CountryName, CountryCode, SeriesCode, Year = 2013, Data = Data2013, stringsAsFactors = FALSE))
data4 <- with(wbData, data.frame(CountryName, CountryCode, SeriesCode, Year = 2014, Data = Data2014, stringsAsFactors = FALSE))
data5 <- with(wbData, data.frame(CountryName, CountryCode, SeriesCode, Year = 2015, Data = Data2015, stringsAsFactors = FALSE))

# Unify the dataframes with the year column. 
wbDataFlat <- rbind(data0, data1, data2, data3, data4, data5)

# Rename columns for ease of reference.
dataNames2 <- c("CountryName", "CountryCode", "Year", "CPI2010", "InflationPCT", "MilPctGDP", 
                "IncomePerCap2010", "GDPGrowthPct", "GDPPerCap", "EdSecFem1", "EdSecMale1", "EdSecFem2", 
                "EdSecMale2", "SmokeFemPct", "SmokeMalePct", "LaborParticipateFemPct", 
                "LaborParticpateMalePct", "LaborRatioFemToMale", "FemUnemployPct", "MaleUnemployPct", 
                "BirthPer1000", "DeathPer1000", "LifeExpFem", "LifeExpMale", "PopGrowPct", "Population", 
                "RuralPopPct", "UrbanPopPct")

# Spread() takes the Series code in each row of wbDataFlat (32550 x 5) and
# converts to columns (1302 x 29).
wbDataSpread = spread(wbDataFlat, SeriesCode, Data)
colnames(wbDataSpread) <- dataNames2

###################################################
# Pass 1 - First attempt - Scatterplot
plot.new()
colorCount1 = 7
cut1 <- cut_number(wbDataSpread$GDPPerCap,colorCount1)
gdpColors1 <- as.numeric(cut1)

# Set all NA’s to colorCount1 + 1 and add column to dataframe
wbDataSpread$gdpColors1 <- as.factor(ifelse(is.na(gdpColors1), colorCount1+1, gdpColors1))
cols1 <- brewer.pal(colorCount1+1,"Blues")

# switch last entry in cols[], signfying NA.
cols1[colorCount1+1] <- "#FD8383"

labels1 = rep(" ", length(cols1))
labels1[1] <- "Lowest GDP"
labels1[length(cols1)-1] = "Highest GDP"
labels1[length(cols1)] = "N/A"

plot1 <-  ggplot(wbDataSpread, aes(x = BirthPer1000, y = LifeExpFem, size=Year)) +
  geom_point(shape=19, aes(color = gdpColors1)) + scale_color_manual(values=cols1, labels=labels1)

plot1 <- plot1 + labs(
  x = "Birth Rate/1000", 
  y = "Female Life Expectancy(Yrs)", 
  title = "Birth Rate by Country and Female Life Expectancy over time - globally trending in positive directions",
  caption = "DataSource: World Development Indicators - http://databank.worldbank.org/data/Data-Project-Extract-1/id/5b353e77")

plot1 <- plot1 + labs(color = "GDP")
print(plot1)

####################################################################
# Pass 2 - Second attempt - Data for 2015 Only
tiled <- na.omit(wbDataSpread[wbDataSpread$Year == 2015, c("CountryCode", "BirthPer1000", "LifeExpFem", "GDPPerCap")])
rownames(tiled) <- tiled$CountryCode
colnames(tiled) <- c("Country", "Birth","LifeExp","GDP")
tiled = arrange(tiled, desc(Birth))
tiled2 <- as.matrix(tiled[,c(2,3,4)])
rownames(tiled2) <- tiled$Country

# Original: cex.main = 1.2, cex.sub = 1, mar = c(5.1,4.1,4.1,2.1))
par(cex.main = .9, cex.sub = .75, mar = c(1,1,1,1))
plot2 <- heatmap(tiled2, Rowv=NA, Colv = NA, col = rev(heat.colors(256)), scale = "column", 
                     cexCol = .9, margins = c(8,8), main = " \n \n \n")

title(main = "By-country Correlations between Birth Rate/1000, Female Life Expectancy %, and GDP (2010 US$) ordered by Birth Rate/1000 for 2015                     \n")
par(mar = c(8, 4.1, 4.1, 2.1))
title(sub  = "DataSource: World Development Indicators - http://databank.worldbank.org/data/Data-Project-Extract-1/id/5b353e77")

print(plot2)

######################################################################
# Pass 3 - Finalized display - Scatterplot with colors and labeling
# Prepare color gradient based on GDP Per Capita
colorCount3 = 7
cut3 <- cut_number(wbDataSpread$GDPPerCap,colorCount3)
gdpColors3 <- as.numeric(cut3)
gdpColors3 <- ifelse(is.na(gdpColors3), colorCount3+1, gdpColors3)

# Set all NA’s to colorCount3 + 1 and add column to dataframe
wbDataSpread$gdpColors3 <- as.factor(ifelse(is.na(gdpColors3), colorCount3+1, gdpColors3))
cols3 <- brewer.pal(colorCount3+1,"Greens")

#reverse the color scheme to accentuate the lower-GDP nations
cols3 <- rev(cols3)
cols3[colorCount3+1] <- "#F0DBDB"

# switch last entry in cols[], signfying NA, to reddish-pink.
plot3 <-  ggplot(wbDataSpread, aes(x = BirthPer1000, y = LifeExpFem, size=Year)) +
              geom_point(shape=19, aes(color = gdpColors3)) +
              scale_color_manual(values=cols3) + 
              geom_text(aes(label=ifelse(CountryName == "Djibouti" & Year == 2011, as.character(CountryName), " "),
                            hjust=1.7,vjust=0), size=3) +
              geom_text(aes(label=ifelse(CountryName == "Chad" & Year == 2011, as.character(CountryName), " "),
                            hjust=2.4,vjust=0), size=3) +
              geom_text(aes(label=ifelse(CountryName == "Sierra Leone" & Year == 2015, as.character(CountryName), " "),
                            hjust=1.4,vjust=0), size=3) +  
              geom_text(aes(label=ifelse(CountryName == "Central African Republic" & Year == 2015, as.character(CountryName), " "),
                            hjust=1.2,vjust=0), size=3) +
              geom_text(aes(label=ifelse(CountryName == "Niger" & Year == 2011, as.character(CountryName), " "),
                            hjust=1.6,vjust=0), size=3) +
              geom_text(aes(label=ifelse(CountryName == "Lesotho", as.character(round(GDPPerCap)), " "),
                            hjust=1.6,vjust=.5), size=3) +
              geom_text(aes(label=ifelse(CountryName == "South Africa", as.character(round(GDPPerCap)), " "),
                            hjust=1.6,vjust=.5), size=3) +
              geom_text(aes(label=ifelse(CountryName == "Japan" & Year == 2015, as.character(CountryName), " "),
                            hjust=0,vjust=-2.3), size=3) +
              geom_text(aes(label=ifelse(CountryName == "Israel" & Year == 2015, as.character(CountryName), " "),
                            hjust=0,vjust=-1.7), size=3) +
              geom_text(aes(label=ifelse(CountryName == "Macao SAR, China" & Year == 2015, "Macao", " "),
                            hjust=0,vjust=-1.75), size=3) +
              geom_text(aes(label=ifelse(CountryName == "Angola", as.character(Year), " "),
                            hjust=0,vjust=-1.75), size=3) +
              annotate("text", label= "GDP Not Available", x=23.9,y=61.8, size=3) +
              annotate("text", label= "South Africa GDP \n(2010 US$)", x=22,y=57.3, size=3) +
              annotate("text", label= "Lesotho GDP \n(2010 US$)", x=28.5 ,y=51, size=3) 

# Set ggplot labels
plot3 <- plot3 + labs(
              x = "Birth Rate/1000", 
              y = "Female Life Expectancy(Yrs)", 
              title = "Correlations of Birth Rate by Country to Female Life Expectancy over time are invariant of global economic conditions (2010-2015)",
              subtitle = "Life expectancies bunch into the 80s along with low birth rates and decreases in poverty, awaiting the next generation of medical advances",
              caption = "DataSource: World Development Indicators - http://databank.worldbank.org/data/Data-Project-Extract-1/id/5b353e77")

# set ggplot thematic controls
plot3 <- plot3 + theme(
              plot.title = element_text(size=14),
              plot.subtitle = element_text(size=10, face="italic", color = "black"),
              panel.background = element_blank(),
              legend.position = "none")

print(plot3)