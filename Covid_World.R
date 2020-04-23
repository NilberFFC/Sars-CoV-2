##DATA PREPARATION
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plotly)
library(plyr)
#these libraries need to be loaded
library(utils)
library(httr)

##WORLD
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
world <- read.csv(tf)
world$countriesAndTerritories = trimws(world$countriesAndTerritories)
world$countriesAndTerritories[which(world$countriesAndTerritories == "United_Kingdom")] <- "UK"
world$countriesAndTerritories[which(world$countriesAndTerritories == "United_States_of_America")] <- "USA"
world$dateRep <- as.Date(world$dateRep,tryFormats = "%d/%m/%Y")
colnames(world)[7] <- "countries"
world <- world[rev(order(world$countries,as.Date(world$dateRep),decreasing = TRUE)),]
world$totDeaths <- ave(world$deaths, world$countries, FUN=cumsum)
world$totCases <- ave(world$cases, world$countries, FUN=cumsum)
world <- transform(world, var_casesPerDay=unlist(tapply(cases, countries, function(x) c(0, diff(x)))))
world <- transform(world, var_deathsPerDay=unlist(tapply(deaths, countries, function(x) c(0, diff(x)))))


## LOGS GRAPHS DATA

worldlog <- world[rev(order(world$countries,as.Date(world$dateRep),decreasing = TRUE)),]
worldlogCases <- worldlog[!(worldlog$totCases <= 100),]
worldlogDeaths <- worldlog[!(worldlog$totDeaths <= 100),]
worldlogCases[1,15] <- 1
for(i in 2:nrow(worldlogCases))
{
  if(worldlogCases[i,7] == worldlogCases[i-1,7]){worldlogCases[i,15] <- worldlogCases[i-1,15] + 1}
  else{worldlogCases[i,15] <- 1}
}
worldlogDeaths[1,15] <- 1
for(i in 2:nrow(worldlogDeaths))
{
  if(worldlogDeaths[i,7] == worldlogDeaths[i-1,7]){worldlogDeaths[i,15] <- worldlogDeaths[i-1,15] + 1}
  else{worldlogDeaths[i,15] <- 1}
}

colnames(worldlogCases)[15] <- "counter"
colnames(worldlogDeaths)[15] <- "counter"

##SUBSET FOR ARITH GRAPHS
graphFilter <- subset(world, countries %in% c("USA","Italy","Spain","France","UK","Brazil"))
##SUBSET FOR LOGS GRAPHS
logfilter <- subset(DATA, countries %in% c("Sweden","Norway","Denmark"))
##* DATA: One of the created data frames in log data scripts
##==================================================================================================
## - GRAPH 1: DEATHS/DAY
##==================================================================================================
graphFilter$countries <- factor(graphFilter$countries, levels=c("USA","Italy","Spain","France","UK","Brazil"))
dpd <- ggplot(graphFilter,aes(x=dateRep, y=deaths, 
                              group= countries, color=countries,
                              text = paste('Country:', countries,       
                                           '<br>e: ', dateRep,
                                           '<br>Deaths/day: ', deaths,
                                           '<br>Variation: ', round(pct_deaths*100/deaths, digits = 2),'%'))) +
  geom_line(linetype = 1, size = 1) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 2.75, stroke = 0.6) +
  ggtitle("Deaths per day - COVID-19") +
  xlab("Chronological Growth") +
  theme_ipsum() +
  scale_color_manual(values=c("blue", "darkgreen", "red", "lightblue", "darkblue")) +
  scale_x_date(limit=c(as.Date("2020-03-08"),as.Date("2020-04-15"))) +
  ylab("N?mero de Mortos")

ggplotly(dpd, 1350, 700, tooltip = c("text")) %>%
  highlight("plotly_selected")

##==================================================================================================
## - GRAPH 2: TOTAL DEATHS
##==================================================================================================
graphFilter$countries <- factor(graphFilter$countries, levels=c("USA","Italy","Spain","France","UK","Brazil"))

dthsOverallWorld <- ggplot(graphFilter, aes(x=dateRep, y=totDeaths, 
                                            group=countries, color=countries,
                                            text = paste('Pa?Country:', countries,
                                                         '<br>DatE: ', dateRep,
                                                         '<br>Deaths: ', totDeaths))) +
  geom_line(linetype = 1, size = 1) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 2, stroke = 0.5) +
  scale_color_manual(values=c("blue", "darkgreen", "red", "lightblue", "darkblue","gold")) +
  ggtitle("Deaths - COVID-19") +
  xlab("Chronological Growth") +
  theme_ipsum() +
  scale_x_date(limit=c(as.Date("2020-03-01"),as.Date("2020-04-16"))) +
  ylab("N?mero de Mortos")

ggplotly(dthsOverallWorld, 1350, 700, tooltip = c("text")) %>%
  highlight("plotly_selected")

##==================================================================================================
## - GRAPH 3: TOTAL CASES
##==================================================================================================
casesOverallWorld <- ggplot(graphFilter, aes(x=dateRep, y=totCases, 
                                             group=countries, color=countries,
                                             text = paste('Country:', countries,
                                                          '<br>Data: ', dateRep,
                                                          '<br>Cases: ', totCases))) +
  geom_line(linetype = 1, size = 1.5) e
  geom_point(shape = 21, colour = "black", fill = "white", size = 2.5, stroke = 0.25) +
  ##scale_color_viridis(discrete = TRUE) +
  labs(title = "Total Cases - COVID-19\n", x = "Chronological Growth", y = "Number of Infected", color = "Country") +
  scale_color_manual(values=c("red", "darkblue","orange")) +    
  theme_ipsum() +
  scale_x_date(limit=c(as.Date("2020-02-25"),as.Date("2020-04-23"))) +
  theme(legend.title = element_text(color = "darkBlue", size = 16),
        legend.text = element_text(color = "darkBlue"))

ggplotly(casesOverallWorld, 1350, 700, tooltip = c("text")) %>%
  highlight("plotly_selected")

##==================================================================================================
## - GRAPH 4: CASES/DAY
##==================================================================================================
cpdWorld <- ggplot(graphFilter, aes(x=dateRep, y=cases, 
                                    group=countries, color=countries,
                                    text = paste('Country:', countries,
                                                 '<br>Date: ', dateRep,
                                                 '<br>Cases: ', cases,
                                                 '<br>Variation: ', round(pct_cases*100/cases, digits = 2),'%'))) +
  geom_line(linetype = 1, size = 1.5) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 3, stroke = 0.75) +
  scale_color_manual(values=c("blue", "darkgreen", "red", "lightblue", "darkblue","gold")) +
  labs(title = "Cases per day - COVID-19\n", x = "Chronological Growth", y = "Number of Infected", color = "Country") +
  theme_ipsum() +
  scale_x_date(limit=c(as.Date("2020-02-25"),as.Date("2020-04-15"))) +
  theme(legend.title = element_text(color = "darkBlue", size = 16),
        legend.text = element_text(color = "darkBlue"))

ggplotly(cpdWorld, 1350, 700, tooltip = c("text")) %>%
  highlight("plotly_selected")

##==================================================================================================
## - GRAPH 5: LOG GRAP EXAMPLE
##==================================================================================================
y <- subset(worldlogDeaths, countries %in% c("Sweden","Norway","Denmark"))
x <- ggplot(y, aes(x=counter, y=totDeaths, 
                   group=countries, color=countries,
                   text = paste('Regi?o:', countries,
                                '<br>Dias de contamina??o: ', counter,
                                '<br>Data: ', dateRep,
                                '<br>Fatalidades: ', totDeaths))) +
  geom_line(linetype = 1, size = 1.5) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 2, stroke = 0.25) +
  scale_color_manual(values=c("red", "darkblue","orange")) +    
  ggtitle("Curva Logar?timica - Evolu??o de mortes confirmadas - Sars-COV-02") +
  xlab("Dias contados do cent?sima morte confirmada") +
  ylab("N?mero de Mortos")+
  scale_y_log10(limits=c(100, 2000),breaks=c(100, 500, 2000))

  ggplotly(x, 1350, 680, tooltip = c("text"), dynamicTicks = T) %>%
  highlight("plotly_selected")
