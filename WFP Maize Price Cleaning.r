#   Title:      WFP Maize Price Cleaning
#   Name:       Andy Zimmer
#   Contact:    azimmer@arizona.edu
#   Updated:    may 2022
#   Purpose:    clean, interpolate and export maize prices for markets of southern Africa as three variables for analysis alongside climate
#               variables to generate: 1) post-harvest price change 2) post-harvest average 3) post-harvest coefficient of variation

####   load packages    ###
rm(list=ls()) #clear and close

library(tidyverse)
library(lubridate)
library(imputeTS)
library(raster)
library(ggpubr)

####    step 1 - load and filter raw maize price data ####

####    maize price data
#   monthly, market-level maize price data for southern africa, downloaded from the world food programme (https://dataviz.vam.wfp.org/version2/)
zambia_wfp_raw <-       read.csv("/Users/azimmer/Documents/PhD Geography/Research/Zimmer - Maize Price/WFP Data/Raw/WFP_2022Feb28_Zambia_FoodPricesData.csv")
mozambique_wfp_raw <-   read.csv("/Users/azimmer/Documents/PhD Geography/Research/Zimmer - Maize Price/WFP Data/Raw/WFP_2022Feb28_Mozambique_FoodPricesData.csv")
malawi_wfp_raw <-       read.csv("/Users/azimmer/Documents/PhD Geography/Research/Zimmer - Maize Price/WFP Data/Raw/WFP_2022Feb28_Malawi_FoodPricesData.csv")

#   merge three countries together
complete_wfp_raw <- rbind(zambia_wfp_raw, mozambique_wfp_raw, malawi_wfp_raw)

#   create a unique market id variable to make market-level aggregation easier
complete_wfp_raw <- complete_wfp_raw %>%
    group_by(Market) %>%
    mutate(marketID = cur_group_id())

#   check number of unique markets in each country
complete_wfp_raw %>%
    group_by(Country) %>%
    summarise(num_markets = n_distinct(Market))

#   make date a datetime feature for temporal aggregation
complete_wfp_raw_date <- complete_wfp_raw %>%
  dplyr::select(Country, Admin.1, Market, marketID, Commodity, Year, Month, Price, Unit, Currency) %>%
  mutate(Date = make_date(Year, Month))

#   extract only maize prices. keeps all variations of 'maize'
maize_wfp_raw <- complete_wfp_raw_date[grepl("Maize", complete_wfp_raw_date$Commodity), ]

#   count observations in each country for each variation of maize. will retain most common
maize_wfp_raw %>%
    group_by(Country, Commodity) %>%
    tally()

#   retain country/maize type based on previous step
maize_wfp_raw<-maize_wfp_raw[(maize_wfp_raw$Country=="Malawi" & maize_wfp_raw$Commodity == "Maize" |
                                maize_wfp_raw$Country=="Mozambique" & maize_wfp_raw$Commodity == "Maize (white)" |
                                maize_wfp_raw$Country=="Zambia" & maize_wfp_raw$Commodity == "Maize (white)"),]

#   clip record from 2003-2019 to ensure most complete record across all countries
maize_wfp_raw <- maize_wfp_raw %>%
                    filter(Year >= 2003 & Year <= 2019)

#   plot number of markets with data available through time
maize_wfp_raw %>%
    group_by(Country, Date) %>%
    tally() %>%
    ggplot(aes(x = Date, y = n)) +
    geom_line() +
    theme_bw() +
    facet_wrap(~Country) +
    scale_x_date(date_breaks = "1 year") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = "", y = "Number of Markets w/ Data")

#   plot maize price through time for each country, in local currency/kg
ggplot(maize_wfp_raw, aes(x = Date, y = Price, color = Commodity)) +
    geom_line() +
    theme_bw() +
    facet_wrap(~Country) +
     scale_x_date(date_breaks = "1 year") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     labs(x = "date", y = "price/kg")




####    step 2 - convert raw maize price data to usd/kg ####

####    exchange rate data
#   monthly exchange rate data for each country to usd, from bis (https://www.bis.org)
currency_data <- read.csv("/Users/azimmer/Documents/PhD Geography/Research/Zimmer - Maize Price/Exchange Rate Data/BIS_data.csv")

#   reshape dataframe and convert date to a datetime feature to help merge with maize prices
currency_data <- currency_data %>%
    gather(Country, USD_conversion, Malawi:Zambia) %>%
    mutate(Date = make_date(Year, Month)) %>%
    dplyr::select(Date, Country, USD_conversion)

#   merge exchange rate data with maize prices and convert local currency to usd/kg
maize_wfp_deflated <- merge(maize_wfp_raw, currency_data, by.x= c("Country", "Date"), by.y= c("Country", "Date"))
maize_wfp_deflated <- maize_wfp_deflated %>%
  mutate(USD_Price = (Price/USD_conversion))

#   plot maize price through time for each country, in usd/kg
ggplot(maize_wfp_deflated, aes(x = Date, y = USD_Price, color = Commodity)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~Country) +
  scale_x_date(date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "date", y = "usd/kg")




####    step 3 - correct for inflation  ####

#### consumer price index data
#   monthly cpi data for usd, will result in prices reflecting december 2020 price equivalent. data from (https://www.bls.gov)
usa_cpi <- read.csv("/Users/azimmer/Documents/PhD Geography/Research/Zimmer - Maize Price/Inflation Data/CPI.csv")

#   format cpi data, make sure date is a datetime feature
usa_cpi$Month <- as.integer(factor(usa_cpi$Month, levels = month.name))
usa_cpi <-   usa_cpi %>%
  mutate(Date = make_date(Year, Month)) %>%
  dplyr::select(Date, Deflator)

colnames(usa_cpi) <- c("Date", "CPI_Deflator")

#   merge with usd maize prices
maize_wfp_deflated_USD <- merge(maize_wfp_deflated, usa_cpi, by =  "Date")

#   create deflated_USD using cpi
maize_wfp_deflated_USD <- maize_wfp_deflated_USD %>%
  mutate(deflated_USD = (USD_Price * CPI_Deflator))

#   plot maize price through time for each country, in deflated usd/kg
ggplot(maize_wfp_deflated_USD, aes(x = Date, y = deflated_USD, color = Commodity)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~Country) +
  scale_x_date(date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "date", y = "usd/kg (deflated)")




####    step 4 - export deflated usd/kg maize prices as csv ####
cleaned_wfp_data <- dplyr::select(maize_wfp_deflated_USD, Date, Country, Market, marketID, deflated_USD)
colnames(cleaned_wfp_data) <- c("date", "country", "market", "marketID", "usd_price")

write.csv(cleaned_wfp_data, "/Users/azimmer/Documents/PhD Geography/Research/Zimmer - Maize Price/WFP Data/Raw/cleaned_deflated_wfp_data.csv")




####    step 5 - add growing season variable based on time-window   ####

#   southern africa growing season spans two calendar years
#   we assign each date a growing season variable, which relates to the season maize was planted
#   we are interested in post-harvest prices
market_year_count <- cleaned_wfp_data %>%
  mutate(growing_season = case_when(
    date <= "2003-05-01" ~ 2002,
    date <= "2004-05-01" & date > "2003-05-01" ~ 2003,
    date <= "2005-05-01" & date > "2004-05-01" ~ 2004,
    date <= "2006-05-01" & date > "2005-05-01" ~ 2005,
    date <= "2007-05-01" & date > "2006-05-01" ~ 2006,
    date <= "2008-05-01" & date > "2007-05-01" ~ 2007,
    date <= "2009-05-01" & date > "2008-05-01" ~ 2008,
    date <= "2010-05-01" & date > "2009-05-01" ~ 2009,
    date <= "2011-05-01" & date > "2010-05-01" ~ 2010,
    date <= "2012-05-01" & date > "2011-05-01" ~ 2011,
    date <= "2013-05-01" & date > "2012-05-01" ~ 2012,
    date <= "2014-05-01" & date > "2013-05-01" ~ 2013,
    date <= "2015-05-01" & date > "2014-05-01" ~ 2014,
    date <= "2016-05-01" & date > "2015-05-01" ~ 2015,
    date <= "2017-05-01" & date > "2016-05-01" ~ 2016,
    date <= "2018-05-01" & date > "2017-05-01" ~ 2017,
    date <= "2019-05-01" & date > "2018-05-01" ~ 2018,
    date <= "2020-05-01" & date > "2019-05-01" ~ 2019,
    date <= "2021-05-01" & date > "2020-05-01" ~ 2020)) %>%
    filter(!is.na(usd_price)) # removing rows with NA price

#   add column to describe how much missing data for each market/year combination
market_year_count <- market_year_count %>%
  group_by(marketID, growing_season) %>%
  mutate(market_count = n()) %>%
  mutate(missing_count = 12-n()) %>%
  ungroup()




####    step 6 - interpolate missing data   ####

#   calculate missing data for each market. should be 204 observations for each market (17 years x 12 months)
#   markets with > 100% of data are errors from wfp duplicate naming and will be removed from sample
data_available <- cleaned_wfp_data %>%
  group_by(marketID, market, country) %>%
  summarise(non_na_count = sum(!is.na(usd_price))) %>%
  mutate(percentage = (non_na_count/204)*100)

data_available <-data_available[(data_available$percentage <=100),]

#   plot percentage of data avilable
ggplot(data_available, aes(y = percentage)) +
  geom_histogram(bins = 10, fill = 'steelblue', color = 'gray90') +
  coord_flip() +
  theme_bw() +
  labs(y = "% data available", x = "Count") +
  geom_hline(yintercept = 71, color = 'red')

#   filter data for interpolation. performed sensitivity analysis to retain sufficient # of markets in each country
useful_markets <- data_available[(data_available$percentage >= 70) & (data_available$percentage <=100), ]
useful_markets %>% 
    group_by(country) %>% 
    summarise(no_markets = n_distinct(market))

#   keep only 'useful markets' for imputation
useful_cleaned_wfp_data <- cleaned_wfp_data %>%
    filter(market %in% useful_markets$market)

#   perform three types of interpolation and compare between
interpolated_test <- useful_cleaned_wfp_data %>%        #standard interpolation
  group_by(market) %>%
  na_interpolation()

spline_test <- useful_cleaned_wfp_data %>%              #spline interpolation
  group_by(market) %>%
  na_interpolation(option = "spline")

kalman_test <- useful_cleaned_wfp_data %>%              #kalman interpolation
  group_by(market) %>%
  na_kalman()

#   change column names and merge together for plotting/comparison
colnames(interpolated_test) <- c("date", "country", "market", "interpolated_usd_price", "marketID")
colnames(spline_test) <- c("date", "country", "market", "splined_usd_price", "marketID")
colnames(kalman_test) <- c("date", "country", "market", "kalman_usd_price", "marketID")

interpolated_df <- merge(useful_cleaned_wfp_data, interpolated_test, by = c("date", "country", "market", "marketID"))
interpolated_df <- merge(interpolated_df, spline_test, by = c("date", "country", "market", "marketID"))
interpolated_df <- merge(interpolated_df, kalman_test, by = c("date", "country", "market", "marketID"))

#   plot three types of interpolation and visually compare. use limits to compare
ggplot(interpolated_df, aes(x = date)) +
  geom_line(aes(y = usd_price), color = 'black', size = 3) +
  geom_line(aes(y = interpolated_usd_price), color = 'red', size =1) +
  geom_line(aes(y = splined_usd_price), color = 'green', size =1) +
  geom_line(aes(y = kalman_usd_price), color = 'blue', size =1) +
  facet_wrap(~market) 

####    now every market has a complete record from 2003-2019. standard interpolation used for remaining analysis



####    step 7 - calculate dependent variable 1 - post-harvest maize price change   ####

#   this is performed as a loop to identify the lowest and highest prices in two seperate three month windows
#   post-harvest price change (phpc) is the difference between the low and high point, for each market/year

loopdata <- dplyr::select(interpolated_df, date, marketID, interpolated_usd_price)
colnames(loopdata) <- c("date", "marketID", "price")
loopdata <- loopdata %>% spread(marketID, price)

loopdata <- loopdata %>%
  mutate(year = year(date),
         month = month(date))

loopdata <- subset(loopdata, select = -c(date))

#   define years and months for loop
years <- loopdata$year
months <- loopdata$month

loopdata <- loopdata[0:77]

numyrs <- (max(years) - min(years))

range <- matrix(NA, nrow = numyrs, ncol = (ncol(loopdata)))

for(j in 1:77){ #number of markets 
  
  #initialize counter of first min price window. This corresponds to June-August of 2003
  q = 6
  w = 8
  
  # initialize counter of first max price window. This corresponds to March-May of 2004
  x = 16
  y = 19
  
  for(i in 1:numyrs){ # 17 years: 2003-2019
    
    Pmax <- max(loopdata[x:y, (j+2)])
    Pmin <- min(loopdata[q:w, (j+2)])
    
    range[i,j] <- (Pmax-Pmin)
    
    #increase counters by 12 months
    
    x = (x+12)
    y = (y+12)
    
    q = (q+12)
    w = (w+12)
  }}

grouped_season <- c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013",
                  "2014", "2015", "2016", "2017", "2018")

markets <- colnames(loopdata)

colnames(range) <- markets
range <- cbind(grouped_season, range)
range <- as.data.frame(range)
range <- range[,0:76]

price_shifts <- range %>%
  gather(marketID, Price_Change, 2:76)

price_shifts$Price_Change <- as.numeric(price_shifts$Price_Change)

#   add back in market name and country
market_lookup <- dplyr::select(interpolated_df, market, country, marketID)
market_lookup <- market_lookup[!duplicated(market_lookup), ]

price_shifts_merged <- merge(x = price_shifts, y = market_lookup, by = "marketID", all.x = TRUE)

#   plot a boxplot of post-harvest price changes for each country and growing season
phpc_plot <- ggplot(price_shifts_merged, aes(x = grouped_season, y = Price_Change, fill = country)) +
         geom_boxplot() +
         theme_bw() +
    facet_wrap(~country) +
    theme(legend.position = "none") +
     labs(x = "growing season", y = "phpc") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
phpc_plot



####    step 8 - calculate dependent variable 2 - post-harvest coefficient of variation   ####

#   similar to dv1 this is calculated over the post-harvest window
phcv_data <- interpolated_df

#CV for each post-harvest time period
cv_data_season <- phcv_data %>%
  mutate(grouped_season = case_when(
    date <= "2003-07-01" ~ 2002,
    date <= "2004-07-01" & date > "2003-04-01" ~ 2003,
    date <= "2005-07-01" & date > "2004-04-01" ~ 2004,
    date <= "2006-07-01" & date > "2005-04-01" ~ 2005,
    date <= "2007-07-01" & date > "2006-04-01" ~ 2006,
    date <= "2008-07-01" & date > "2007-04-01" ~ 2007,
    date <= "2009-07-01" & date > "2008-04-01" ~ 2008,
    date <= "2010-07-01" & date > "2009-04-01" ~ 2009,
    date <= "2011-07-01" & date > "2010-04-01" ~ 2010,
    date <= "2012-07-01" & date > "2011-04-01" ~ 2011,
    date <= "2013-07-01" & date > "2012-04-01" ~ 2012,
    date <= "2014-07-01" & date > "2013-04-01" ~ 2013,
    date <= "2015-07-01" & date > "2014-04-01" ~ 2014,
    date <= "2016-07-01" & date > "2015-04-01" ~ 2015,
    date <= "2017-07-01" & date > "2016-04-01" ~ 2016,
    date <= "2018-07-01" & date > "2017-04-01" ~ 2017,
    date <= "2019-07-01" & date > "2018-04-01" ~ 2018,
    date <= "2020-07-01" & date > "2019-04-01" ~ 2019,
    date <= "2021-07-01" & date > "2020-04-01" ~ 2020))

#c  alculate cv for each market year
cv_data_market_year <- cv_data_season %>%
  group_by(grouped_season, marketID) %>%
  summarise(cv = cv(interpolated_usd_price))

#   merge country/market info back in
cv_data_market_year_merged <- merge(x = cv_data_market_year, y = market_lookup, by = "marketID", all.x = TRUE)

#   plot a boxplot of post-harvest coefficient of variation for each country and growing season
phcv_plot <- ggplot(cv_data_market_year_merged, aes(x = as.factor(grouped_season), y = cv, fill = country)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~country) +
  theme(legend.position = "none") +
  labs(x = "growing season", y = "phcv") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
phcv_plot



  ####  step 9 - calculate dependent variable 3 - post-harvest average price    ####

avg_price <- interpolated_df

#   phavg for each post-harvest time period
avg_price <- avg_price %>%
  mutate(grouped_season = case_when(
    date <= "2003-07-01" ~ 2002,
    date <= "2004-07-01" & date > "2003-04-01" ~ 2003,
    date <= "2005-07-01" & date > "2004-04-01" ~ 2004,
    date <= "2006-07-01" & date > "2005-04-01" ~ 2005,
    date <= "2007-07-01" & date > "2006-04-01" ~ 2006,
    date <= "2008-07-01" & date > "2007-04-01" ~ 2007,
    date <= "2009-07-01" & date > "2008-04-01" ~ 2008,
    date <= "2010-07-01" & date > "2009-04-01" ~ 2009,
    date <= "2011-07-01" & date > "2010-04-01" ~ 2010,
    date <= "2012-07-01" & date > "2011-04-01" ~ 2011,
    date <= "2013-07-01" & date > "2012-04-01" ~ 2012,
    date <= "2014-07-01" & date > "2013-04-01" ~ 2013,
    date <= "2015-07-01" & date > "2014-04-01" ~ 2014,
    date <= "2016-07-01" & date > "2015-04-01" ~ 2015,
    date <= "2017-07-01" & date > "2016-04-01" ~ 2016,
    date <= "2018-07-01" & date > "2017-04-01" ~ 2017,
    date <= "2019-07-01" & date > "2018-04-01" ~ 2018,
    date <= "2020-07-01" & date > "2019-04-01" ~ 2019,
    date <= "2021-07-01" & date > "2020-04-01" ~ 2020))

avg_market_year <- avg_price %>%
  group_by(grouped_season, marketID) %>%
  summarise(avg = mean(interpolated_usd_price))

#   merge country/market info back in
avg_market_year_merged <- merge(x = avg_market_year, y = market_lookup, by = "marketID", all.x = TRUE)

#   plot a boxplot of post-harvest average price for each country and growing season
phavg_plot <- ggplot(avg_market_year_merged, aes(x = as.factor(grouped_season), y = avg, fill = country)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~country) +
  theme(legend.position = "none") +
  labs(x = "growing season", y = "phavg") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
phavg_plot

####    step 10 - merge all dependent variable together, and plot ####
final_price_dataset <- rbind(price_shifts_merged, cv_data_market_year_merged, avg_market_year_merged)

#   join plots together
ggarrange(phpc_plot, phcv_plot, phavg_plot, nrow = 1, ncol = 3, common,legend = T)


####    step 11 - add in gps data for markets   ####
#   gps data came from wfp and has been used to calculate climate and land-use covariates in gee and distance metrics in qgis

gps_data <- read.csv("/Users/azimmer/Documents/PhD Geography/Research/Zimmer - Maize Price/WFP Data/GPS Data/MarketsofSouthernAfrica.csv")
gps_data <- dplyr::select(gps_data, Market, Country, Latitude, Longitude)
colnames(gps_data) <- c("market", "country", "latitude", "longitude")

spatial_maize_price_dataset  <- merge(final_price_dataset, gps_data, by = c("market", "country")) #merge GPS locations with main maize price dataset

#   export final price dataset
colnames(final_price_dataset) <- c("marketID", "country", "market", "growing_season", "phpc", "phcv", "phavg")
write_csv(final_price_dataset, "/Users/azimmer/Documents/PhD Geography/Research/Zimmer - Maize Price/Cleaned Maize Price Data w: Vars/final_price_dataset.csv")

####    we now have 1149 observations across 77 markets
#       next steps:     merge dependent variables with climate data, land-use data and distance data generated in gee and qgis
#                       prepare distance weights matrix for spatial regression modeling
#                       run linear and spatial models for post-harvest price patterns vs covariates




























