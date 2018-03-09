########### Superstore Forecasting ###################
###########======================== ##################

##### Required Packages #####

library(DescTools)
library(ggplot2)
library(stringr)
library(reshape2)
library(gridExtra)
library(dplyr)
library(zoo)
library(forecast)
library(tseries)
library(graphics)

#### Import the data in R
Superstore <- read.csv("Global Superstore.csv",stringsAsFactors = F)
dim(Superstore) #### 51290 Rows and 24 Coloums
str(Superstore)
sum(is.na(Superstore))

### identifing the Missing Values

colSums(is.na(Superstore)) #### 80% NA's is available in PostalCode

##removing postal.code column from dataframe 
Superstore1 <- Superstore[,-12]
sum(is.na(Superstore1))
##[1] 0

### Identified Dates having different formet converting into R formet

str(Superstore1)
Superstore1$Order.Date <- as.Date(Superstore1$Order.Date,format = "%d-%m-%Y")
Superstore1$Ship.Date <- as.Date(Superstore1$Ship.Date,format="%d-%m-%Y")

##Creating 2 columns 1. having month-year info 2. month info for each mrkt_sgmnt
Superstore1$order_mnthyear <- format(Superstore1$Order.Date,"%m-%Y")
Superstore1$order_month <- format(Superstore1$Order.Date,"%m")
Superstore1$order_month <- as.numeric(Superstore1$order_month)
Superstore1$order_month <- month.abb[Superstore1$order_month]

#creating a new column by combining segment and market

Superstore1$Mrkt_Sgmnt <- paste(str_to_title(Superstore1$Market),str_to_title(Superstore1$Segment),sep="_")
Superstore3 <- Superstore1[order(as.Date(Superstore1$Order.Date, format="%d-%m-%Y")),]

#converting order_mnthyear to factor
Superstore3$order_mnthyear <- as.factor(Superstore3$order_mnthyear)
Superstore3$order_month <- as.factor(Superstore3$order_month)

## Data Quality Checking
# Row ID
Desc(Superstore3$Row.ID)
table(duplicated(Superstore3$Row.ID))### No Duplicates Found (False : 51290)

# Order.ID
Desc(Superstore3$Order.ID) 
table(duplicated(Superstore3$Order.ID))### Duplicates Found, it's Valid because in Every order they givem mulyiple item.

##### EDA Process #################
### Based on the businees Problem need to work on market and Category to understand the Insights
# Market & Segments:
New_Global = Superstore3[,c("Market","Segment","Sales","Profit","order_month")]

Global_SP <- aggregate(. ~ Market + Segment+order_month, data = New_Global,FUN = sum)
Global_1 <- melt(Global_SP, id.vars = c("Market", "Segment","order_month"))

ggplot(Global_1, aes(x = Segment, y = value, group = variable, colour=variable))+
  geom_point()+geom_line()+
  facet_grid(Market~ order_month)+theme_bw()+
  theme(axis.text.x = element_text(angle = 90),legend.position="top")+
  ggtitle("Market and  Segment\nTrend lines of sales and Profit") +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Segment") + ylab("Sales Vs Profit")


### EDA Process for Sigment & Market wise
################## Africa Market ########################
## Africa Market and Consumer Segment: 

Africa_Consumer = New_Global %>% filter(New_Global$Market=="Africa" & New_Global$Segment=="Consumer")
Africa_Consumer <- aggregate(. ~ Market + Segment+order_month, data = Africa_Consumer,FUN = sum)
Africa_Consumer <- melt(Africa_Consumer, id.vars = c("Market", "Segment","order_month"))

A1 = ggplot(data = Africa_Consumer, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("Africa Market and Consumer Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

## Africa Market and Corporate Segment: 

Africa_Corporate = New_Global %>% filter(New_Global$Market=="Africa" & New_Global$Segment=="Corporate")
Africa_Corporate <- aggregate(. ~ Market + Segment+order_month, data = Africa_Corporate,FUN = sum)
Africa_Corporate <- melt(Africa_Corporate, id.vars = c("Market", "Segment","order_month"))

A2 = ggplot(data = Africa_Corporate, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("Africa Market and Corporate Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

## Africa Market and Home Office  Segment: 

Africa_Home_office = New_Global %>% filter(New_Global$Market=="Africa" & New_Global$Segment=="Home Office")
Africa_Home_office <- aggregate(. ~ Market + Segment+order_month, data = Africa_Home_office,FUN = sum)
Africa_Home_office <- melt(Africa_Home_office, id.vars = c("Market", "Segment","order_month"))

A3 = ggplot(data = Africa_Home_office, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("Africa Market and Home_office Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

grid.arrange(A1,A2,A3,ncol=2, top="Africa Market Segments wise")


############## APAC MARKET #########################
# APAC Market and Consumer segment

APAC_Consumer = New_Global %>% filter(New_Global$Market=="APAC" & New_Global$Segment=="Consumer")
APAC_Consumer <- aggregate(. ~ Market + Segment+order_month, data = APAC_Consumer,FUN = sum)
APAC_Consumer <- melt(APAC_Consumer, id.vars = c("Market", "Segment","order_month"))

AP1 = ggplot(data = APAC_Consumer, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("APAC Market and Consumer Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))


# APAC Market and Corporate segment

AP2 = APAC_Corporate = New_Global %>% filter(New_Global$Market=="APAC" & New_Global$Segment=="Corporate")
APAC_Corporate <- aggregate(. ~ Market + Segment+order_month, data = APAC_Corporate,FUN = sum)
APAC_Corporate <- melt(APAC_Corporate, id.vars = c("Market", "Segment","order_month"))

AP2 = ggplot(data = APAC_Corporate, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("APAC Market and Corporate Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))


# APAC Market and Home Office segment

APAC_Home_Office = New_Global %>% filter(New_Global$Market=="APAC" & New_Global$Segment=="Home Office")
APAC_Home_Office <- aggregate(. ~ Market + Segment+order_month, data = APAC_Home_Office,FUN = sum)
APAC_Home_Office <- melt(APAC_Home_Office, id.vars = c("Market", "Segment","order_month"))

AP3 = ggplot(data = APAC_Home_Office, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("APAC Market and Home_Office Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

grid.arrange(AP1,AP2,AP3,ncol=2,top="APAC Market Segments wise")

############# Canada MARKET ##################
# Canada Market and Consumer segment

Canada_Consumer = New_Global %>% filter(New_Global$Market=="Canada" & New_Global$Segment=="Consumer")
Canada_Consumer <- aggregate(. ~ Market + Segment+order_month, data = Canada_Consumer,FUN = sum)
Canada_Consumer <- melt(Canada_Consumer, id.vars = c("Market", "Segment","order_month"))

CC1 = ggplot(data = Canada_Consumer, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("Canada Market and Consumer Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

# Canada Market and Corporate segment

Canada_Corporate = New_Global %>% filter(New_Global$Market=="Canada" & New_Global$Segment=="Corporate")
Canada_Corporate <- aggregate(. ~ Market + Segment+order_month, data = Canada_Corporate,FUN = sum)
Canada_Corporate <- melt(Canada_Corporate, id.vars = c("Market", "Segment","order_month"))

CC2 = ggplot(data = Canada_Corporate, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("Canada Market and Corporate Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

# Canada Market and Home Office segment

Canada_Home_Office = New_Global %>% filter(New_Global$Market=="Canada" & New_Global$Segment=="Home Office")
Canada_Home_Office <- aggregate(. ~ Market + Segment+order_month, data = Canada_Home_Office,FUN = sum)
Canada_Home_Office <- melt(Canada_Home_Office, id.vars = c("Market", "Segment","order_month"))

CC3 = ggplot(data = Canada_Home_Office, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("Canada Market and Home_Office Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

grid.arrange(CC1,CC2,CC3,ncol=2,top="Canada Market Segments wise")

###############EMEA MARKET ####################
# EMEA Market and Consumer segment
EMEA_Consumer = New_Global %>% filter(New_Global$Market=="EMEA" & New_Global$Segment=="Consumer")
EMEA_Consumer <- aggregate(. ~ Market + Segment+order_month, data = EMEA_Consumer,FUN = sum)
EMEA_Consumer <- melt(EMEA_Consumer, id.vars = c("Market", "Segment","order_month"))

EM1 = ggplot(data = EMEA_Consumer, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("EMEA Market and Consumer Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

# EMEA Market and Corporate segment

EMEA_Corporate = New_Global %>% filter(New_Global$Market=="EMEA" & New_Global$Segment=="Corporate")
EMEA_Corporate <- aggregate(. ~ Market + Segment+order_month, data = EMEA_Corporate,FUN = sum)
EMEA_Corporate <- melt(EMEA_Corporate, id.vars = c("Market", "Segment","order_month"))

EM2 = ggplot(data = EMEA_Corporate, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("EMEA Market and Corporate Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

# EMEA Market and Home Office segment

EMEA_Home_Office = New_Global %>% filter(New_Global$Market=="EMEA" & New_Global$Segment=="Home Office")
EMEA_Home_Office <- aggregate(. ~ Market + Segment+order_month, data = EMEA_Home_Office,FUN = sum)
EMEA_Home_Office <- melt(EMEA_Home_Office, id.vars = c("Market", "Segment","order_month"))

EM3 = ggplot(data = EMEA_Home_Office, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("EMEA Market and Home_Office Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

grid.arrange(EM1,EM2,EM3,ncol=2,top="EMEA Market Segments wise")

###############EU MARKET ####################
# EU Market and Consumer segment
EU_Consumer = New_Global %>% filter(New_Global$Market=="EU" & New_Global$Segment=="Consumer")
EU_Consumer <- aggregate(. ~ Market + Segment+order_month, data = EU_Consumer,FUN = sum)
EU_Consumer <- melt(EU_Consumer, id.vars = c("Market", "Segment","order_month"))

EU1 = ggplot(data = EU_Consumer, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("EU Market and Consumer Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

# EU Market and Corporate segment

EU_Corporate = New_Global %>% filter(New_Global$Market=="EU" & New_Global$Segment=="Corporate")
EU_Corporate <- aggregate(. ~ Market + Segment+order_month, data = EU_Corporate,FUN = sum)
EU_Corporate <- melt(EU_Corporate, id.vars = c("Market", "Segment","order_month"))

EU2 = ggplot(data = EU_Corporate, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("EU Market and Corporate Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

# EU Market and Home Office segment

EU_Home_Office = New_Global %>% filter(New_Global$Market=="EU" & New_Global$Segment=="Home Office")
EU_Home_Office <- aggregate(. ~ Market + Segment+order_month, data = EU_Home_Office,FUN = sum)
EU_Home_Office <- melt(EU_Home_Office, id.vars = c("Market", "Segment","order_month"))

EU3 = ggplot(data = EU_Home_Office, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("EU Market and Home_Office Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

grid.arrange(EU1,EU2,EU3,ncol=2,top="EU Market Segments wise")


###############LATAM MARKET ####################
# LATAM Market and Consumer segment
LATAM_Consumer = New_Global %>% filter(New_Global$Market=="LATAM" & New_Global$Segment=="Consumer")
LATAM_Consumer <- aggregate(. ~ Market + Segment+order_month, data = LATAM_Consumer,FUN = sum)
LATAM_Consumer <- melt(LATAM_Consumer, id.vars = c("Market", "Segment","order_month"))

LATAM1 = ggplot(data = LATAM_Consumer, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("LATAM Market and Consumer Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

# LATAM Market and Corporate segment

LATAM_Corporate = New_Global %>% filter(New_Global$Market=="LATAM" & New_Global$Segment=="Corporate")
LATAM_Corporate <- aggregate(. ~ Market + Segment+order_month, data = LATAM_Corporate,FUN = sum)
LATAM_Corporate <- melt(LATAM_Corporate, id.vars = c("Market", "Segment","order_month"))

LATAM2 = ggplot(data = LATAM_Corporate, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("LATAM Market and Corporate Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

# LATAM Market and Home Office segment

LATAM_Home_Office = New_Global %>% filter(New_Global$Market=="LATAM" & New_Global$Segment=="Home Office")
LATAM_Home_Office <- aggregate(. ~ Market + Segment+order_month, data = LATAM_Home_Office,FUN = sum)
LATAM_Home_Office <- melt(LATAM_Home_Office, id.vars = c("Market", "Segment","order_month"))

LATAM3 = ggplot(data = LATAM_Home_Office, aes(x = order_month, y = value, group = variable, colour=variable))+
  geom_point()+geom_line(size=1)+labs(x="Month Wise Trend", y="Sales Vs Profit")+
  theme(axis.text.x = element_text(angle = 90),legend.position="top", legend.direction="horizontal")+
  ggtitle("LATAM Market and Home_Office Segment\nTrend lines of sales and Profit")+
  scale_x_discrete(limits = month.abb)+ theme(plot.title = element_text(hjust = 0.5))

grid.arrange(LATAM1,LATAM2,LATAM3,ncol=2,top="LATAM Market Segments wise")

##### EDA Process End #########

#grouping the data by Mrkt_Sgmnt,order_month
groupedSuperstoredata <- Superstore3%>%
  group_by(Mrkt_Sgmnt,order_month)%>%
  summarise(Monthly_Sales=sum(Sales),Monthly_Quantity=sum(Quantity),Monthly_Profit=sum(Profit))%>%
  arrange(desc(Monthly_Profit))
View(groupedSuperstoredata)

##Computing coeffiecient of Variation and selecting top 2 Market Segment 
mrkt_sgmnt_COV <- groupedSuperstoredata%>%
  group_by(Mrkt_Sgmnt)%>%
  summarise(covar=(sd(Monthly_Profit)/mean(Monthly_Profit)))%>%
  arrange(-desc(covar))
##After checking the profit and covar we can select Eu_Consumer,Apac_Consumer as top 2 profitable market segments
##Creating a new df having data for these 2 market segment only
Masterdata <- Superstore3%>%
  filter(Mrkt_Sgmnt=="Eu_Consumer" | Mrkt_Sgmnt=="Apac_Consumer")
##creating a df having profit,sales,quantity for these segments
Masterdata1 <- Masterdata %>%
  group_by(Mrkt_Sgmnt,order_mnthyear)%>%
  summarise(Mnthyear_Sales=sum(Sales),Mnthyear_Quantity=sum(Quantity),Mnthyear_Profit=sum(Profit))
##Creating 2 df for Apac_Consumer and Eu_Consumer market segments 
Apac_Consumer <- Masterdata1%>%
                 filter(Mrkt_Sgmnt=="Apac_Consumer")
				 View(Apac_Consumer)				 
Apac_Consumer$order_mnthyear <- as.yearmon(Apac_Consumer$order_mnthyear, format = "%m-%Y")

Apac_Consumer <- Apac_Consumer[order(as.Date(Apac_Consumer$order_mnthyear, format="%M %Y")),]

Eu_Consumer <-   Masterdata1%>%
                 filter(Mrkt_Sgmnt=="Eu_Consumer")
Eu_Consumer$order_mnthyear <- as.yearmon(Eu_Consumer$order_mnthyear, format = "%m-%Y")

Eu_Consumer <- Eu_Consumer[order(as.Date(Eu_Consumer$order_mnthyear, format="%M %Y")),]
View(Eu_Consumer)
##-------------------------------------------------------------------------------------------------
##starting with Eu_Consumer_sales
##As asked, separating out last 6 months data for testing, and building model on first 42 months data
##Starting with Eu_Consumer data sales data
Eu_Consumer$Month_Year <- seq(1:48) 
Eu_Consumer_sales <- Eu_Consumer[,c(6,3)]
#Eu_Consumer_sales$Mnthyear_Sales <- log(Eu_Consumer_sales$Mnthyear_Sales)
View(Eu_Consumer_sales)
Eu_Consumer_tssalesdata <- ts(Eu_Consumer_sales$Mnthyear_Sales)
View(Eu_Consumer_tssalesdata)
Eu_Consumer_sales_traindata <- Eu_Consumer_sales[1:42,]
View(Eu_Consumer_sales_traindata)
Eu_Consumer_sales_testdata <- Eu_Consumer_sales[43:48,]
View(Eu_Consumer_sales_testdata)
Eu_Consumer_sales_traindata_ts <- ts(Eu_Consumer_sales_traindata$Mnthyear_Sales)
plot(Eu_Consumer_sales_traindata_ts)

detach("package:dplyr", unload=TRUE)
#Smoothing the timeseries - Using Moving Average method
w <-1
Eu_Consumer_sales_smoothedseries <- filter(Eu_Consumer_sales_traindata_ts,
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- Eu_Consumer_sales_smoothedseries[w+2] - Eu_Consumer_sales_smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  Eu_Consumer_sales_smoothedseries[i] <- Eu_Consumer_sales_smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(Eu_Consumer_sales_traindata_ts)
diff <- Eu_Consumer_sales_smoothedseries[n-w] - Eu_Consumer_sales_smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  Eu_Consumer_sales_smoothedseries[i] <- Eu_Consumer_sales_smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in_Eu_Consumer_sales <- Eu_Consumer_sales_traindata$Month_Year

lines(Eu_Consumer_sales_smoothedseries, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

Eu_Consumer_sales_smootheddf <- as.data.frame(cbind(timevals_in_Eu_Consumer_sales, as.vector(Eu_Consumer_sales_smoothedseries)))
colnames(Eu_Consumer_sales_smootheddf) <- c('Month_Year', 'Sales')
View(Eu_Consumer_sales_smootheddf)

#Now, let's fit a model on data

Eu_Consumer_sales_lmfit <- lm(Sales ~ poly(Month_Year,1) + Month_Year
, data=Eu_Consumer_sales_smootheddf)
Eu_Consumer_sales_global_pred <- predict(Eu_Consumer_sales_lmfit, Month_Year=timevals_in_Eu_Consumer_sales)
summary(Eu_Consumer_sales_global_pred)
plot(Eu_Consumer_sales_traindata_ts)
lines(Eu_Consumer_sales_global_pred, col='green', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

Eu_Consumer_sales_local_pred <- Eu_Consumer_sales_traindata_ts-Eu_Consumer_sales_global_pred
plot(Eu_Consumer_sales_local_pred, col='red', type = "l")
acf(Eu_Consumer_sales_local_pred)
acf(Eu_Consumer_sales_local_pred, type="partial")
##We can see that the ACF, PACF plots crosses the safe area which means it is not a stationery series let us predict the model using auto_arima
Eu_Consumer_sales_armafit <- auto.arima(Eu_Consumer_sales_local_pred)

tsdiag(Eu_Consumer_sales_armafit)
Eu_Consumer_sales_armafit

#Series: Eu_Consumer_sales_local_pred 
#ARIMA(0,0,0) with zero mean 

#sigma^2 estimated as 1.37e+08:  log likelihood=-453.04
#AIC=908.08   AICc=908.18   BIC=909.82

#We'll check if the residual series is white noise

Eu_Consumer_sales_resi <- Eu_Consumer_sales_local_pred-fitted(Eu_Consumer_sales_armafit)

adf.test(Eu_Consumer_sales_resi,alternative = "stationary")
#Augmented Dickey-Fuller Test

#data:  Eu_Consumer_sales_resi
#Dickey-Fuller = -3.7581, Lag order = 3, p-value = 0.03254
#alternative hypothesis: stationary
kpss.test(Eu_Consumer_sales_resi)
#KPSS Level = 0.1046, Truncation lag parameter = 1, p-value = 0.1


##From kpss and adf test we can conclude that residual series is white noise.
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
Eu_Consumer_sales_outdata <- data.frame(Eu_Consumer_sales_testdata)

Eu_Consumer_sales_timevals_out <- Eu_Consumer_sales_outdata$Month_Year

Eu_Consumer_sales_global_pred_out <- predict(Eu_Consumer_sales_lmfit,data.frame(Month_Year =Eu_Consumer_sales_timevals_out))

Eu_Consumer_sales_fcast <- Eu_Consumer_sales_global_pred_out


#Now, let's compare our prediction with the actual values, using MAPE

Eu_Consumer_sales_MAPE_class_dec <- accuracy(Eu_Consumer_sales_fcast,Eu_Consumer_sales_outdata[,2])[5]
Eu_Consumer_sales_MAPE_class_dec
# [1] 28.10294

#----------------------------------------------------------------------------------------------------
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

Eu_Consumer_sales_class_dec_pred <- c(ts(Eu_Consumer_sales_global_pred),ts(Eu_Consumer_sales_global_pred_out))
plot(Eu_Consumer_tssalesdata, col = "black")
lines(Eu_Consumer_sales_class_dec_pred, col = "red")
#-----------------------------------------------------------------------------------------------------------------------
#So, that was classical decomposition, now let's do an ARIMA fit

Eu_Consumer_sales_autoarima <- auto.arima(Eu_Consumer_sales_traindata_ts,stepwise=FALSE,approx=FALSE)
Eu_Consumer_sales_autoarima
tsdiag(Eu_Consumer_sales_autoarima)
plot(Eu_Consumer_sales_autoarima$x, col="black")
lines(fitted(Eu_Consumer_sales_autoarima), col="red")

#Again, let's check if the residual series is white noise

Eu_Consumer_sales_resi_auto_arima <- Eu_Consumer_sales_traindata_ts - fitted(Eu_Consumer_sales_autoarima)

adf.test(Eu_Consumer_sales_resi_auto_arima,alternative = "stationary")

#Dickey-Fuller = -4.0012, Lag order = 3, p-value = 0.01917
kpss.test(Eu_Consumer_sales_resi_auto_arima)
#KPSS Level = 0.12543, Truncation lag parameter = 1, p-value = 0.1

#Also, let's evaluate the model using MAPE
Eu_Consumer_sales_fcast_auto_arima <- predict(Eu_Consumer_sales_autoarima, n.ahead = 6)

Eu_Consumer_sales_MAPE_auto_arima <- accuracy(Eu_Consumer_sales_fcast_auto_arima$pred,Eu_Consumer_sales_outdata[,2])[5]
Eu_Consumer_sales_MAPE_auto_arima 
##[1] 30.6759 ((MAPE is higher in comparison to classical decomposition method)

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

Eu_Consumer_sales_auto_arima_pred <- c(fitted(Eu_Consumer_sales_autoarima),ts(Eu_Consumer_sales_fcast_auto_arima$pred))
plot(Eu_Consumer_sales_traindata_ts, col = "black")
lines(Eu_Consumer_sales_auto_arima_pred, col = "green")

##Forecasting the sales in Eu_Consumer market segment for next 6 months, as model made using classical decomposition is having lower MAPE value we will use the same model for forecasting

Eu_Consumer_sales_forecast_month <- seq(49,54, by = 1)
Eu_Consumer_sales_global_forecast <- predict(Eu_Consumer_sales_lmfit,data.frame(Month_Year =Eu_Consumer_sales_forecast_month))
 #Eu_Consumer_sales_global_forecast
 #      1        2        3        4        5        6 
#42806.42 43308.28 43810.14 44311.99 44813.85 45315.71 

##__________________________________________________________________________________________________________________________________________
##Applying classical decomposition technique on Eu_Consumer_Quantity data
##As asked, separating out last 6 months data for testing, and building model on first 42 months data
##Starting with Eu_Consumer data Quantity data
library(dplyr)
Eu_Consumer_Quantity <- Eu_Consumer[,c(6,4)]
#Eu_Consumer_Quantity$Mnthyear_Quantity <- log(Eu_Consumer_Quantity$Mnthyear_Quantity)
View(Eu_Consumer_Quantity)
Eu_Consumer_tsQuantitydata <- ts(Eu_Consumer_Quantity$Mnthyear_Quantity)
View(Eu_Consumer_tsQuantitydata)
Eu_Consumer_Quantity_traindata <- Eu_Consumer_Quantity[1:42,]
View(Eu_Consumer_Quantity_traindata)
Eu_Consumer_Quantity_testdata <- Eu_Consumer_Quantity[43:48,]
View(Eu_Consumer_Quantity_testdata)
Eu_Consumer_Quantity_traindata_ts <- ts(Eu_Consumer_Quantity_traindata$Mnthyear_Quantity)
plot(Eu_Consumer_Quantity_traindata_ts)

detach("package:dplyr", unload=TRUE)
#Smoothing the timeseries - Using Moving Average method
w <-1
Eu_Consumer_Quantity_smoothedseries <- filter(Eu_Consumer_Quantity_traindata_ts,
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- Eu_Consumer_Quantity_smoothedseries[w+2] - Eu_Consumer_Quantity_smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  Eu_Consumer_Quantity_smoothedseries[i] <- Eu_Consumer_Quantity_smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(Eu_Consumer_Quantity_traindata_ts)
diff <- Eu_Consumer_Quantity_smoothedseries[n-w] - Eu_Consumer_Quantity_smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  Eu_Consumer_Quantity_smoothedseries[i] <- Eu_Consumer_Quantity_smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in_Eu_Consumer_Quantity <- Eu_Consumer_Quantity_traindata$Month_Year

lines(Eu_Consumer_Quantity_smoothedseries, col="blue", lwd=2)



#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

Eu_Consumer_Quantity_smootheddf <- as.data.frame(cbind(timevals_in_Eu_Consumer_Quantity, as.vector(Eu_Consumer_Quantity_smoothedseries)))
colnames(Eu_Consumer_Quantity_smootheddf) <- c('Month_Year', 'Quantity')
View(Eu_Consumer_Quantity_smootheddf)

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

Eu_Consumer_Quantity_lmfit <- lm(Quantity ~sin(0.5*Month_Year-pi/2) * poly(Month_Year,1)+cos(0.5*Month_Year-pi/2)
, data=Eu_Consumer_Quantity_smootheddf)

Eu_Consumer_Quantity_global_pred <- predict(Eu_Consumer_Quantity_lmfit, Month_Year=timevals_in_Eu_Consumer_Quantity)
summary(Eu_Consumer_Quantity_global_pred)
plot(Eu_Consumer_Quantity_traindata_ts)
#lines(Eu_Consumer_Quantity_smoothedseries, col="blue", lwd=2)
lines(Eu_Consumer_Quantity_global_pred, col='red')

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

Eu_Consumer_Quantity_local_pred <- Eu_Consumer_Quantity_traindata_ts-Eu_Consumer_Quantity_global_pred
plot(Eu_Consumer_Quantity_local_pred, col='red', type = "l")
acf(Eu_Consumer_Quantity_local_pred)
acf(Eu_Consumer_Quantity_local_pred, type="partial")
##We can see that the ACF, PACF plots crosses the safe area which means it is not a stationery series let us predict the model using auto_arima
Eu_Consumer_Quantity_armafit <- auto.arima(Eu_Consumer_Quantity_local_pred)

tsdiag(Eu_Consumer_Quantity_armafit)
Eu_Consumer_Quantity_armafit

#ARIMA(2,0,0) with zero mean 

#Coefficients:
#          ar1      ar2
#      -0.5392  -0.5255
#s.e.   0.1284   0.1247

#sigma^2 estimated as 9263:  log likelihood=-250.77
#AIC=507.54   AICc=508.17   BIC=512.75

#We'll check if the residual series is white noise

Eu_Consumer_Quantity_resi <- Eu_Consumer_Quantity_local_pred-fitted(Eu_Consumer_Quantity_armafit)

adf.test(Eu_Consumer_Quantity_resi,alternative = "stationary")

kpss.test(Eu_Consumer_Quantity_resi)

##From kpss and adf test we can conclude that residual series is white noise.
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
library(dplyr)
Eu_Consumer_Quantity_outdata <- data.frame(Eu_Consumer_Quantity_testdata)

Eu_Consumer_Quantity_timevals_out <- Eu_Consumer_Quantity_outdata$Month_Year

Eu_Consumer_Quantity_global_pred_out <- predict(Eu_Consumer_Quantity_lmfit,data.frame(Month_Year =Eu_Consumer_Quantity_timevals_out))

Eu_Consumer_Quantity_fcast <- Eu_Consumer_Quantity_global_pred_out

#fcast <- Eu_Consumer_Quantity_global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

Eu_Consumer_Quantity_MAPE_class_dec <- accuracy(Eu_Consumer_Quantity_fcast,Eu_Consumer_Quantity_outdata[,2])[5]
Eu_Consumer_Quantity_MAPE_class_dec
#  [1] 29.37272


#----------------------------------------------------------------------------------------------------
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

Eu_Consumer_Quantity_class_dec_pred <- c(ts(Eu_Consumer_Quantity_global_pred),ts(Eu_Consumer_Quantity_global_pred_out))
plot(Eu_Consumer_tsQuantitydata, col = "black")
lines(Eu_Consumer_Quantity_class_dec_pred, col = "red")
#-----------------------------------------------------------------------------------------------------------------------
#So, that was classical decomposition, now let's do an ARIMA fit

Eu_Consumer_Quantity_autoarima <- auto.arima(Eu_Consumer_Quantity_traindata_ts,stepwise=FALSE,approx=FALSE)
Eu_Consumer_Quantity_autoarima
tsdiag(Eu_Consumer_Quantity_autoarima)
plot(Eu_Consumer_Quantity_autoarima$x, col="black")
lines(fitted(Eu_Consumer_Quantity_autoarima), col="red")

#Again, let's check if the residual series is white noise

Eu_Consumer_Quantity_resi_auto_arima <- Eu_Consumer_Quantity_traindata_ts - fitted(Eu_Consumer_Quantity_autoarima)

adf.test(Eu_Consumer_Quantity_resi_auto_arima,alternative = "stationary")
kpss.test(Eu_Consumer_Quantity_resi_auto_arima)

#Also, let's evaluate the model using MAPE
Eu_Consumer_Quantity_fcast_auto_arima <- predict(Eu_Consumer_Quantity_autoarima, n.ahead = 6)

Eu_Consumer_Quantity_MAPE_auto_arima <- accuracy(Eu_Consumer_Quantity_fcast_auto_arima$pred,Eu_Consumer_Quantity_outdata[,2])[5]
Eu_Consumer_Quantity_MAPE_auto_arima 
##[1] 30.13319 ((MAPE is higher in comparison to classical decomposition method)

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

Eu_Consumer_Quantity_auto_arima_pred <- c(fitted(Eu_Consumer_Quantity_autoarima),ts(Eu_Consumer_Quantity_fcast_auto_arima$pred))
plot(Eu_Consumer_Quantity_traindata_ts, col = "black")
lines(Eu_Consumer_Quantity_auto_arima_pred, col = "red")
##Forecasting the Quantity in Eu_Consumer market segment for next 6 months, as model made using classical decomposition is having lower MAPE value we will use the same model for forecasting

Eu_Consumer_Quantity_forecast_month <- seq(49,54, by = 1)
Eu_Consumer_Quantity_global_forecast <- predict(Eu_Consumer_Quantity_lmfit,data.frame(Month_Year =Eu_Consumer_Quantity_forecast_month))
 #1        2        3        4        5        6 
#525.1833 480.4738 456.3460 461.2320 496.3046 555.0161 
##__________________________________________________________________________________________________________________________________________
##Applying classical decomposition technique on Apac_Consumer_sales data
##As asked, separating out last 6 months data for testing, and building model on first 42 months data
##Starting with Apac_Consumer sales data
library(dplyr)
Apac_Consumer$Month_Year <- seq(1:48) 
Apac_Consumer_sales <- Apac_Consumer[,c(6,3)]
#Apac_Consumer_sales$Mnthyear_Sales <- log(Apac_Consumer_sales$Mnthyear_Sales)
View(Apac_Consumer_sales)
Apac_Consumer_tssalesdata <- ts(Apac_Consumer_sales$Mnthyear_Sales)
View(Apac_Consumer_tssalesdata)
Apac_Consumer_sales_traindata <- Apac_Consumer_sales[1:42,]
View(Apac_Consumer_sales_traindata)
Apac_Consumer_sales_testdata <- Apac_Consumer_sales[43:48,]
View(Apac_Consumer_sales_testdata)
Apac_Consumer_sales_traindata_ts <- ts(Apac_Consumer_sales_traindata$Mnthyear_Sales)
plot(Apac_Consumer_sales_traindata_ts)

detach("package:dplyr", unload=TRUE)
#Smoothing the timeseries - Using Moving Average method
w <-1
Apac_Consumer_sales_smoothedseries <- filter(Apac_Consumer_sales_traindata_ts,
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- Apac_Consumer_sales_smoothedseries[w+2] - Apac_Consumer_sales_smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  Apac_Consumer_sales_smoothedseries[i] <- Apac_Consumer_sales_smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(Apac_Consumer_sales_traindata_ts)
diff <- Apac_Consumer_sales_smoothedseries[n-w] - Apac_Consumer_sales_smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  Apac_Consumer_sales_smoothedseries[i] <- Apac_Consumer_sales_smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in_Apac_Consumer_sales <- Apac_Consumer_sales_traindata$Month_Year

lines(Apac_Consumer_sales_smoothedseries, col="blue")


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

Apac_Consumer_sales_smootheddf <- as.data.frame(cbind(timevals_in_Apac_Consumer_sales, as.vector(Apac_Consumer_sales_smoothedseries)))
colnames(Apac_Consumer_sales_smootheddf) <- c('Month_Year', 'Sales')
View(Apac_Consumer_sales_smootheddf)

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

Apac_Consumer_sales_lmfit <- lm(Sales ~ sin(0.5*Month_Year) * poly(Month_Year,1), data=Apac_Consumer_sales_smootheddf)
Apac_Consumer_sales_global_pred <- predict(Apac_Consumer_sales_lmfit, Month_Year=timevals_in_Apac_Consumer_sales)
summary(Apac_Consumer_sales_global_pred)
plot(Apac_Consumer_sales_traindata_ts)
lines(Apac_Consumer_sales_global_pred, col='red')

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

Apac_Consumer_sales_local_pred <- Apac_Consumer_sales_traindata_ts-Apac_Consumer_sales_global_pred
plot(Apac_Consumer_sales_local_pred, col='red', type = "l")
acf(Apac_Consumer_sales_local_pred)
acf(Apac_Consumer_sales_local_pred, type="partial")
##We can see that the ACF, PACF plots are in safe area which means it is stationery series, however let us apply auto arima to see
Apac_Consumer_sales_armafit <- auto.arima(Apac_Consumer_sales_local_pred)

tsdiag(Apac_Consumer_sales_armafit)
Apac_Consumer_sales_armafit

#Series: Apac_Consumer_sales_local_pred 
#ARIMA(0,0,0) with zero mean 

#sigma^2 estimated as 111869421:  log likelihood=-448.79
#AIC=899.57   AICc=899.67   BIC=901.31

#We'll check if the residual series is white noise

Apac_Consumer_sales_resi <- Apac_Consumer_sales_local_pred-fitted(Apac_Consumer_sales_armafit)

adf.test(Apac_Consumer_sales_resi,alternative = "stationary")
#Dickey-Fuller = -4.4937, Lag order = 3, p-value = 0.01
kpss.test(Apac_Consumer_sales_resi)
#KPSS Level = 0.035958, Truncation lag parameter = 1, p-value = 0.1


##From kpss and adf test we can conclude that residual series is white noise.
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
Apac_Consumer_sales_outdata <- data.frame(Apac_Consumer_sales_testdata)

Apac_Consumer_sales_timevals_out <- Apac_Consumer_sales_outdata$Month_Year

Apac_Consumer_sales_global_pred_out <- predict(Apac_Consumer_sales_lmfit,data.frame(Month_Year =Apac_Consumer_sales_timevals_out))

Apac_Consumer_sales_fcast <- Apac_Consumer_sales_global_pred_out


#Now, let's compare our prediction with the actual values, using MAPE

Apac_Consumer_sales_MAPE_class_dec <- accuracy(Apac_Consumer_sales_fcast,Apac_Consumer_sales_outdata[,2])[5]
Apac_Consumer_sales_MAPE_class_dec
#[1] [1] 22.29696

#----------------------------------------------------------------------------------------------------
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

Apac_Consumer_sales_class_dec_pred <- c(ts(Apac_Consumer_sales_global_pred),ts(Apac_Consumer_sales_global_pred_out))
plot(Apac_Consumer_tssalesdata, col = "black")
lines(Apac_Consumer_sales_class_dec_pred, col = "red")
#-----------------------------------------------------------------------------------------------------------------------
#So, that was classical decomposition, now let's do an ARIMA fit

Apac_Consumer_sales_autoarima <- auto.arima(Apac_Consumer_sales_traindata_ts)
Apac_Consumer_sales_autoarima
tsdiag(Apac_Consumer_sales_autoarima)
plot(Apac_Consumer_sales_autoarima$x, col="black")
lines(fitted(Apac_Consumer_sales_autoarima), col="red")

#Again, let's check if the residual series is white noise

Apac_Consumer_sales_resi_auto_arima <- Apac_Consumer_sales_traindata_ts - fitted(Apac_Consumer_sales_autoarima)

adf.test(Apac_Consumer_sales_resi_auto_arima,alternative = "stationary")
kpss.test(Apac_Consumer_sales_resi_auto_arima)

#Also, let's evaluate the model using MAPE
Apac_Consumer_sales_fcast_auto_arima <- predict(Apac_Consumer_sales_autoarima, n.ahead = 6)

Apac_Consumer_sales_MAPE_auto_arima <- accuracy(Apac_Consumer_sales_fcast_auto_arima$pred,Apac_Consumer_sales_outdata[,2])[5]
Apac_Consumer_sales_MAPE_auto_arima 
##[1] 27.68952((MAPE is higher in comparison to classical decomposition method)

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

Apac_Consumer_sales_auto_arima_pred <- c(fitted(Apac_Consumer_sales_autoarima),ts(Apac_Consumer_sales_fcast_auto_arima$pred))
plot(Apac_Consumer_sales_traindata_ts, col = "black")
lines(Apac_Consumer_sales_auto_arima_pred, col = "red")
##Forecasting the Sales in Apac_Consumer market segment for next 6 months, as model made using classical decomposition is having lower MAPE value we will use the same model for forecasting

Apac_Consumer_sales_forecast_month <- seq(49,54, by = 1)
Apac_Consumer_sales_global_forecast <- predict(Apac_Consumer_sales_lmfit,data.frame(Month_Year =Apac_Consumer_sales_forecast_month))
# 1        2        3        4        5        6 
#56102.73 54588.54 52980.98 51832.88 51571.23 52396.74 

##__________________________________________________________________________________________________________________________________________
##Applying classical decomposition technique on Apac_Consumer_Quantity data
##As asked, separating out last 6 months data for testing, and building model on first 42 months data
##Starting with Apac_Consumer data Quantity data
library(dplyr)
Apac_Consumer_Quantity <- Apac_Consumer[,c(6,4)]
#Apac_Consumer_Quantity$Mnthyear_Quantity <- log(Apac_Consumer_Quantity$Mnthyear_Quantity)
View(Apac_Consumer_Quantity)
Apac_Consumer_tsQuantitydata <- ts(Apac_Consumer_Quantity$Mnthyear_Quantity)
View(Apac_Consumer_tsQuantitydata)
Apac_Consumer_Quantity_traindata <- Apac_Consumer_Quantity[1:42,]
View(Apac_Consumer_Quantity_traindata)
Apac_Consumer_Quantity_testdata <- Apac_Consumer_Quantity[43:48,]
View(Apac_Consumer_Quantity_testdata)
Apac_Consumer_Quantity_traindata_ts <- ts(Apac_Consumer_Quantity_traindata$Mnthyear_Quantity)
plot(Apac_Consumer_Quantity_traindata_ts)

detach("package:dplyr", unload=TRUE)
#Smoothing the timeseries - Using Moving Average method
w <-1
Apac_Consumer_Quantity_smoothedseries <- filter(Apac_Consumer_Quantity_traindata_ts,
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- Apac_Consumer_Quantity_smoothedseries[w+2] - Apac_Consumer_Quantity_smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  Apac_Consumer_Quantity_smoothedseries[i] <- Apac_Consumer_Quantity_smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(Apac_Consumer_Quantity_traindata_ts)
diff <- Apac_Consumer_Quantity_smoothedseries[n-w] - Apac_Consumer_Quantity_smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  Apac_Consumer_Quantity_smoothedseries[i] <- Apac_Consumer_Quantity_smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in_Apac_Consumer_Quantity <- Apac_Consumer_Quantity_traindata$Month_Year

lines(Apac_Consumer_Quantity_smoothedseries, col="blue")



#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

Apac_Consumer_Quantity_smootheddf <- as.data.frame(cbind(timevals_in_Apac_Consumer_Quantity, as.vector(Apac_Consumer_Quantity_smoothedseries)))
colnames(Apac_Consumer_Quantity_smootheddf) <- c('Month_Year', 'Quantity')
View(Apac_Consumer_Quantity_smootheddf)

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

Apac_Consumer_Quantity_lmfit <- lm(Quantity ~ sin(0.5*Month_Year-pi/2) * poly(Month_Year,1)+cos(0.5*Month_Year-pi/2)
, data=Apac_Consumer_Quantity_smootheddf)
Apac_Consumer_Quantity_global_pred <- predict(Apac_Consumer_Quantity_lmfit, Month_Year=timevals_in_Apac_Consumer_Quantity)
summary(Apac_Consumer_Quantity_global_pred)
plot(Apac_Consumer_Quantity_traindata_ts)
lines(Apac_Consumer_Quantity_global_pred, col='red')

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

Apac_Consumer_Quantity_local_pred <- Apac_Consumer_Quantity_traindata_ts-Apac_Consumer_Quantity_global_pred
plot(Apac_Consumer_Quantity_local_pred, col='red', type = "l")
acf(Apac_Consumer_Quantity_local_pred)
acf(Apac_Consumer_Quantity_local_pred, type="partial")
##We can see that the ACF, PACF plots crosses the safe area which means it is not a stationery series let us predict the model using auto_arima
Apac_Consumer_Quantity_armafit <- auto.arima(Apac_Consumer_Quantity_local_pred)

tsdiag(Apac_Consumer_Quantity_armafit)
Apac_Consumer_Quantity_armafit


#We'll check if the residual series is white noise

Apac_Consumer_Quantity_resi <- Apac_Consumer_Quantity_local_pred-fitted(Apac_Consumer_Quantity_armafit)

adf.test(Apac_Consumer_Quantity_resi,alternative = "stationary")
#Dickey-Fuller = -6.3068, Lag order = 3, p-value = 0.01

kpss.test(Apac_Consumer_Quantity_resi)
#KPSS Level = 0.026061, Truncation lag parameter = 1, p-value = 0.1

##From kpss and adf test we can conclude that residual series is white noise.
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
library(dplyr)
Apac_Consumer_Quantity_outdata <- data.frame(Apac_Consumer_Quantity_testdata)

Apac_Consumer_Quantity_timevals_out <- Apac_Consumer_Quantity_outdata$Month_Year

Apac_Consumer_Quantity_global_pred_out <- predict(Apac_Consumer_Quantity_lmfit,data.frame(Month_Year =Apac_Consumer_Quantity_timevals_out))

Apac_Consumer_Quantity_fcast <- Apac_Consumer_Quantity_global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

Apac_Consumer_Quantity_MAPE_class_dec <- accuracy(Apac_Consumer_Quantity_fcast,Apac_Consumer_Quantity_outdata[,2])[5]
Apac_Consumer_Quantity_MAPE_class_dec
# [1] 27.03118

#----------------------------------------------------------------------------------------------------
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

Apac_Consumer_Quantity_class_dec_pred <- c(ts(Apac_Consumer_Quantity_global_pred),ts(Apac_Consumer_Quantity_global_pred_out))
plot(Apac_Consumer_tsQuantitydata, col = "black")
lines(Apac_Consumer_Quantity_class_dec_pred, col = "red")
#-----------------------------------------------------------------------------------------------------------------------
#So, that was classical decomposition, now let's do an ARIMA fit

Apac_Consumer_Quantity_autoarima <- auto.arima(Apac_Consumer_Quantity_traindata_ts,stepwise=FALSE,approx=FALSE)
Apac_Consumer_Quantity_autoarima
tsdiag(Apac_Consumer_Quantity_autoarima)
plot(Apac_Consumer_Quantity_autoarima$x, col="black")
lines(fitted(Apac_Consumer_Quantity_autoarima), col="red")

#Again, let's check if the residual series is white noise

Apac_Consumer_Quantity_resi_auto_arima <- Apac_Consumer_Quantity_traindata_ts - fitted(Apac_Consumer_Quantity_autoarima)

adf.test(Apac_Consumer_Quantity_resi_auto_arima,alternative = "stationary")
kpss.test(Apac_Consumer_Quantity_resi_auto_arima)

#Also, let's evaluate the model using MAPE
Apac_Consumer_Quantity_fcast_auto_arima <- predict(Apac_Consumer_Quantity_autoarima, n.ahead = 6)

Apac_Consumer_Quantity_MAPE_auto_arima <- accuracy(Apac_Consumer_Quantity_fcast_auto_arima$pred,Apac_Consumer_Quantity_outdata[,2])[5]
Apac_Consumer_Quantity_MAPE_auto_arima 
##[1] 37.39167 ((MAPE is higher in comparison to classical decomposition method)

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

Apac_Consumer_Quantity_auto_arima_pred <- c(fitted(Apac_Consumer_Quantity_autoarima),ts(Apac_Consumer_Quantity_fcast_auto_arima$pred))
plot(Apac_Consumer_Quantity_traindata_ts, col = "black")
lines(Apac_Consumer_Quantity_auto_arima_pred, col = "red")

##Forecasting the quantity in Apac_Consumer market segment for next 6 months, as model made using classical decomposition is having lower MAPE value we will use the same model for forecasting

Apac_Consumer_Quantity_forecast_month <- seq(49,54, by = 1)
Apac_Consumer_Quantity_global_forecast <- predict(Apac_Consumer_Quantity_lmfit,data.frame(Month_Year =Apac_Consumer_Quantity_forecast_month))
#1        2        3        4        5        6 
#541.7999 478.3454 456.8664 487.0945 565.6635 676.4414 
##__________________________________________________________________________________________________________________________________________

############### END of Code ###############




   
 