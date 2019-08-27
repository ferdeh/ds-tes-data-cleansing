
#Title Cleansing data untuk analisa RFM

#install.packages("readxl")
library("readxl")
library(dplyr)
library(tidyr)

#import data
my_data <- read_excel("Online Retail.xlsx")

head(my_data)

#data cleansing 

data_dirty <-filter(my_data,my_data$Quantity <0 | my_data$UnitPrice <0  )
data_clean <- filter(my_data,my_data$Quantity >=0 | my_data$UnitPrice >=0  )
data_clean <- data_clean %>%drop_na()


#Recode variable
data_clean <- data_clean %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
         InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country))
data_clean <- data_clean %>% mutate(total_price = Quantity*UnitPrice)
head(data_clean)


#Reformat data to RFM format

data_RFM <- data_clean %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequency=n_distinct(InvoiceNo), monitery= sum(total_price)/n_distinct(InvoiceNo))

summary(data_RFM)
head(data_RFM)
