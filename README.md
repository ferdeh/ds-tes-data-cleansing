# ds-tes-data-cleansing

---
title: "Cleansing data untuk analisa RFM"
output: html_notebook
author: "Ferdiansyah"
---

Berikut adalah salah satu exercise data preparation menggunakan data set https://archive.ics.uci.edu/ml/datasets/online+retail. Data preparation digunakan agar data customer dalam data set dapat di analisa menggunakan Metode RFM (Recency, Frequency dan Monetary) 

##Import Library 

```{r}
#install.packages("readxl")
library("readxl")
library(dplyr)
library(tidyr)
```

##Import data

```{r}
my_data <- read_excel("Online Retail.xlsx")

print(head(my_data))

```


##data cleansing 
Menghilangkan data jumlah dan unit price yang negatif dan membuang data NA
```{r}
data_dirty <-filter(my_data,my_data$Quantity <0 | my_data$UnitPrice <0  )
data_clean <- filter(my_data,my_data$Quantity >=0 | my_data$UnitPrice >=0  )
data_clean <- data_clean %>%drop_na()

```


##Recode variable

```{r}
data_clean <- data_clean %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
         InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country))
data_clean <- data_clean %>% mutate(total_price = Quantity*UnitPrice)
print(head(data_clean))

```



##Reformat data to RFM format
```{r}
data_RFM <- data_clean %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequency=n_distinct(InvoiceNo), monitery= sum(total_price)/n_distinct(InvoiceNo))

```
###Berikut adalah data RFM dari data online retail yang sudah di reformat
```{r}

print(summary(data_RFM))

print(head(data_RFM))

```

