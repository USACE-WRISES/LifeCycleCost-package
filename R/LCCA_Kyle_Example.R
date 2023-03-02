---
title: 'Life Cycle Cost Analysis'
author: "Liya Abera and Kyle McKay"
date: "November 2022"
always_allow_html: true
---

# Initialize Model


```{r}
#Markdown options
knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message = FALSE) 

#Clear local memory
rm(list=ls(all=TRUE))

#Import any necessary packages
#Currently none
```

# DEVELOP FUNCTIONS

Set up functions for standard life cycle cost factors

## Discount Factor

```{r}
#Function for computing annual discount factor based on:
        #discount rate (dr)
        #time in the future in years (tdis)
disfact <- function(dr, tdis){1 / (1+dr)^tdis}

#Test function
disfact(0.055,30)
disfact(0.055,seq(1,30))
```


## Economic Cost Annualization

```{r}
#Function for computing annualized economic costs based:
        #discount rate (dr)
        #present value cost (CostPV)
        #time horizon (thor)
ann.econ <- function(dr, CostPV, thor){CostPV * (dr*(1+dr)^thor) / ((1+dr)^thor - 1)}

#Test function
ann.econ(0.0275, 10905648, 50)
```


## Interest during construction 

```{r}
#Function for calculating interest during construction based on:
        #Capital cost (Ccap)
        #Construction duration in months (tdur)
        #Annual discount rate (dr)
idc <- function(Ccap, tdur, dr){
        #Compute monthly discount rate
        dr.month <- (1+dr)^(1/12) - 1
        
        #Compute idc for each month
        idc.month <- (Ccap/tdur)*((1+dr.month)^(tdur-seq(1,tdur))-1)
        
        #Compute total idc in PV
        sum(idc.month)
}

#Test function
idc(10905648, 16, 0.0275)
idc(1000,12,0.055)
```


## Ditribute OM into a time series and discount

```{r}
#Function for distributing and discounting OM factors based on:
        #annual OM cost (OM)
        #frequency in years (fq)
        #time horizon (thor)
        #discount factor (df)
OMdist <- function(OM, fq, thor, dr){
        #Distribute present value through time
        OM.PV <- rep(c(OM,rep(0, length.out=fq-1)),thor/fq)
        
        #Discount present value
        disfact(dr,seq(1,thor)) * OM.PV
}

#Test function
OMdist(1640,1,30, 0.05)
OMdist(5200,3,30, 0.05)
OMdist(4704,5,30, 0.05)
OMdist(5000,3,30, 0.05)
OMdist(1000,2,10, 0.0275)
```


# EXAMPLE CASE


# Set inputs

```{r}
#Annual discount rate (dr)
dr <- 0.055

#Time horizon in yrs
thor <- 30

#Capital cost (Ccap)
Ccap <- 65837

#Construction duration (dur) in MONTHS
dur <- 12

#Operations and maintenance (OM) expenses and frequency (fq)
OM1 <- 1640; fq1 <- 1
OM2 <- 5200; fq2 <- 3
OM3 <- 4704; fq3 <- 5
```


## Compute outputs

```{r}
#Compute interest during construction
#idc.test <- idc(Ccap,dur,dr)

#Distribute OM factors into a matrix
OM.test <- matrix(0, nrow=thor, ncol=3)

OM.test[,1] <- OMdist(OM1, fq1, thor, dr)
OM.test[,2] <- OMdist(OM2, fq2, thor, dr)
OM.test[,3] <- OMdist(OM3, fq3, thor, dr)
OM.test
```

## Convert to annualized economic cost

```{r}
#Empty vector to store annualized costs
Cann.test <- c()

#Capital cost
Cann.test[1] <- ann.econ(dr, Ccap, thor)

#Interest during construction
#Cann.test[2] <- ann.econ(dr, idc.test, thor)
Cann.test[2] <- 0

#O&M
Cann.test[3] <-ann.econ(dr, sum(OM.test[,1]), thor)
Cann.test[4] <-ann.econ(dr, sum(OM.test[,2]), thor)
Cann.test[5] <-ann.econ(dr, sum(OM.test[,3]), thor)

Cann.test
```


## Compute cost summaries

```{r}
#TOTAL COST
Cann.test.total <- sum(Cann.test)
Cann.test.total

#Proportion of total cost
Pann.test <- Cann.test / Cann.test.total
Pann.test
```
