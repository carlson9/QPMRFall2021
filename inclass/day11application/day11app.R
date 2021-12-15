rm(list=ls())
setwd('~/QPMRFall2021/inclass/day11application/')
library("readxl")
data = read_excel('DataWDI.xlsx')
head(data)
summary(data)

# We can check the relationship of these four:
# 1. GDP per capita growth (net migration) – Net Migration 
# 2. Exports of goods and services (annual % growth) – Net Migration 
# 3. Income share held by lowest 20% - Net Migration 
# 4. Households and NPISHs Final Consumption Expenditure (annual % growth) – Net Migration

colnames(data)
#all data is from the US - drop these columns
data = data[, -(1:2)]
data = t(data)
head(data)
colnames(data) = data[2, ]
data = data[, !is.na(colnames(data))]

#vars we care about:
# Net migration
vars = colnames(data)[data['Series Name', ] == 'Net migration']
# GDP per capita growth (annual %)
vars = c(vars, colnames(data)[data['Series Name', ] == 'GDP per capita growth (annual %)'])
# Exports of goods and services (annual % growth)
vars = c(vars, colnames(data)[data['Series Name', ] == 'Exports of goods and services (annual % growth)'])
# Income share held by lowest 20%
vars = c(vars, colnames(data)[data['Series Name', ] == 'Income share held by lowest 20%'])
# Households and NPISHs Final consumption expenditure (annual % growth)
vars = c(vars, colnames(data)[data['Series Name', ] == 'Households and NPISHs Final consumption expenditure (annual % growth)'])

#we don't need first two rows now, and we only care about our vars of interest
#let's rename so we can think about controls later
data2 = data[-(1:2), vars]
holdRows = rownames(data2)
#recode everything to numeric and NAs
data2 = apply(data2, 2, as.numeric)

#let's make the rownames something better
rownames(data2) = as.numeric(as.factor(holdRows)) + 1959

#plot densities to get a sense of the dists
par(mfrow = c(2,3))
for(var in vars) plot(density(data2[, var], na.rm = T), main = data['Series Name', var])
#wildly different scales, and maybe a problem with bimodality in outcome
#rescale
holdRows = rownames(data2)
data2 = apply(data2, 2, scale)
rownames(data2) = holdRows
#ignore time, just check bivariate correlations
for(var in vars[-1]) assign(paste0('mod', var), lm(data2[, var] ~ data2[, 1] - 1)) #why minus 1?

#store it all in one table
biLMtab = matrix(NA, nrow = length(vars) - 1, ncol = 4)
for(i in 1:length(vars[-1])) biLMtab[i, ] = summary(get(paste0('mod', vars[i + 1])))$coef
rownames(biLMtab) = data['Series Name', vars[-1]]
colnames(biLMtab) = colnames(summary(get(paste0('mod', vars[i + 1])))$coef)

summary(data2[, 1])
sum(!is.na(data2[, 1]))
#well this is a problem...

data3 = na.omit(data2[, 1])
par(mfrow = c(1,1))
plot(data3 ~ as.numeric(names(data3)))
#can we interpolate?
#interpolating splines
data2 = cbind(data2,
              spline(as.numeric(names(data3)),
                     data3,
                     n = nrow(data2),
                     xmin = as.numeric(rownames(data2)[1]),
                     xmax = as.numeric(rownames(data2)[nrow(data2)]))$y)
colnames(data2)[6] = 'interNet'
plot(data2[, 'interNet'] ~ as.numeric(rownames(data2)))

#now, let's repeat
vars[1] = 'interNet'
holdRows = rownames(data2)
#recode everything to numeric and NAs
data2 = apply(data2, 2, as.numeric)

#let's make the rownames something better
rownames(data2) = as.numeric(as.factor(holdRows)) + 1959

#wildly different scales, and maybe a problem with bimodality in outcome
#rescale
holdRows = rownames(data2)
data2 = apply(data2, 2, scale)
rownames(data2) = holdRows
#ignore time, just check bivariate correlations
for(var in vars[-1]) assign(paste0('mod', var), lm(data2[, var] ~ data2[, vars[1]] - 1)) #why minus 1?

#store it all in one table
biLMtab = matrix(NA, nrow = length(vars) - 1, ncol = 4)
for(i in 1:length(vars[-1])) biLMtab[i, ] = summary(get(paste0('mod', vars[i + 1])))$coef
rownames(biLMtab) = data['Series Name', vars[-1]]
colnames(biLMtab) = colnames(summary(get(paste0('mod', vars[i + 1])))$coef)


#TODO: Interpolate all values, rerun

#TODO: Lag all explanatory variables, rerun (do not use a package)

#TODO: With both lagged data and unlagged, run a first-difference model (again, no package)


#what about a temporal effect?
#this is when we can turn to GPs (more details next week)
data3 = na.omit(data.frame('NY.GDP.PCAP.KD.ZG' = data2$NY.GDP.PCAP.KD.ZG, 'interNet' = data2$interNet))
data3$time = rownames(data3)
library(kernlab)
modGP = gausspr(interNet ~ NY.GDP.PCAP.KD.ZG + time,
                data = data3,
                kernel = 'rbfdot',
                type = 'regression',
                variance.model = T) #we use dot product for the non-stationarity, but here we use sep exp
#when we code our own model, we can specify different inputs to mean and kernel, and make inferences on correlations
#for now we just plot to make inferences
meanX = data.frame('NY.GDP.PCAP.KD.ZG' = mean(data3$NY.GDP.PCAP.KD.ZG), 'time' = data3$time)
yPreds = predict(modGP, meanX)
yPredsLower = yPreds - 1.96*predict(modGP,
                                    meanX,
                                    type = 'sdeviation')
yPredsUpper = yPreds + 1.96*predict(modGP,
                                    meanX,
                                    type = 'sdeviation')

sdX = data.frame('NY.GDP.PCAP.KD.ZG' = mean(data3$NY.GDP.PCAP.KD.ZG) - sd(data3$NY.GDP.PCAP.KD.ZG), 'time' = data3$time)
yPreds2 = predict(modGP, sdX)
yPredsLower2 = yPreds - 1.96*predict(modGP,
                                    sdX,
                                    type = 'sdeviation')
yPredsUpper2 = yPreds + 1.96*predict(modGP,
                                    sdX,
                                    type = 'sdeviation')
par(mfrow = c(1,1))
plot(yPreds ~ as.numeric(rownames(data3)), type = 'l', col = 'red', ylim = c(-2, 2.5))
lines(as.numeric(rownames(data3)), yPredsUpper, col = 'red', lty = 2)
lines(as.numeric(rownames(data3)), yPredsLower, col = 'red', lty = 2)
lines(as.numeric(rownames(data3)), yPreds2, col = 'green')
lines(as.numeric(rownames(data3)), yPredsUpper2, col = 'green', lty = 2)
lines(as.numeric(rownames(data3)), yPredsLower2, col = 'green', lty = 2)

#TODO: repeat the same for all variables and lagged variables

#should we investigate a change-point model?

