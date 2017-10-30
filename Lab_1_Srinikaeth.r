
library("ggplot2")
load("plants.Rdata")
head(df)
dim(df)

# First calculating the plants that have either partial or full combustion
indices_comb = df$Combust != 0
df_comb = df[indices_comb,]
head(df_comb)

# Calculating % of electricity genration with partial or full combustion
netgen_comb = sum(df_comb['NetGen'])
netgen_total = sum(df['NetGen'])
genfraction_comb = netgen_comb/netgen_total
genfraction_comb*100

# To get the indices of the plants where there's only partial combustion
indices_partcomb = df_comb$Combust == 0.5
df_partcomb = df_comb[indices_partcomb,]

# Calculating % of electricity genration with partial combustion in combustion plants
netgen_partcomb = sum(df_partcomb['NetGen'])
genfraction_partcomb = netgen_partcomb/netgen_comb
genfraction_partcomb*100

# Choosing the plants that have full combustion
indices_fullcomb = df_comb$Combust == 1
df_fullcomb = df_comb[indices_fullcomb,]
dim(df_fullcomb)


# Calculating the rate of emissions
df_fullcomb['SO2Rate'] <- 'NA'
df_fullcomb['SO2Rate'] <- 2000*(df_fullcomb['SO2']/df_fullcomb['NetGen'])

df_fullcomb['CO2Rate'] <- 'NA'
df_fullcomb['CO2Rate'] <- 2000*(df_fullcomb['CO2']/df_fullcomb['NetGen'])

df_fullcomb['NOXRate'] <- 'NA'
df_fullcomb['NOXRate'] <- 2000*(df_fullcomb['NOX']/df_fullcomb['NetGen'])

# Calcualting HeatRate
df_fullcomb['HeatRate'] <- 'NA'
df_fullcomb['HeatRate'] <- (1/1000)*(df_fullcomb['HeatInput']/df_fullcomb['NetGen'])

# Splitting by emission type
zero_co2 = df_fullcomb['CO2Rate'] != 0
df_co2 = df_fullcomb[zero_co2,]

zero_so2 = df_fullcomb['SO2Rate'] != 0
df_so2 = df_fullcomb[zero_so2,]

zero_nox = df_fullcomb['NOXRate'] != 0
df_nox = df_fullcomb[zero_nox,]


# Plotting Fig.1

df_plot <- subset(df_fullcomb , FuelCat == 'BIOMASS' | FuelCat == 'COAL' | FuelCat == 'OIL' | FuelCat == 'GAS' | FuelCat == 'OTHRFOSL')
l = tapply(df_plot$Capacity, as.factor(df_plot$FuelCat), identity)
boxplot(l, main = 'Plant Capacity over Primary Fuel Type', xlab = 'Primary Fuel Type', ylab = 'Plant Capacity (MW)')

df_plot <- subset(df_fullcomb , FuelCat == 'BIOMASS' | FuelCat == 'COAL' | FuelCat == 'OIL' | FuelCat == 'GAS' | FuelCat == 'OTHRFOSL')
l = tapply(df_plot$NetGen, as.factor(df_plot$FuelCat), identity)
boxplot(l, main = 'Net Generation over Primary Fuel Type', xlab = 'Primary Fuel Type', ylab = 'Net Annual Generation (MWh)')

# Plotting Fig.2

df_temp <- subset(df_co2, CO2Rate <= 3200)
df_plot <- subset(df_temp , FuelCat == 'BIOMASS' | FuelCat == 'COAL' | FuelCat == 'OIL' | FuelCat == 'GAS' | FuelCat == 'OTHRFOSL')
l = tapply(df_plot$CO2Rate, as.factor(df_plot$FuelCat), identity)
boxplot(l, main = expression('CO'['2']*' Emission Rate over Primary Fuel Type'), xlab = 'Primary Fuel Type', ylab = expression('CO'['2']*' Emission Rate (lbs/MWh)'))

df_temp <- subset(df_so2, SO2Rate <= 10)
df_plot <- subset(df_temp , FuelCat == 'BIOMASS' | FuelCat == 'COAL' | FuelCat == 'OIL' | FuelCat == 'GAS' | FuelCat == 'OTHRFOSL')
l = tapply(df_plot$SO2Rate, as.factor(df_plot$FuelCat), identity)
boxplot(l, main = expression('SO'['2']*' Emission Rate over Primary Fuel Type'), xlab = 'Primary Fuel Type', ylab = expression('SO'['2']*' Emission Rate (lbs/MWh)'))

df_temp <- subset(df_nox, NOXRate <= 6)
df_plot <- subset(df_temp , FuelCat == 'BIOMASS' | FuelCat == 'COAL' | FuelCat == 'OIL' | FuelCat == 'GAS' | FuelCat == 'OTHRFOSL')
l = tapply(df_plot$NOXRate, as.factor(df_plot$FuelCat), identity)
boxplot(l, main = expression('NO'['X']*' Emission Rate over Primary Fuel Type'), xlab = 'Primary Fuel Type', ylab = expression('NO'['X']*' Emission Rate (lbs/MWh)'))

# Plotting Fig.3

df_plot <- subset(df_co2, CO2Rate <= 3200)
plot(df_plot$HeatRate, df_plot$CO2Rate, log = "xy", col = factor(df_plot$FuelCat), pch = 20, cex = 1, ylim = c(5,10000), xlim =c(0.002,0.05), xlab = 'Heat Rate', ylab = expression('CO'['2']*' Emission Rate (lbs/MWh)'))
m = unique(factor(df_plot$FuelCat))
options(scipen=999)
legend('topleft', legend = m, col = m, pch = 20, cex = 1)

df_plot <- subset(df_so2, SO2Rate <= 3200)
plot(df_plot$HeatRate, df_plot$SO2Rate, log = "xy", col = factor(df_plot$FuelCat), pch = 20, cex = 1, xlim =c(0.002,0.05), xlab = 'Heat Rate', ylab = expression('SO'['2']*' Emission Rate (lbs/MWh)'))
m = unique(factor(df_plot$FuelCat))
options(scipen=999)
legend('topright', legend = m, col = m, pch = 20, cex = 1)

df_plot <- subset(df_nox, NOXRate <= 6)
plot(df_plot$HeatRate, df_plot$NOXRate, log = "xy", col = factor(df_plot$FuelCat), pch = 20, cex = 1, xlab = 'Heat Rate', ylim = c(0.02,6), xlim =c(0.002,0.05), ylab = expression('NO'['X']*' Emission Rate (lbs/MWh)'))
m = unique(factor(df_plot$FuelCat))
options(scipen=999)
legend('topleft', legend = m, col = m, pch = 20, cex = 1)

# Plotting Fig.4

df_plot <- subset(df_co2, CO2Rate <= 3200)
plot(df_plot$NetGen, df_plot$CO2Rate, log = "xy", col = factor(df_plot$FuelCat), pch = 20, ylim = c(10,5000), cex = 1, xlab = 'Net Generation (MWh)', ylab = expression('CO'['2']*' Emission Rate (lbs/MWh)'))
m = unique(factor(df_plot$FuelCat))
options(scipen=999)
legend('bottomleft', legend = m, col = m, pch = 20, cex = 1)

df_plot <- subset(df_so2, SO2Rate <= 3200)
plot(df_plot$NetGen, df_plot$SO2Rate, log = "xy", col = factor(df_plot$FuelCat), pch = 20, cex = 1, xlab = 'Net Generation (MWh)', ylab = expression('SO'['2']*' Emission Rate (lbs/MWh)'))
m = unique(factor(df_plot$FuelCat))
options(scipen=999)
legend('bottomleft', legend = m, col = m, pch = 20, cex = 1)

df_plot <- subset(df_nox, NOXRate <= 6)
plot(df_plot$NetGen, df_plot$NOXRate, log = "xy", col = factor(df_plot$FuelCat), pch = 20, cex = 1, ylim = c(0.02, 6), xlab = 'Net Generation (MWh)', ylab = expression('NO'['X']*' Emission Rate (lbs/MWh)'))
m = unique(factor(df_plot$FuelCat))
options(scipen=999)
legend('bottomleft', legend = m, col = m, pch = 20, cex = 1)

# Plotting Fig.5

plot(df_co2$NetGen, df_co2$CO2, log = "xy", xlab = 'Net Generation (MWh)', ylab = expression('CO'['2']*' Emission (short tons)'))

plot(df_co2$Capacity, df_co2$CO2, log = "xy", xlab = 'Capacity (MW)', ylab = expression('CO'['2']*' Emission (short tons)'))

plot(df_co2$HeatInput, df_co2$CO2, log = "xy", xlab = 'HeatInput (MMBtu)', ylab = expression('CO'['2']*' Emission (short tons)'))
