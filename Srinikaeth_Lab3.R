library('ggplot2')

load('smb_ts.RData')

summary(smb_ts)

# Cnverting to matrix of days
smb_mat = matrix(smb_ts, nrow = 96, ncol = 365)

# Fig.1
plot(smb_mat[,32], type ='l', ylab = 'Electricity consumption (kWh)', xlab = 'Time (hours)', xaxt = 'n')

check = seq(0,24, by=0.25)
check = check[2:97]

axis(1, at=1:96, labels = check)

# Calculating the normalized daily use
days = 1:365

normalized_smb = matrix(0, nrow = 96, ncol = 365)

for (i in days)
{
  
  start = 1 + (i-1)*96
  end = 96 + (i-1)*96
  
  total = sum(smb_ts[start:end])
  
  smb_ts[start:end]
  
  normalized_smb[,i] = smb_ts[start:end]/total
  
}

c_norm = kmeans(t(normalized_smb), 7, nstart = 25)

# Fig 2a
plot(c_norm$centers[7,], type ='l', ylab = 'Fraction of daily electricity consumption ', xlab = 'Time (hours)', xaxt = 'n', main = 'Cluster Type I (Sat, Sun, Mon)')
axis(1, at=1:96, labels = check)


# Fig 2b
plot(c_norm$centers[3,], type ='l', ylab = 'Fraction of daily electricity consumption ', xlab = 'Time (hours)', xaxt = 'n', main = 'Cluster Type II (Tue, Wed)')
axis(1, at=1:96, labels = check)


# Fig 2c
plot(c_norm$centers[2,], type ='l', ylab = 'Fraction of daily electricity consumption ', xlab = 'Time (hours)', xaxt = 'n', main = 'Cluster Type III (Thu, Fri)')
axis(1, at=1:96, labels = check)

# Fig 3a

c_mean = kmeans(t(smb_mat), 7, nstart = 25)

plot(c_mean$centers[1,], ylim = c(0,1),t='l', col = "red", xlab = 'Time (hours)', ylab = 'Electricity consumption (kWh)', xaxt = 'n')
lines(c_mean$centers[3,],t='l', col = 'blue')

m = c('Weekday', 'Weekend')
c = c('red', 'blue')

legend('topright', legend = m, col = c, pch = 20, cex = 1)

axis(1, at=1:96, labels = check)

# Fig 3b
plot(c_mean$centers[1,],t='l', col = "blue", xlab = 'Time (hours)', ylab = 'Electricity consumption (kWh)' , xaxt = 'n')
axis(1, at=1:96, labels = check)

#Part 2

df = read.csv('elec_end_use.csv', header = TRUE)
df_reduced = df[,c(4,7,8,17,18,20,21,22,23,24,25,26)]
names(df_reduced)[names(df_reduced)=="SQFT8"] <- "Area"

names(df_reduced)[names(df_reduced)=="PBA8"] <- "Pr_Bldg_Act"

names(df_reduced)[names(df_reduced)=="ELUSED8"] <- "Electricity_Used"

names(df_reduced)[names(df_reduced)=="ELHTBTU8"] <- "Heating"

names(df_reduced)[names(df_reduced)=="ELCLBTU8"] <- "Cooling"

names(df_reduced)[names(df_reduced)=="ELWTBTU8"] <- "Water_Heating"

names(df_reduced)[names(df_reduced)=="ELLTBTU8"] <- "Lighting"

names(df_reduced)[names(df_reduced)=="ELCKBTU8"] <- "Cooking"

names(df_reduced)[names(df_reduced)=="ELRFBTU8"] <- "Refrigeration"

names(df_reduced)[names(df_reduced)=="ELOFBTU8"] <- "Office"

names(df_reduced)[names(df_reduced)=="ELPCBTU8"] <- "Comp"

names(df_reduced)[names(df_reduced)=="ELMSBTU8"] <- "Misc"

# Removing rows with no electricity usage
indices = df_reduced$Electricity_Used == 1

df_final = df_reduced[indices,]
df_final <- df_final[-c(1:3)]


# PCA
pcabd_corr<-princomp(df_final[c(1,2,3,4,5,6,7,8,9)],scores = TRUE)

print(pcabd_corr$loadings)
print(summary(pcabd_corr))

# Fig 4

boxplot(xaxt = 'n', ylab = 'Energy use (kBTU)', df_final$Cooling, df_final$Lighting, df_final$Comp, df_final$Misc, df_final$Refrigeration, df_final$Water_Heating, df_final$Heating, df_final$Office, df_final$Cooking)

bplot = c('Cooling', 'Lighting', 'Computer', 'Misc', 'Refrig', 'WaterHeat', 'Heating', 'Office', 'Cooking')

axis(1, at=1:9, labels = bplot, las =2)







