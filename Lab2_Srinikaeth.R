# Reading the data

df_ashton = read.table("C:\\Users\\user1\\Documents\\CEE 322\\Lab 2\\Ashton14.txt", header=TRUE)
df_deer = read.table("C:\\Users\\user1\\Documents\\CEE 322\\Lab 2\\DeerLodge14.txt", header=TRUE)

# Summary of variables
summary(df_ashton)
summary(df_deer)

# Fig 1a.

sits_ashton2 = ts(df_ashton$SI, frequency = 24*30)
sits_ashton_comps2 = decompose(sits_ashton2)
plot(sits_ashton_comps2$trend, xlab = 'Time (months)', ylab = 'Trend')

# Fig 1b

plot(df_ashton$SI[3625:3648],type = "o",col = "red", xlab = "Time of day", ylab = "Hourly solar radiation (langleys/hr)")

lines(df_ashton$SI[8737:8760], type = "o", col = "blue")

m = c('Summer', 'Winter')
c = c('red', 'blue')

legend('topright', legend = m, col = c, pch = 20, cex = 1)

# Fig 2

cumhours = c(0,744,1416,2160,2880,3624,4344,5088,5832,6552,7296,8016,8760)

auc_ashton = c()
auc_deer = c()

for (i in c(1,2,3,4,5,6,7,8,9,10,11,12))
{
  del = cumhours[i+1] - cumhours[i]
  
  time = seq(1,del, length = del)
  
  start = 1+cumhours[i]
  end = cumhours[i+1]
  
  dfred_ashton <- df_ashton[start:end,]
  dfred_deer <- df_deer[start:end,]
  
  #si_ashton <- si_ashton[!is.na(si_ashton)]
  #si_deer <- si_deer[!is.na(si_deer)]
  
  si_ashton <- dfred_ashton$SI
  si_deer <- dfred_deer$SI
  
  
  a_ashton = trapz(time,si_ashton)
  a_deer = trapz(time,si_deer)
  
  auc_ashton <- append(auc_ashton,a_ashton)
  
  auc_deer <- append(auc_deer,a_deer)
  
}

auc_ashton

auc_deer


plot(auc_ashton,type = "o",col = "red", xlab = "Month", ylab = "Average solar radiation (langleys)")

lines(auc_deer, type = "o", col = "blue")

m = c('Ashton', 'Deer Lodge')
c = c('red', 'blue')

legend('topright', legend = m, col = c, pch = 20, cex = 1)

# Fig 3a
si_ts_ashton = ts(df_ashton$SI, frequency = 1)

si_ts_deer = ts(df_deer$SI, frequency = 1)

acf(si_ts_ashton, lag.max = 24*7, type = 'correlation')

acf(si_ts_deer, lag.max = 24*7, type = 'correlation')

# Fig 3b

ccf(df_ashton$SI,df_ashton$OBM)
ccf(df_deer$SI,df_deer$OBM)

# Fig 4a

hist(df_ashton$WD, breaks = 20, xlab ='Wind Direction')

plot(df_ashton$WD[0:24],type = "o",col = "red", xlab = "Time of day", ylab = "Wind direction (degrees azimuth)", ylim=c(0,400))

lines(df_deer$WD[0:24], type = "o", col = "blue")

m = c('Ashton', 'Deer Lodge')
c = c('red', 'blue')

legend('topright', legend = m, col = c, pch = 20, cex = 1)


# Fig 4b

ind1 = df_ashton$WG <= 10
df_ash1 = df_ashton[ind1,]

ind2 = df_deer$WG <= 10
df_deer1 = df_deer[ind2,]

hist(df_ash1$WS, breaks = 50, xlab = 'Wind speed (mph)')

hist(df_deer1$WS, breaks = 50, xlab = 'Wind Speed (mph)')

# Fig 5

cumhours = c(0,744,1416,2160,2880,3624,4344,5088,5832,6552,7296,8016,8760)

average_ashton = c()
average_deer = c()

for (i in c(1,2,3,4,5,6,7,8,9,10,11,12))
{
  
  start = cumhours[i]+1
  end = cumhours[i+1]
  
  dfred_ashton = df_ashton[start:end,]
  dfred_deer = df_deer[start:end,]
  
  ave1 = mean(dfred_ashton$WS)
  ave2 = mean(dfred_deer$WS)
  
  average_ashton <- append(average_ashton,ave1)
  
  average_deer <- append(average_deer,ave2)
  
}

average_ashton

average_deer

plot(average_ashton,type = "o",col = "red", xlab = "Month", ylab = "Average Wind speed (mph)", ylim = c(4,11))

lines(average_deer, type = "o", col = "blue")

m = c('Ashton', 'Deer Lodge')
c = c('red', 'blue')

legend('topright', legend = m, col = c, pch = 20, cex = 1)

# Fig 6a

ws_ts_ashton = ts(df_ashton$WS, frequency = 1)

ws_ts_deer = ts(df_deer$WS, frequency = 1)

acf(ws_ts_ashton, lag.max = 7*24, type = 'correlation')

acf(ws_ts_deer, lag.max = 7*24, type = 'correlation')

# Fig 6b

ccf(df_ashton$WS,df_ashton$TU)
ccf(df_deer$WS,df_deer$TU)

