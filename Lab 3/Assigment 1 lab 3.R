#packAssigment 1 LAb 3
rm(list=ls())
set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", encoding = "latin1")
temps <- read.csv("temps50k.csv")
stNoFilter <- merge(stations,temps,by="station_number")


#h_distance to account in a radius of 15 swedish miles
h_distance <-10
#To account for the closest 4 hours
h_timeDiff <-2.5
#To account for the 15 closest days
h_dateDiff <-10

latitude <- 58.4274 # The point to predict (up to the students)
longitud <- 14.826


# observation 624

myPos <- c(longitud, latitude)
date <-"2013-11-04" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00","10:00:00","12:00:00","14:00:00","16:00:00","18:00:00",
           "20:00:00","22:00:00", "24:00:00")
temp <- vector(length=length(times))
st <- stNoFilter[as.Date(stNoFilter$date) < as.Date(date),]
# Students' code here



get_distance = function(x1, x2){
  return(distHaversine(x1,x2))
}




kernel_position = function(x,position,h){
  return (exp(-(distHaversine(x , position)/h/10000)^2))
}


my_kernel_position = function(x, position, temperature,h){
  teller = 0
  denominator = 0
  
  
  for(i in 1:length(temperature)){
    teller = teller + kernel_position(x, position[i,],h)*temperature[i]
    denominator = denominator + kernel_position(x, position[i,],h)
  }
  
  return(teller/denominator)
  
}
get_day_diff = function(newest, oldest){
  newest = (paste("2020",substr(newest,5,10),sep=""))
  oldest = (paste("2016",substr(oldest,5,10),sep=""))
  d = difftime(newest, oldest, units=c("days"))
  d1 = as.numeric(d,units=("days"))
  d1 = (d1-1) %% 365
  if(d1>=183){
    d1= 183- d1 %% 183
  }
  return(d1)
}

kernel_days = function(x, date,h){
  return (exp(-(get_day_diff(x,date)/h)^2))
}

my_kernel_dateDiff = function(x, date, temperature,h){
  teller = 0
  denominator = 0

  for(i in 1:length(temperature)){
    teller = teller + kernel_days(x, date[i],h)*temperature[i]
    denominator = denominator + kernel_days(x, date[i],h)
    
  }
 return(teller/denominator)
  
}

get_time_diff = function(time1, time2){
  t1= as.numeric(substr(time1,1,2))
  t2= as.numeric(substr(time2,1,2))
  diff = abs(t1-t2)
  if(diff>12){
    diff= 12 - diff %% 12
  }
  return(diff)
              
}

kernel_time = function(x, time,h){
  return(exp(-(get_time_diff(x,time)/h)^2))
  
}

my_kernel_timeDiff = function (times , time, temperature, h){
  timeVector = c()
  for( k in 1:length(times)){
    teller = 0
    denominator = 0
    print(k)
    for(i in 1:length(temperature)){
      teller = teller + kernel_time(times[k],time[i],h)*temperature[i]
      denominator = denominator + kernel_time(times[k],time[i],h)
      
    }
    prediction = teller/denominator
  
    timeVector= c(timeVector, prediction)
    print(timeVector)
  }
  return(timeVector)
}

sum_of_kernels = function(position,times, date, data, h_dateDiff, h_distance, h_timeDiff){
  
  tempVector = c()
  tempVectorAddition = c()
  
  for(t in 1:length(times)){
    
    teller = 0
    denominator = 0
    tellerAddition = 0
    denominatorAddition = 0
    for(i in 1:length(data$air_temperature)){
      kernelD = kernel_days(date, data$date[i], h_dateDiff)
      kernelP = kernel_position(position,cbind(data$longitude[i], data$latitude[i]), h_distance)
      kernelT = kernel_time(times[t], data$time[i], h_timeDiff)
      
      
      kernel= kernelD*kernelP*kernelT
      kernelAddition = kernelD + kernelP + kernelT
      
      
      teller = teller + data$air_temperature[i]*(kernel)
      denominator = denominator + (kernel)
      tellerAddition = tellerAddition + data$air_temperature[i]*kernelAddition
      denominatorAddition = denominatorAddition + kernelAddition
      
    }
    predict = teller/denominator
    tempVectorAddition = c(tempVectorAddition, tellerAddition/denominatorAddition)
    tempVector = c(tempVector, predict)
    print(tempVector)
    print(paste0(t/11*100,"%"))
  }
  return(cbind(tempVector, tempVectorAddition))
  
}
predictVector = sum_of_kernels(myPos, times, date,st,h_dateDiff ,h_distance , h_timeDiff)
plot(predictVector[,2], type="o", main="addition")
plot(predictVector[,1], type="o", main="multiplication")
#my_predict_pos = my_kernel_position(myPos, cbind(st$latitude,st$longitude), st$air_temperature, h_distance)
#print("inne på andra")
#my_predict_days = my_kernel_dateDiff(date, st$date, st$air_temperature, h_dateDiff)
#print("inne på tredje")
#my_predict_time = my_kernel_timeDiff(times, st$time, st$air_temperature,h_timeDiff)
