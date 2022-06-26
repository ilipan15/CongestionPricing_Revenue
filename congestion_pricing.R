
### --- loading libraries --- ###

library(nloptr)

### --- loading the dataset --- ###

data = read.csv('CongestionPricing.csv')

### --- Question 1 --- ###
# -- a) -- #

# Compute maximum willingness to pay for each driver 
for (i in 1:345){ 
  data$maxWTP[i]=max(data[i,2:3])
}

#Displaying the first ten rows of data including the maxWTP for each id
data[1:20,]

 # No need to consider a price if no one can afford it.
maxprice=max(data$maxWTP)

# Defining empty array variables we will be introducing
demand=rep(NA,maxprice) 
revenue=rep(NA,maxprice)

# Find how many people buy at each price level
for (p in 1:maxprice){
  
  demand[p]=sum(data$maxWTP>=p)
  revenue[p]=p*demand[p]
  
}

### --- Identify the best price --- ###
revenueBest=max(revenue)
priceBest=which(revenue == revenueBest)
demand_best_price = sum(data$maxWTP>= priceBest)

print(paste("If a single price is to be charged for both peak and non peak hours, the optimal price is:",priceBest))

print(paste("For price 8 the demand is :",demand_best_price, "out of the 345 drivers of the survey"))

# calculating the percentage for the demand 
overall_per = 303/345 ## --- 0.8782609

# getting the percentage for the 192,000 drivers
cars = 0.8782609*192000 ## --- 168626.1 --- ###

# calculating the average speed
average_speed = 30 - 0.0625 * 168.626

print(paste("The average speed for 168626.1 cars is:", round(average_speed)))

# The average speed is below 25km/h, so we will use the second type for calculating the emissions per car.

emissions_per_car = 617.5 - 16.7*average_speed 
total = emissions_per_car * 168626.1 # calculating the total emissions for the 168626.1 cars 

print(paste("For price equal to 8 the total emissions are", round(total)))


total_revenue_a = 8*cars


print(paste("For price equal to 8 the total revenue is", round(total_revenue_a)))

### --- Plotting the demand vs the Price --- ###
xaxis=1:maxprice
plot(xaxis,demand,pch = 16, type="s", col="blue", las=1, xaxt="n",
     xlab="Price",ylab="Demand") 
xticks <- seq(0, maxprice, by=3) 
axis(side = 1, at = xticks)



### --- b) --- ###

non_peak_price = 7 

# The maximum WTP in data, we can use this as the upper bound for our price search.
# No need to consider a price if no one can afford it.
maxprice_b = max(data[2:3])

demandNonPeak<-rep(0,maxprice_b) 
demandPeak<-rep(0,maxprice_b) 
revenue_b<-rep(0,maxprice_b)


##  STEP 1:
# For each driver we will obtain their maximum WTP and
# maximum Surplus among the Non peak hour

maxWTPNonPeak <-rep(0,345)

maxsurplusNonPeak<-rep(0,345)

for (i in 1:345){
  
  maxWTPNonPeak[i] = max(data[i,3]) 
  maxsurplusNonPeak[i]=max(data[i,3]-non_peak_price)
  # We can also generate new column(s) and add this information to our data:
  data$maxWTPNonPeak[i]=max(data[i,3])
  data$maxsurplusNonPeak[i]=max(data[i,3]-non_peak_price)
}

# Viewing the first ten rows of data
data[1:10,]

# STEP 2:
# For each possible price point:
# and for all drivers at the particular price point currently in consideration:

# Compare a driver's surplus from Non Peak and Peak Hours

# If a driver's surplus from Non Peak is greater than their surplus for Peak
# and if the driver's surplus from Non Peak is greater than 0,
# That driver will purchase Non Peak.

# If a driver's surplus from Peak is greater than their surplus for Non Peak
# and if the driver's surplus from Peak is greater than 0,
# That driver will purchase Peak.

# If both surpluses are less than 0, the driver will not buy.


# Let's first compute drivers' surpluses for Peak across all possible Peak Price choices
# There are 345 clients and 17 possible price choices
# So we will create a matrix of dimension: 345 rows (for each client) and 17 Columns


surplusPeak<-matrix(0,345,maxprice_b)

for (p in 1:maxprice_b){ 
  
  for (i in 1:345){
    
  surplusPeak[i,p]=data[i,2]-p }
}

colnames(surplusPeak) = paste0("p=",1:maxprice_b) 
surplusPeak[1:10,c(1:17)]

for (p in 1:maxprice_b){ 
  
  demandNonPeak[p]=sum((maxsurplusNonPeak>surplusPeak[,p])*(maxsurplusNonPeak>=0))*192000/345 
  demandPeak[p]=sum((surplusPeak[,p]>=maxsurplusNonPeak)*(surplusPeak[,p]>=0))*192000/345 
  revenue_b[p]=non_peak_price*demandNonPeak[p]+p*demandPeak[p]
  
}


### --- Plotting NonPeak Demand vs NonPeak Period Price --- ###
xaxis=1:maxprice_b
plot(xaxis,demandNonPeak,pch = 16, type="s",col="blue", las=1, xaxt="n",
     xlab="Price for Non Peak Period ",ylab="Non-Peak Period Demand")
xticks <- seq(0, maxprice_b, by=2)
axis(side = 1, at = xticks)


### --- Plotting Peak Demand vs Peak Period Price --- ###
xaxis=1:maxprice_b
plot(xaxis,demandPeak,pch = 16, type="s",col="blue", las=1, xaxt="n",
     xlab="Price for Peak Period ",ylab="Peak Hour Demand") 
xticks <- seq(0, maxprice_b, by=2)
axis(side = 1, at = xticks)



### --- Plotting Revenue vs Peak Period Price --- ###

xaxis=1:maxprice_b
plot(xaxis,revenue_b,pch = 16, type="s",col="blue", las=1, xaxt="n",
     xlab="Price for Peak Hour ",ylab="Total Revenue")
xticks <- seq(0, maxprice_b, by=2)
axis(side = 1, at = xticks)

### --- Finding the revenue and the optimal price --- ###
revenueBest_b=max(revenue_b[non_peak_price:maxprice_b])
priceBest_b=which(revenue_b == revenueBest_b)


axis(side = 1, at = priceBest_b) 
lines(c(priceBest_b,priceBest_b),c(0, revenueBest_b),lty=2)
axis(side = 2, at = round(revenueBest_b,3),las=1)
lines(c(0,priceBest_b),c(revenueBest_b, revenueBest_b),lty=2)

### --- Printing the results --- ###

print(paste("When Non Peak Hour has a base price of 7, the optimal price for the Peak Hour  is", priceBest_b))

print(paste("When Non Peak Hour has a base price of 7, the optimal price for the Peak Hour  is", priceBest_b, "and the Revenue is",revenueBest_b ))

### --- Calculating the demand for Non Peak and Peak hour --- ####

demandNonPeak_b= demandNonPeak[priceBest_b] 
demandPeak_b=demandPeak[priceBest_b]

print(paste("When Non Peak Hour has a base price of 7, the demand for Non Peak Hour is:", round(demandNonPeak_b)))
print(paste("When Peak Hour has a base price of 9, the demand for Peak Hour is:", round(demandPeak_b)))

### --- Calculating the average speed for Non Peak and Peak hour --- ####

average_speed_non_peak = 30 - (0.0625*(demandNonPeak_b)/1000) 
average_speed_peak = 30 - (0.0625*(demandPeak_b)/1000)

print(paste("The average speed for 42852 cars in Non Peak Hour is:", round(average_speed_non_peak)))
print(paste("The average speed for 119096 cars in Peak Hour is:", round(average_speed_peak)))


### --- The average speed is above 25km/h, so we will use the second type for calculating the emissions per car non peak hours. --- ###

emissions_per_car_nonpeak= 235.0 - 1.4*average_speed_non_peak

### --- The average speed is under 25km/h, so we will use the second type for calculating the emissions per car peak hours. --- ###

emissions_per_car_peak= 617.5 - 16.7*average_speed_peak

### --- Finding the total emissions for Peak and Non Peak Hour --- ###

emissionspeak_cars = demandPeak_b * emissions_per_car_peak
emissionsnonpeak_cars = demandNonPeak_b*emissions_per_car_nonpeak

### --- calculating the total emissions --- ###

total_emissions_b = emissionsnonpeak_cars + emissionspeak_cars # 37110105  

### --- Finding the total revenue --- ###

revenue_b_total= 7*demandNonPeak[9] + 9*demandPeak[9] # 1371826


### --- Printing the results --- ###
print(paste("For Non Peak price equal to 7 and Peak price equal to 9 the total revenue is :", round(revenue_b_total)))
print(paste("For Non Peak price equal to 7 and Peak price equal to 9 the total emissions are :", round(total_emissions_b)))





 ## -- c) -- ##

### --- Minimize the Emissions --- ###

### --- Constructing the Objective Function --- ###
eval_f <- function(x){
  basePrice=7
  peakPrice=x
  
  NonPeakDemand = demandNonPeak[x]
  PeakDemand = demandPeak[x]
  
  ### --- Finding the emissions and average speed for Non Peak Hour--- ##
  
  NonPeak_Average_Speed = (30-0.0625*(NonPeakDemand/1000))
  
  if ( NonPeak_Average_Speed < 25) {
    
    Emissions_NonPeak = (617.5-16.7*(NonPeak_Average_Speed))*NonPeakDemand
  } else {
    Emissions_NonPeak = (235.0-1.4*(NonPeak_Average_Speed))*NonPeakDemand
  }
  ### --- Finding the emissions and average speed for Peak Hour--- ##
  
  Peak_Average_Speed = (30-0.0625*(PeakDemand/1000))
  
  if ( Peak_Average_Speed < 25) {
    Emissions_Peak = (617.5-16.7*(Peak_Average_Speed))*PeakDemand
  } else {
    Emissions_Peak = (235.0-1.4*(Peak_Average_Speed))*PeakDemand
  }
  
  ### ---  Calculating the total emissions --- ###
  Emissions = Emissions_Peak+Emissions_NonPeak
  
  objfunction = Emissions
  return(objfunction)
}


### --- Constructing the Constraints --- ###
eval_g_ineq <- function(x) {
  basePrice=7
  peakPrice=x
  
  NonPeakDemand = demandNonPeak[x]
  PeakDemand = demandPeak[x]
  revenue=basePrice*NonPeakDemand+peakPrice*PeakDemand
  constraint <- c(1100000-revenue,
                  basePrice-peakPrice)
  return(constraint)
}

### --- Initial value --- ###

x0 <- 1

# lower and upper bounds of control

lb <- 1
ub <- 17
opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel"  = 1.0e-9,
              "maxeval"   = 1000)
result <- nloptr(x0=x0,eval_f=eval_f,lb=lb,ub=ub,
                 eval_g_ineq=eval_g_ineq,opts=opts)

print(result) # printing the result

priceOpt<-result$solution
priceOpt
EmissionOpt<- result$objective
EmissionOpt 

print(paste("Optimal peak Price:",priceOpt[1]))
print(paste("The emissions are :", round(EmissionOpt[1])))

basePrice=7
peakPrice=13
NonPeakDemand = demandNonPeak[13]
PeakDemand = demandPeak[13]
revenue_c =7*NonPeakDemand+13*PeakDemand
revenue_c

print(paste("The total revenue is:", round(revenue_c)))

