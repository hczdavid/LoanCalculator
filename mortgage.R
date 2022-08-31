# Calculate the interest and the year for full paid

# get the function from optiRum package to calculate the monthly payment
cal_monthlypayment <- function(pv, nterm, rate) {
  stopifnot(rate > 0, rate < 1, nterm >= 1, pv > 0)
  return(round(pv * rate/(1 - 1/(1 + rate)^nterm), 2))
} 

# calcuate monthly interest and principle detail 
cal_IntPrin <- function(PV, payment, rate){
  
  rate       <- rate/12          # update the annuel interest rate to monthly rate
  month_int  <- c()              # vector to save interest
  month_pv   <- c()              # vector to save paid principle
  left_pv    <- c()              # vector to save left principle
  cum_pv     <- c()              # vector to save cumulative paid principle
  cum_int    <- c()              # vector to save cumulative interest 
  
  i=0
  while(PV > 0){ #stop the loop when current PV less than 0
    i <- i + 1
    month_int[i] <- PV*rate
    month_pv[i]  <- payment - PV*rate
    left_pv[i]   <- PV - (payment - PV*rate)
    cum_pv[i]    <- sum(month_pv[1:i])
    cum_int[i]   <- sum(month_int[1:i])
    
    newPV        <- PV - (payment - PV*rate)
    PV           <- newPV  #update PV
  }
  
  tot_int   <- round(sum(month_int),2)
  finalyear <- round(length(month_int)/12, 2)
  
  fulltable <- data.frame(Month = 1:length(month_int), 
                         Monthly.Interest  = round(month_int,2),
                         Cum.Interest      = round(cum_int,2),
                         Monthly.Principle = round(month_pv,2),
                         Cum.Principle     = round(cum_pv,2),
                         Remain.Principle    = round(left_pv,2))
  return(list(tot_int = tot_int, finalyear = finalyear, fulltable = fulltable))
}



# calcuate refinance 
cal_refinance <- function(PV, oldpayment, oldrate, newrate, newterm){
  
  oldint     <- cal_IntPrin(PV = PV, payment = oldpayment, rate = oldrate)$tot_int
  newpayment <- cal_monthlypayment(rate = newrate/12, nterm = newterm, pv = PV)
  newint     <- cal_IntPrin(PV = PV, payment = newpayment, rate = newrate)
  
  return(list(Saveint     =  round(oldint-newint$tot_int,2),
              newmonthpay =  newpayment,
              newInt_Prin =  newint))
}

cal_onetimepay <- function(PV, payment, rate, onetimepay){
  
  before_one <- cal_IntPrin(PV = PV, payment = payment, rate = rate)
  after_one  <- cal_IntPrin(PV = PV-onetimepay, payment = payment, rate = rate)
  
  save_int  <- before_one$tot_int   - after_one$tot_int
  save_year <- before_one$finalyear - after_one$finalyear
  return(list(save_int = save_int, save_year = save_year))
}


cal_incmonthpay <- function(PV, payment, rate, incmonthpay){
  
  before_one <- cal_IntPrin(PV = PV, payment = payment, rate = rate)
  after_one  <- cal_IntPrin(PV = PV, payment = payment+incmonthpay, rate = rate)
  save_int   <- before_one$tot_int   - after_one$tot_int
  save_year  <- before_one$finalyear - after_one$finalyear
  return(list(save_int = save_int, save_year = save_year))
}

#check the inial amount using Mary's mortgage information
pv   <- 138800
nterm <- 360
rate <- 0.0425

mypayment  <- cal_monthlypayment(pv = pv, rate = rate/12, nterm = nterm) #-682.81, correct!
myinterest_original <- cal_IntPrin(PV = pv, payment = mypayment, rate = 0.0425)
myre <- cal_refinance(PV = 88806.25, oldpayment = mypayment, oldrate = 0.0425, newrate = 0.0375, newterm = 180)
# cal_incmonthpay(PV = 200000, payment = mypayment, rate = 0.03, incmonthpay = 200)
# cal_onetimepay(PV = 88806.25, payment = mypayment, rate = 0.0425, onetimepay  = 15000)

# plotdata <- myinterest_original$fulltable[c(1,2,4)]
# colnames(plotdata) <- c("Month", "Interest", "Principle")
# newplotdata <- plotdata %>% 
#   gather(key="P_I",value = "all", Principle,Interest)
# 
# ggplot(data=newplotdata, aes(x=Month, y=all, fill=P_I)) +
#   geom_bar(stat="identity") +
#   theme(legend.title = element_blank()) 
# 
# 
