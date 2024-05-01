library("quantmod")
##PREPARATORY

first_date <- as.Date("2010-01-01")
tickers <- c("WMT","TGT","VZ","T","F", "GM")
companies <- c("Walmart","Target","VeriZon","ATT","Ford","GM")

##GET RISK-FREE RATE
treas_1m <- getSymbols("DGS1MO", src="FRED", auto.assign=FALSE)
names(treas_1m) <- "treas_1m"
treas_1m <- na.omit(treas_1m)
Rf <- treas_1m[endpoints(treas_1m,"month"),]
names(Rf) <- "Rf"
Rf <- Rf/1200   # translate from annualized percentages
index(Rf) <- as.yearmon(index(Rf))
Rf <- na.omit(lag(Rf,1))  
Rf <- window(Rf,start=as.yearmon(first_date))

#Exercise - get stock returns
get_stock_excess_returns <- function(ticker){
  data <- getSymbols(ticker,auto.assign=FALSE,from=first_date)
  daily_prices <-na.omit(data[,6])
  monthly_returns <- monthlyReturn(daily_prices)
  names(monthly_returns) <- "monthly_returns"
  index(monthly_returns) <- as.yearmon(index(monthly_returns))
  data <- merge(monthly_returns,Rf,all=FALSE)
  excess_returns <- data$monthly_returns-data$Rf
  names(excess_returns) <- "excess_returns"
  return(excess_returns)
}

# use the SP500 as market index
ticker <- "^GSPC"
erm <- get_stock_excess_returns(ticker)
names(erm) <- "erm"
                      
erWMT <- get_stock_excess_returns(tickers[1])# Walmart
names(erWMT) <- "erWMT"

erTGT <- get_stock_excess_returns(tickers[2])
names(erTGT) <- "erTGT"

erVZ <- get_stock_excess_returns(tickers[3])
names(erVZ) <- "erVZ"

erT <- get_stock_excess_returns(tickers[4])
names(erT) <- "erT"

erF <- get_stock_excess_returns(tickers[5])
names(erF) <- "erF"

erGM <- get_stock_excess_returns(tickers[6])
names(erGM) <- "erGM"

#Desc stats

data    <- merge(erWMT,erTGT,erVZ,erT,erF,erGM,erm,all=FALSE)
desc_stats <- matrix(nrow=6,ncol=7)
colnames (desc_stats) <- c(companies,"SP500")
rownames (desc_stats) <- c("mean","sd", "min",
                           "med","max","n")
desc_stats[1,] <- colMeans(data)
desc_stats[2,] <- apply(data,2,sd)
desc_stats[3,] <- apply(data,2,min)
desc_stats[4,] <- apply(data,2,median)
desc_stats[5,] <- apply(data,2,max)
desc_stats[6,] <- apply(data,2,length)

#Run regressions
data    <- merge(erWMT,erm,all=FALSE)
regrWMT <- lm(data$erWMT~data$erm)

#Market model parameters
tab <- matrix(nrow=7,ncol=5)
rownames(tab) <- c("index",companies)
colnames(tab) <- c("sd excess ret","beta","syst","SD residual","corr w mkt ")
#market
sigmaM <- sd(erm)
tab[1,1] <- sigmaM
tab[1,2] <- 1
tab[1,3] <- sigmaM
tab[1,4] <- 0
tab[1,5] <- 1

#Walmart
sdretWMT <- sd(erWMT)
alphaWMT <- regrWMT$coefficients[1]
betaWMT  <- regrWMT$coefficients[2]
resWMT   <- regrWMT$residuals
sigmaWMT <- sd(resWMT)
corWMTm  <- cor(erWMT,erm)
tab[2,1] <- sdretWMT
tab[2,2] <- betaWMT
tab[2,3] <- betaWMT*sigmaM
tab[2,4] <- sigmaWMT
tab[2,5] <- corWMTm
alphas<-c(0.00125,	-0.00083,	-0.00042,	0.00063,	0.00100,	0.00021)

#Here complete the table tab with the rest of securities

##Target
data    <- merge(erT,erm,all=FALSE)
regrT <- lm(data$erT~data$erm)

tab[3,1] <- sd(erT)
tab[3,2] <- regrT$coefficients[2]
tab[3,3] <- regrT$coefficients[2]*sigmaM
tab[3,4] <- sd(regrT$residuals)
tab[3,5] <- cor(erT,erm)

#VeriZon
data    <- merge(erVZ,erm,all=FALSE)
regrVZ <- lm(data$erVZ~data$erm)

tab[4,1] <- sd(erVZ)
tab[4,2] <- regrVZ$coefficients[2]
tab[4,3] <- regrVZ$coefficients[2]*sigmaM
tab[4,4] <- sd(regrVZ$residuals)
tab[4,5] <- cor(erVZ,erm)

#ATT
data    <- merge(erT,erm,all=FALSE)
regrT <- lm(data$erT~data$erm)

tab[5,1] <- sd(erT)
tab[5,2] <- regrT$coefficients[2]
tab[5,3] <- regrT$coefficients[2]*sigmaM
tab[5,4] <- sd(regrT$residuals)
tab[5,5] <- cor(data$erT,data$erm)


#Ford
data    <- merge(erF,erm,all=FALSE)
regrF<- lm(data$erF~data$erm)

tab[6,1] <- sd(erF)
tab[6,2] <- regrF$coefficients[2]
tab[6,3] <- regrF$coefficients[2]*sigmaM
tab[6,4] <- sd(regrF$residuals)
tab[6,5] <- cor(data$erF,data$erm)

#""GM"
data    <- merge(erGM,erm,all=FALSE)
regrGM<- lm(data$erGM~data$erm)

tab[7,1] <- sd(data$erGM)
tab[7,2] <- regrGM$coefficients[2]
tab[7,3] <- regrGM$coefficients[2]*sigmaM
tab[7,4] <- sd(regrGM$residuals)
tab[7,5] <- cor(data$erGM,data$erm)

#See Formula Sheet and compute the first step, initial weights, 
#inital weight_i = alpha_i/var residual
w_o = alphas/tab[c(-1),4]^2
print(paste("weights add to ",sum(w_o )))

#Scale those initial positions to force portfolio weights to sum to one!
w_i = w_o/sum(w_o)
print(paste("weights add to ",sum(w_i )))

#Compute the alpha of the active portfolio
alpha_A = w_i%*%alphas

# The initial position in the active portfolio is proportional to the overall index portfolio
#### Perhaps, the tricky part is to find sigma^2 of the residual of the active..
sigma2_eA = (w_i%*%tab[c(-1),4])^2

###For the market, we need to make a macro forecast, 
#####let's assume that mean and variance follows the historical data
#my guess of market return is... 
guess_return_m = mean(data$erm)
guess_variance_m = tab[1,1]^2
w_o_a = (alpha_A/sigma2_eA)/(guess_return_m/guess_variance_m)

# Compute the beta of the active portfolio 
beta_a = w_i%*%tab[c(-1),2]

# Adjust the initial position in the active portfolio using that beta
w_star_a = w_o_a/(1+(1-beta_a)*w_o_a)
w_star_m = 1 - w_star_a
# Calculate returns
return_final = (w_star_m + beta_a*w_star_a)*guess_return_m+ w_star_a*alpha_A
# Calculate variance
variance_final = (w_star_m + beta_a*w_star_a)^2*guess_variance_m+(w_star_a)^2*sigma2_eA
#Congrats, you are done! 
print(paste("The return and variance of the portfolio managed is",round(return_final,5),round(variance_final,5)))

