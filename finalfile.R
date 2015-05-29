# Load necessary libraries

library(Quandl)
library(ggplot2)
library(reshape2)
library(plyr)

# Data Wrangling ------------------------------------------------------------------------------------
# my function which wrangles the data


getPrice<-function(ListOfSymbols,startdate,enddate)
{
  
  # Date Validation
  
  d <- try( as.Date( startdate, format= "%Y-%m-%d" ) )
  if( class( d ) == "try-error" || is.na( d ) ) {stop( "Start Date isn't valid! It should be of format yyyy-mm-dd" )}
  f <- try( as.Date( enddate, format= "%Y-%m-%d" ) )
  if( class( f ) == "try-error" || is.na( f ) ) {stop( "End Date isn't valid! It should be of format yyyy-mm-dd")}
  
  
  
  stockdf <- data.frame()
  
  start <- as.Date(startdate)
  end <- as.Date(enddate)
  
  # Check if end date is before start date
  if(((end - start)> 0) == FALSE){stop("The end date is before start date or end date is same as start date")}
  
  
  # The snp500 file must be in the current R project folder
  sp500 <- read.csv("SnP500Listing.csv")
  
  
  
  for (symbol in ListOfSymbols)
  {
    dataBaseName<-paste("WIKI/",symbol,sep="")
    
    print (dataBaseName)
    stockPrices<-try(Quandl(dataBaseName, authcode="Gq7ejM7X3aV7GmSxcV5V",start_date=start,end_date=end))
    
    sector <- subset(sp500,Ticker.symbol==symbol,select = GICS.Sector)
  
    if(nrow(sector)==0)
    {
      stop(paste("The ticker symbol",symbol,"does not exist in the SP500 index"))
    }
    
    if(!is.null(nrow(stockPrices)))
    {
    
    refstockPrices <- data.frame(Symbol = symbol,Date = stockPrices$Date , price = stockPrices$Close , sector = sector)
    
    returnexp <- vector()
    for(i in 1:nrow(refstockPrices)-1)
    {
      returnexp <- c(returnexp,(refstockPrices$price[i] - refstockPrices$price[i+1]) /refstockPrices$price[i+1])
      
    }
    returnexp <- c(returnexp,NA)
    
    refstockPrices$returns <- returnexp
    
    stockdf <- rbind(stockdf,refstockPrices)
    print(head(stockPrices))
    }
    
    else
    {
      stop(paste("The API call did not work properly"))
      
    }
    
    
  }
  return(stockdf)
}

# Portfolio Optimization  --------------------------------------------------------------------------
# Function which gets minimum variance portfolio

getminvarportf <- function(Symbolvect,startdate,enddate)
  
{
  

# calling function which wrangles data

stockdf <- getPrice(Symbolvect,startdate,enddate)

print(stockdf)

# this step converts from long to wide
dftempwide <- dcast(stockdf,formula = Date ~ Symbol ,value.val = "returns")

dfwide <- dcast(stockdf,formula = Date ~ Symbol ,value.val = "returns")
dfwide$Date <- NULL

#this step creates covariance matrix

sigma.mat <- cov(dfwide, use="pairwise.complete.obs",method="pearson")

# This step is used to find the expected return and standard deviation

dfn<-ddply(stockdf,.(Symbol),summarize, mean=mean(returns,na.rm = TRUE),sd=sd(returns,na.rm =TRUE))

#final step to find min variance portfolio:

mu.vec = dfn$mean
names(mu.vec) = Symbolvect
size = length(Symbolvect)


top.mat = cbind(2*sigma.mat, rep(1, size))
bot.vec = c(rep(1, size), 0)
Am.mat = rbind(top.mat, bot.vec)
b.vec = c(rep(0, size), 1)
z.m.mat = solve(Am.mat)%*%b.vec
m.vec = z.m.mat[1:size,1]
m.vec

#variance of minimum variance portfolio

sig2.gmin = as.numeric(t(m.vec)%*%sigma.mat%*%m.vec)

returnlist = list(m.vec,sig2.gmin)


return(returnlist)

#expected return of minimum variance portfolio
mu.gmin = as.numeric(crossprod(m.vec, mu.vec))
mu.gmin


}


# Function which finds minimum variance portfolio for given expected return

geteffret <- function(Symbolvec,reqret,startdate,enddate)
{
  
  
  # calling function which wrangles data
  
  stockdf <- getPrice(Symbolvec,startdate,enddate)
  
  print(stockdf)
  
  # this step converts from long to wide
  dftempwide <- dcast(stockdf,formula = Date ~ Symbol ,value.val = "returns")
  
  dfwide <- dcast(stockdf,formula = Date ~ Symbol ,value.val = "returns")
  dfwide$Date <- NULL
  
  #this step creates covariance matrix
  
  sigma.mat <- cov(dfwide, use="pairwise.complete.obs",method="pearson")
  
  # This step is used to find the expected return and standard deviation
  
  dfn<-ddply(stockdf,.(Symbol),summarize, mean=mean(returns,na.rm = TRUE),sd=sd(returns,na.rm =TRUE)) 
  
  
  
#step to find efficient portfolio for particular return
mu.vec = dfn$mean
sd.vec = dfn$sd
names(mu.vec) = Symbolvec
len = length(Symbolvec)
expret = reqret


top.mat = cbind(2*sigma.mat, mu.vec, rep(1, len))
mid.vec = c(mu.vec, 0, 0)
bot.vec = c(rep(1, len), 0, 0)
A.mat = rbind(top.mat, mid.vec, bot.vec)
ret.vec = c(rep(0, len), expret, 1)

z.mat = solve(A.mat)%*%ret.vec
x.vec = z.mat[1:len,]
x.vec

#variance
sig2.gmin = as.numeric(t(x.vec)%*%sigma.mat%*%x.vec)

returnlist = list(x.vec,sig2.gmin)

return(returnlist)

#you can see that the expected return of the portfolio is the same as the one we provided
mu.px = as.numeric(crossprod(x.vec, mu.vec))
mu.px
}


# Test cases:

#initialize test case 1
(mySymbols<-c("GM","MSFT","AAPL"))

start = "2015-05-03"
end = "2015-05-08"

#initialize test case 2
(mySymbols<-c("GM","MSFT","AAPL"))

start = "2015-05-08"
end = "2015-05-03"

#initialize test case 3
(mySymbols<-c("GM","MSFT","aa"))

start = "2015-05-03"
end = "2015-05-08"

# initialize test case 4
(mySymbols<-c("GM","MSFT","aa"))

start = "2015-05-03"
end = "2015-05-03"

#After initializing a test case execute the following
#min variance portfolio - use these to get weights and variance for initialized cases
weights <- getminvarportf(mySymbols,start,end)[1]
#this gives variance of portfolio
portfoliovariance <- getminvarportf(mySymbols,start,end)[2]

print(weights)
print(portfoliovariance)

#effecient portfolio for expected return
expectedret = 0.04
#this gives minimum variance portfolio weights for given expected return
effweights <-  geteffret(mySymbols,expectedret,start,end)[1]
#this gives variance of portfolio
effportfoliovar <-  geteffret(mySymbols,expectedret,start,end)[2]

print(effweights)
print(effportfoliovar)

# Parametric stability analysis ------------------------------------------------------------------

portvarlist = vector()
timeframe = vector()

(mySymbols<-c("GM","MSFT","AAPL"))

#1 month

start = "2015-04-08"
end = "2015-05-08"
timeframe = c(timeframe,"1 month")

portfoliovariance <- getminvarportf(mySymbols,start,end)[2]
portvarlist = c(portvarlist,portfoliovariance)

#6 months

start = "2014-12-08"
end = "2015-05-08"
timeframe = c(timeframe,"six months")

portfoliovariance <- getminvarportf(mySymbols,start,end)[2]
portvarlist = c(portvarlist,portfoliovariance)

#1 year

start = "2014-05-08"
end = "2015-05-08"
timeframe = c(timeframe,"1 year")

portfoliovariance <- getminvarportf(mySymbols,start,end)[2]
portvarlist = c(portvarlist,portfoliovariance)

#5 years

start = "2010-05-08"
end = "2015-05-08"
timeframe = c(timeframe,"5 years")

portfoliovariance <- getminvarportf(mySymbols,start,end)[2]
portvarlist = c(portvarlist,portfoliovariance)

#10 years

start = "2005-05-08"
end = "2015-05-08"
timeframe = c(timeframe,"10 years")

portfoliovariance <- getminvarportf(mySymbols,start,end)[2]
portvarlist = c(portvarlist,portfoliovariance)

#15 years

start = "2000-05-08"
end = "2015-05-08"
timeframe = c(timeframe,"15 years")

portfoliovariance <- getminvarportf(mySymbols,start,end)[2]
portvarlist = c(portvarlist,portfoliovariance)

#20 years

start = "1995-05-08"
end = "2015-05-08"
timeframe = c(timeframe,"20 years")

portfoliovariance <- getminvarportf(mySymbols,start,end)[2]
portvarlist = c(portvarlist,portfoliovariance)


#25 years

start = "1990-05-08"
end = "2015-05-08"
timeframe = c(timeframe,"25 years")

portfoliovariance <- getminvarportf(mySymbols,start,end)[2]
portvarlist = c(portvarlist,portfoliovariance)

#30 years

start = "1985-05-08"
end = "2015-05-08"
timeframe = c(timeframe,"30 years")

portfoliovariance <- getminvarportf(mySymbols,start,end)[2]
portvarlist = c(portvarlist,portfoliovariance)

#Dataframe which shows different portfolio variances according to timeframe
analysisdf = data.frame()
analysisdf = cbind(timeframe,portvarlist)
View(analysisdf)

#Plots the graph between Time frame and Portfolio variance
plot(c(1:length(portvarlist)),portvarlist,'o-',ylim = c(0,.0004),xaxt="n",xlab = "Time frames",ylab = "Portfolio variance",pch=16, col="blue")
axis(1,at = 1:9,timeframe)


# Extra credits - STRATEGY FOR FORMING A PORTFOLIO THAT WILL SATISFY A CLIENT’S RISK TOLERANCE --------

m.vec <- as.numeric(unlist(getminvarportf(mySymbols,start,end)[1]))
x.vec <- as.numeric(unlist(geteffret(mySymbols,expectedret,start,end)[1]))

len = length(mySymbols)

#global min

sig2.gmin = as.numeric(t(m.vec)%*%sigma.mat%*%m.vec)
sig.gmin = sqrt(sig2.gmin)
sig2.px = as.numeric(t(x.vec)%*%sigma.mat%*%x.vec)


a = seq(from=1, to=-1, by=-0.1)
n.a = length(a)
z.mat = matrix(0, n.a, len)
mu.z = rep(0, n.a)
sig2.z = rep(0, n.a)
sig.mx = t(m.vec)%*%sigma.mat%*%x.vec
for (i in 1:n.a) {
  z.mat[i, ] = a[i]*m.vec + (1-a[i])*x.vec
  mu.z[i] = a[i]*mu.gmin + (1-a[i])*mu.px
  sig2.z[i] = a[i]^2 * sig2.gmin + (1-a[i])^2 * sig2.px +
    + 2*a[i]*(1-a[i])*sig.mx
}

#plot the efficient frontier

plot(sqrt(sig2.z), mu.z, type="b", ylim=c(0, 0.06), xlim=c(0, 0.17), pch=16, col="blue", ylab="Expected return", xlab="Risk")
text(sig.gmin, mu.gmin, labels="Global min", pos=4)

# effrontierdf dataframe gives a list of portfolio weights and corresponding expected return and risk
# The client can choose the optimal portfolio according to his risk tolerance

riskdf <- as.data.frame(z.mat)
effrontierdf <- cbind(riskdf,mu.z,sig2.z)

colnames(effrontierdf)<-c(mySymbols,"expected return","risk")
View(effrontierdf)
