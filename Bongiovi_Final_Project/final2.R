library(quantmod)
library(PerformanceAnalytics)
#library(fitdistrplus)
#library(metRology)
library(e1071)
#library(rugarch)
library(quadprog)

#calculate log difference)
logdiff = function(d){
  len = length(d[,1])
  rets =  d[,]
  for(j in 1:length(rets[1,])){
    rets[1:len,j] = diff(log(rets[,j]))
  }
  rets = rets[2:len,]
  return(rets)
}

ndx = read.csv("HW1-portfolio.csv",header = TRUE,fill = FALSE)
stock_port = subset(ndx,select = Ticker)
weights = subset(ndx,select = Sharesout)
symbols = as.vector(stock_port$Ticker)

#macro_var = c("DGS1","T10YIE","T10Y2Y","BAA10Y")

dataset <- xts()
#FREDdataset <- xts()

n <- length(symbols)
pb <- txtProgressBar(min = 0, max = n, style = 3)

for(i in 1:length(symbols)){
  symbols[i] -> symbol
  tryit <- try(getSymbols(symbol, from = "2008-04-01", to = "2019-08-31", src = 'yahoo'))
  if(inherits(tryit, "try-error")){
    i <- i+1
  }
  else{
    data <- getSymbols(symbol, from= "2008-04-01",to = "2019-08-31", src = 'yahoo')
    dataset <- merge(dataset, Ad(get(symbols[i])))
    rm(symbol)
  }
  setTxtProgressBar(pb,i)
}
#for(i in 1:length(macro_var)){
 # macro_var[i] -> symbol
#  data <- getSymbols(symbol, from = "2008-04-01", to = "2019-08-31", src = "FRED")
#  FREDdataset <- merge(FREDdataset,get(macro_var[i]))
#  rm(symbol)
#}
class(dataset)
#class(FREDdataset)
#FREDdataset = FREDdataset[complete.cases(FREDdataset),]

WeightB = as.numeric(t(as.matrix(weights))*as.matrix(dataset[1,]))/(as.matrix(t(weights))%*%t(as.matrix(dataset[1,])))[1,1]

n = length(dataset[,1])
Lrets = dataset[,]
Lrets = logdiff(Lrets)
skewdata = c()
kurtdata = c()
ret = c()
vol = c()
cut = 63
#daily = 1
#monthly = 21
#quarterly = 63
#yearly = 252
returns = cumsum(as.xts(Lrets))
names = c()
for(i in 1:length(symbols)){
  start = 1
  end = 64
  tempskew = c()
  tempkurt = c()
  tempret = c()
  for(j in 1:(length(Lrets[,1]) - cut)){ #roll from 1 to the end of the dataset - averaging size
    tempskew = rbind(tempskew,skewness(Lrets[(start:end),i],type = 1)) #rolling Skew calculation
    #type 1 normalizes the measure
    tempkurt = rbind(tempkurt , (kurtosis(Lrets[(start:end),i]))) #rolling kurtosis calculation
    x = as.vector(returns[(end),i]) - as.vector(returns[(start),i]) #individual stock returns
    
    tempret = rbind(tempret, x)
    
    #row.names(tempret)[j] = index(returns[(end),i])
    #print(end)
    start = start + 1
    end = end + 1 
 }
  skewdata = cbind(skewdata,tempskew)
  kurtdata = cbind(kurtdata,tempkurt)
  ret = cbind(ret,tempret)
}
colnames(skewdata) = c(symbols)
colnames(kurtdata) = c(symbols)

avgskew = rowMeans(skewdata)
reg = Lrets[((cut + 1):length(Lrets[,1])),]

ret = ret[((cut + 1):length(ret[,1])),]
avgskew = avgskew[1:(length(avgskew) - cut)]

portReturns = returns%*%WeightB
portReturns = as.matrix(portReturns)
row.names(portReturns) = c(row.names(as.matrix(returns)))
portReturns = as.xts(portReturns)

colnames(ret) = c(symbols)

optim_port = c()
Bench_ret = c()
TE = c()
active_weight = c()
active_risk = c()
Lval = c()
Weight_Benchmark_i = c()
IR = c()
dates = c()
ddd = rep(1/30,30)
equal_weight_returns = c()
t_stat_table_ret = c()
t_stat_table_lovar_skew = c()
#t_Stat_table_lovar_kurt = c()
skew_regress_est_table = c()

for(j in 0:30){
  #now we want to predict the next 63 days out
  
  t = 252*3 + j*63 #tfinal
  ts = 1 + j*63  #t-start
  
 
  #these are the times to check for the regression
  #after each iteration we push t-start and t-final out 63 days to our predicted value
  #we then use the new values to calculate a new regression and then predict out
  #another 63 days
  #do this until data is exhausted
  #if done correctly, data should be approximately 28 predictions long, i.e 7 years * 4 quarters
  stock.fit = lm(ret[(ts:(t)),] ~ avgskew[(ts:(t))])
  t_stat_table_ret = c(t_stat_table_ret,coef(summary(stock.fit)))
  
  dpnext = avgskew[t+1]
  skew_regress_est_table =rbind(skew_regress_est_table,dpnext)

  xpredict = predict(stock.fit,data.frame(dpnext)) #here we predict the return of the stock
  realvol = ret^2
  mu = xpredict[1,] #save return for optimization
  kurtregress = kurtdata[(1:(length(kurtdata[,1]) - cut)),]
  skewregress = skewdata[(1:(length(skewdata[,1]) - cut)),]

  volest = c()
for(i in 1:length(symbols)){
  vol.fit = lm(realvol[(ts:(t)),i]~ kurtregress[(ts:(t)),i] + abs(skewregress[(ts:t),i]))
  #snext = abs(skewregress[(t+1),i])
  #knext = kurtregress[(t+1),i]
  #newdata = data.frame(knext,snext)
  #colnames(newdata) = c("kurtregress[(1:(t)), i]","abs(skewregress[(1:t), i])")
  volpred = predict(vol.fit,newdata = data.frame(kurtregress[(t+1),i],abs(skewregress[(t+1),i])))
  volest = cbind(volest,volpred[1])
  t_stat_table_lovar_skew = rbind(t_stat_table_lovar_skew,coef(summary(vol.fit))[,3])
  
}
  
  #now that we have the estimates of mu and vol we can optimize portfolio
  #caveats:
  # no beta bet, calculate betas for last 3 years
  
  Benchmark_diff = diff(portReturns)
  Benchmark_diff = Benchmark_diff[((cut + ts - 1):(t + cut - 1)),]
  Beta = Lrets[((cut*2+ ts - 1):(t + cut*2 - 1)),]
  
  cor_m = cor(Beta) #correlation matrix needed for optimization of the portfolio matrix
  #i need a correlation esimate to minimize the estimated covariance matrix
  cov_m = cov(Beta) #base covariance matrix for Tracking Error
  Beta = BetaCoVariance(Beta,Benchmark_diff) #calculated daily beta for last 3 years of data
  volest = exp(volest)*diag(cov_m)*63
  
  #no shorts, no leverage, Tracking Error = 0.03 or 0.0009 = Wa'*cov*Wa
  #we can solve using quadratic programming numerically
  #we can use package quadprog to solve the convex function with constraints
  
  #we can choose what kind of covariance matrix we want to minimize for our optimization program
  #first D designates using estimated variances but uses historical correlations
  
  D = sqrt(diag(as.numeric(volest)))%*%cor_m%*%sqrt(diag(as.numeric(volest)))
  
  #second D designates using the esimated variance but assumes zero correlation between assets
  #D = diag(as.numeric(volest)) 
  
  #third D uses just a historical covariance matrix
  #D = cov_m*63
  A = rep(1,length(symbols))
  ones = rep(1,length(symbols))
  ones = diag(ones)
  zero = rep(0,length(symbols))
  b0 = rbind(1,1)
  b0 = append(b0,zero,after = length(b0))

  A = cbind((A),as.vector(t(Beta)),ones)

  mu = as.matrix(mu)
  L = 0.5
  
  TE = 1
  x = c()
  eee = as.numeric(t(as.matrix(weights))*as.matrix(dataset[(t + 1 + 63*2),]))/(as.matrix(t(weights))%*%t(as.matrix(dataset[(t + 1 + 63*2),])))[1,1]
  #eee2 = as.numeric(t(as.matrix(weights)))*as.matrix(returns[(t+1+63*2),])/(as.matrix(t(weights))%*%t(as.matrix(returns[(t+1+63*2),])))[1,1]
  #if you want to use no risk aversion the loops need to be commented out
   while(TE > 0.0009){
   if(L > 100000){
     break
    }
    else{
    L = L*2
    W = solve.QP(Dmat = L*D,dvec = (1)*mu,Amat = A, bvec = b0,meq = 2)
  
    x = W$solution
    TE = t(x - eee)%*%(cov_m*63)%*%(x-eee)
    
    }
  }
    active_weight = rbind(active_weight,x)
  
    #x2 = W$unconstrained.solution
  
    optim_port = rbind(optim_port,t(x)%*%ret[(t+64),])
    equal_weight_returns = rbind(equal_weight_returns,t(ddd)%*%ret[(t+64),]) 
    Bench_ret = rbind(Bench_ret,as.numeric(portReturns[(t + 1 + 63*3)]) - as.numeric(portReturns[(t + 1 + 63*2)]))
    dates = rbind(dates,as.character(index(portReturns[(t+1+63*3)])))  
    
    Weight_Benchmark_i = rbind(Weight_Benchmark_i, eee)
  #row.names(Weight_Benchmark_i)[i,] = c()
    
    active_risk = rbind(active_risk,TE)
    Lval = rbind(Lval,L)  
    IR = rbind(IR,sqrt(TE)*2*L)
    
  
}
IC = IR/sqrt(4)
colnames(Weight_Benchmark_i) = c(symbols)
colnames(active_weight) = c(symbols)
rownames(optim_port) = c(dates)
rownames(Bench_ret) = c(dates)
rownames(equal_weight_returns) = c(dates)
#fix out of bounds error
postrisk = sqrt(var(optim_port - Bench_ret))*sqrt(4)
postalpha = mean(optim_port - Bench_ret)*4
sqrt(active_risk)
plot(cumsum(as.xts(optim_port)),type = "l")
lines(cumsum(as.xts(Bench_ret)),type = "l",col ="red")
lines(cumsum(as.xts(equal_weight_returns)),type = "l",col = "blue")
postIR = postalpha/postrisk
postIC = postIR/sqrt(4)


#equal_weight = as.matrix(returns)%*%as.matrix(ddd)
#equal_weight = as.matrix(equal_weight)
#rownames(equal_weight) = c(rownames(as.matrix(returns)))
#equal_weight = as.xts(equal_weight)
table1 = cbind(sqrt(active_risk),IR,IC) #ex-ante
table2 = cbind(postrisk,postIR,postIC,postalpha) #post-hoc
colnames(table2) = c("post-TE","post-IR","post-IC","post-alpha")
colnames(table1) = c("ex-ante-TE","ex-ante-IR","ex-ante-IC")
#write.table(table1,file = "ex_ante.csv",sep = ",",col.names = TRUE)
#write.table(table2,file = "post_hoc.csv",sep = ",",col.names = TRUE)
t_stat_table_lovar_skew = as.matrix(t_stat_table_lovar_skew)
rownames(t_stat_table_lovar_skew) = c(rep(symbols,31))


#algo to extract t-stat averages and variances
#write.table(t_stat_table_ret,file = "ret.csv",sep = ",")
#t_stats_table = read.csv("ret.csv",header = TRUE,fill = FALSE)
#t_stats_table = t(t_stats_table)
#ttt = c()
#for(i in (1:length(t_stats_table[,1]) + 1)){
#  if(i%%4 == 0){
#    ttt = rbind(ttt,t_stats_table[i-1,])
#  }
#}
#diag(cov(ttt))
#colMeans(ttt)