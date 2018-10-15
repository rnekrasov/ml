#********************************************************
data <- dbGetQuery(con, "SELECT * from usdrub 
where time between '10:00:00' and '10:05:00'
and ticker='USD000UTSTOM' 
and date='15-10-2018' order by time")
ticker<-"USD000UTSTOM"
#********************************************************
  
#dbGetQuery(con, "DELETE from param where time between '10:00:00' and '10:05:00' 
#and ticker='USD000UTSTOM' and date='01-02-2018'")
library(stochvol);library(timeDate);library(highfrequency)
data$price=as.numeric(data$price)
ret=log(data$price[2:length(data$price)]/data$price[1:length(data$price)-1])
res <- svsample(ret, draws = 5000, burnin = 1000,
                  priormu = c(-10, 1), priorphi = c(20, 1.5), priorsigma = 0.2)
myresid <- resid(res)
time=data$time
#-------------------------------------------------
z1 <- data.frame("2017-01-09",data)
z2 <- data.frame(as.POSIXct(paste(z1[,1], z1$time), 
                              format="%Y-%m-%d %H:%M:%S",tz="GMT"),data$price)
  z3=xts(z1$price, order.by=as.timeDate(z2[,1]))
#-------------------------------------------------
library(outliers);library(extremevalues)
x1=1;x2=length((ordered(data$time)))
x=data$time[x1:x2];y=data$price[x1:x2]
ret=log(data$price[2:length(data$price)]/data$price[1:length(data$price)-1])
#-------------------------------------------------
K <- getOutliers(y,method="I",distribution="lognormal")
L <- getOutliers(y,method="II",distribution="lognormal")
#-------------------------------------------------
#mlr (автокодировщик №1)
library(mlr);library(h2o)
dat=data.frame(data$number,data$time,data$price)
regrTask=makeRegrTask(data = dat, target = "data.price")
m = makeLearner("regr.h2o.deeplearning")
fit = train(m, regrTask)
res = predict(fit, regrTask)
delta=abs(res$data$response-res$data$truth)
#-------------------------------------------------
#autoencoder (автокодировщик №2)
library(autoencoder)
x=as.vector(data$number)
y=as.vector(data$price)
training.matrix=matrix(c(y),ncol=1)
## Set up the autoencoder architecture:
nl=3 ## number of layers (default is 3: input, hidden, output)
unit.type = "tanh" ## specify the network unit type, i.e., the unit's
## activation function ("logistic" or "tanh")
N.hidden = 10*10 ## number of units in the hidden layer
lambda = 0.0002 ## weight decay parameter
beta = 6 ## weight of sparsity penalty term
rho = 0.01 ## desired sparsity parameter
epsilon <- 0.001 ## a small parameter for initialization of weights
max.iterations = 2000 ## number of iterations in optimizer
autoencoder.object <- autoencode(X.train=training.matrix,X.test=training.matrix,nl=nl,N.hidden=N.hidden,
                                   unit.type=unit.type,lambda=lambda,beta=beta,rho=rho,epsilon=epsilon,
                                   optim.method="BFGS",max.iterations=max.iterations,
                                   rescale.flag=TRUE,rescaling.offset=0.001)
X.output <- predict(autoencoder.object, X.input=training.matrix, hidden.output=FALSE)$X.output
error=X.output-y
#-------------------------------------------------
#h2o.anomaly (автокодировщик №3)
library(h2o)
data2=data.frame(data$number,data$price)
data3=data$time
model.hex <- as.h2o(data2)
#activation="Tanh", "TanhWithDropout", "Rectifier","RectifierWithDropout", "Maxout", "MaxoutWithDropout"
#гиперболический тангенс Tanh
model.dl = h2o.deeplearning(x=2,training_frame=model.hex,autoencoder = TRUE,
                              hidden = c(20,10,20),epochs = 100,max_w2 = 10, l1=1e-5,
                              activation="Tanh")
model.anon.per.feature = h2o.anomaly(model.dl, model.hex, per_feature=TRUE)
#выпрямитель ReLU с dropout
model.dl2 = h2o.deeplearning(x=2,training_frame=model.hex,autoencoder = TRUE,
                               hidden = c(20,10,20),epochs = 100,max_w2 = 10, l1=1e-5,
                               activation="RectifierWithDropout")
model.anon.per.feature2 = h2o.anomaly(model.dl2, model.hex, per_feature=TRUE)
#NEW гиперболический тангенс Tanh
model.dl = h2o.deeplearning(x=2,training_frame=model.hex,autoencoder = TRUE,
                              hidden = c(3,2,3),epochs = 100,max_w2 = 10, l1=1e-5,
                              input_dropout_ratio=0.2,
                              activation="TanhWithDropout")
model.anon.per.feature3 = h2o.anomaly(model.dl, model.hex, per_feature=TRUE)
#-------------------------------------------------
library(kernlab);library(e1071);library(np)
date=as.character(data$date[1])
if (ticker=="USD000UTSTOM")
  {
    delta2=0.01
    delta4=0.015
  }
if (ticker=="BRZ7")
  {
    delta2=0.035
    delta4=0.05
  }
if (ticker=="SBER")
  {
    delta2=0.09
    delta4=0.12
  }
if (ticker=="GDZ7")
  {
    delta2=0.24 
    delta4=0.36 
  }
if (ticker=="EDZ7")
  {
    delta2=0.00015
    delta4=0.0003
  }
x1=1;x2=length((ordered(data$time)))
x=data$time[x1:x2];y=data$price[x1:x2]
#bw <- npregbw(xdat=ordered(x), ydat=y)
fit.lc <- npksum(txdat=x, tydat=y, bws=bw$bw)$ksum/npksum(txdat=x, bws=bw$bw)$ksum
par(mfrow = c(1, 1))
z6=ifelse(abs(y-fit.lc)>delta2,y,NA)
i<-0;j<-0
for(i in 1:length(z6)){
    if(abs(y[i]-fit.lc[i])>delta2)
      j <- j + 1
  }
print("orange");print(j)
ou1=j
i<-0;j<-0
z7=ifelse(abs(y-fit.lc)>delta4,y,NA)
for(i in 1:length(z7)){
    if(abs(y[i]-fit.lc[i])>delta4)
      j <- j + 1
  }
print("blue");print(j)
  ou2=j
#L$nOut#аутлайеры
#***********************************************************
#mlr
ae1=max(abs(delta))
#autoencoder
ae2=max(abs(error))
#h2o tanh
ae3=max(abs(as.data.frame(model.anon.per.feature$reconstr_data.price.SE)))
#h2o ReUL dropout
ae4=max(abs(as.data.frame(model.anon.per.feature2$reconstr_data.price.SE)))
#h2o tanh NEW
ae5=max(abs(as.data.frame(model.anon.per.feature3$reconstr_data.price.SE)))
#-------------------------------------------------
#***************************************************
a<-data.frame(data$date[1],max(data$price),min(data$price),
                data$time[1],mean(data$price),
                ae1,ae2,ae3,ae4,ae5,ou1,ou2,ticker)
colnames(a) <- c("date", "max","min","time","price","ae1","ae2","ae3",
                   "ae4","ae5","ou1",
                   "ou2","ticker")
dbWriteTable(con, "param",a,append=TRUE,row.names = FALSE)
#***************************************************
#***************************************************
data5 <- dbGetQuery(con, "SELECT * from param 
                      where time between '10:00:00' and '23:50:00' 
                      and ticker='USD000UTSTOM'
                      order by date desc, time desc")
#***************************************************
library(extremevalues)
library(condformat)
  
#ae
x<-data.frame(data5$ae2/mean(data5$ae2),data5$ae3/mean(data5$ae3),
                data5$ae4/mean(data5$ae4))

rank1<-nrow(data5)+1-rank(data5$ae1,ties.method= "max")
rank2<-nrow(data5)+1-rank(data5$ae2,ties.method= "max")
rank3<-nrow(data5)+1-rank(data5$ae3,ties.method= "max")
rank4<-nrow(data5)+1-rank(data5$ae4,ties.method= "max")
rank5<-nrow(data5)+1-rank(data5$ae5,ties.method= "max")
rank6<-nrow(data5)+1-rank(data5$ou1,ties.method= "max")
rank7<-nrow(data5)+1-rank(data5$ou2,ties.method= "max")
rank_sum<-rank2+rank3+rank4+rank6 #old rank sum
rank_sum<-x[,1]+x[,2]+x[,3]#rank2+rank3+rank4 #new rank sum
  
y<-rank_sum
y2<-data5$ou2
y3<-data5$ou1
K <- getOutliers(y,method="I",distribution="lognormal")
L <- getOutliers(y,method="II",distribution="lognormal")
M <- getOutliers(y2,method="II",distribution="normal")
N <- getOutliers(y3,method="II",distribution="normal")
state<-data.frame(ifelse(rank_sum < L$yMin-400,"s1",ifelse(rank_sum < L$yMin-200,
                                                             "s2",ifelse(rank_sum < L$yMin+1,"s3","s4"))))
data14<-data.frame(data5$date,data5$time,data5$price,data5$min,data5$max,
                     data5$ou1,data5$ou2,state,rank_sum,
                     data5$ticker)
data14<-data.frame(data5$date,data5$time,data5$price,data5$min,data5$max,
                    rank_sum,data5$ou2,round(data5$ae3,2),data5$ticker)
colnames(data14) <- c("date","time","price","min","max",
                        "rank_sum","ou2","ae3","ticker")
  
ranking<-rank_sum[length(rank_sum)]
n=150 #the number of entries is displayed on the screen
condformat(data14[1:n,])%>%rule_fill_discrete("rank_sum",expression = rank_sum > L$yMax,colours=c("TRUE"="red"))
time_cluster<-time[1]
rank_cluster<-rank_sum[1]
  
#***********************************************************
head(a)
head(data14,10)
time_cluster
#***********************************************************