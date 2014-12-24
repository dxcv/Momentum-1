# Model based on 12 month dollar cost averaging.  Simulate 12 staggered portfolios
# Each holds top.n asset classes for 12 months and then rebalances to the new top classes.
# Optional: if abs return over some look-back period, hold cash/ safe investment
library(xts)
library(quantmod)
library(timeSeries)
library(PerformanceAnalytics)
library(lubridate)

####
# initial exploration of modeling
####
location<-"home"
wd.home<-"~/Quant Trading/Momentum/"
momdir.home<-"~/Quant Trading/Momentum/"
datadir.home<-"~/Quant Trading/datasets/"
wd.work<-"G:/Atlanta/All Departmental Data/Research Department/Asset Allocation/Tactical Asset Allocation Models/Momentum/"
momdir.work<-"G:/Atlanta/All Departmental Data/Research Department/Asset Allocation/Tactical Asset Allocation Models/FlexibleAA/"
datadir.work<-"G:/Atlanta/All Departmental Data/Research Department/Asset Allocation/Tactical Asset Allocation Models/datasets/"

if (location=="home"){
    wd<-wd.home
    momdir<-momdir.home
    datadir<-datadir.home
} else {
    wd<-wd.work
    momdir<-momdir.work
    datadir<-datadir.work
}
setwd(wd)
load("mlmom1_sector.rdata")

rank_var<-"rank_4_1"
absmom_var<-"ret_4_1"
ret_var<-"RET1"
top.n<-2
use.abs<-TRUE

xyret.df<-data.frame(x,y) 
xyret.df<-xyret.df[,c(rank_var,absmom_var,ret_var)]
xyret.df<-xyret.df[complete.cases(xyret.df),]

n.assets<-max(xyret.df[,rank_var])
n.months<-nrow(xyret.df)/n.assets
asset.class.names<-vector(mode="character",length=n.assets)
for (i in 1:n.assets){
    asset.class.names[i]<-substr(row.names(xyret.df)[i],1,nchar(row.names(xyret.df)[i])-8)
}

idx<-seq(1,nrow(xyret.df),n.assets)
temp<-row.names(xyret.df)[idx]
temp<-substr(temp,nchar(temp)-8+1,nchar(temp))
x.dates<-as.Date(temp,"%Y%m%d")

port.returns<-matrix(NA,nrow=n.months,ncol=12) #col 1=Jan, col 12=Dec
bench.returns<-matrix(NA,nrow=n.months,ncol=1) #col 1=Jan, col 12=Dec
last.mo<-0
return.row<-0
for (i in 1:nrow(xyret.df)){ #for each row
    temp<-row.names(xyret.df[i,])
    mo<-as.numeric(substr(temp,nchar(temp)-4+1,nchar(temp)-2)) # get the month = portfolio
    ac<-i %% n.assets # asset class
    if (mo != last.mo){
        return.row<-return.row+1
        idx<-seq(from=i,by=n.assets,length.out=12)
        idx<-idx[(idx<=nrow(xyret.df))]
        port.returns[return.row:(return.row+length(idx)-1),mo]<-0
        bench.returns[return.row:(return.row+length(idx)-1),1]<-mean(xyret.df[i:(i+n.assets-1),ret_var])
    }
    if (xyret.df[i,rank_var]<=top.n){  #invest in this ac for next 12 mos depending on absmom
        idx<-seq(from=i,by=n.assets,length.out=12)
        idx<-idx[(idx<=nrow(xyret.df))]
        temp<-xyret.df[idx,]
        if (use.abs) {
            temp[temp[,absmom_var]<0,ret_var]<-0
        }
        port.returns[return.row:(return.row+nrow(temp)-1),mo]<-port.returns[return.row:(return.row+nrow(temp)-1),mo]+temp[,ret_var]/top.n        
    }
    last.mo<-mo
}

y.dates<-as.character(x.dates[2:length(x.dates)])
temp<-tail(y.dates,1)
temp1<-as.Date(temp,"%Y-%m-%d") %m+% months(1)
y.dates<-c(y.dates,as.character(temp1))

strategy.returns<-port.returns[complete.cases(port.returns),]
temp<-apply(1+strategy.returns,2,cumprod)
temp1<-rowSums(temp)
temp2<-c(12,temp1)
temp2<-data.frame(temp2)
row.names(temp2)<-as.Date(y.dates[11:length(y.dates)],"%Y-%m-%d")
temp3<-as.xts(temp2)
plot(temp3)
temp4<-CalculateReturns(temp3)
temp5<-temp4[complete.cases(temp4)]
chart.CumReturns(temp4)
temp6<-bench.returns[12:nrow(bench.returns),]
temp7<-cbind(temp5,temp6)
table.AnnualizedReturns(temp7)