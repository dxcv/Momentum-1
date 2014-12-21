library(xts)
library(quantmod)
library(timeSeries)
library(PerformanceAnalytics)

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
load("mlmom1_broad.rdata")


xtable<-function(x,y,rank_var,ret_var="RET1"){
    xyret.df<-data.frame(x,y=y[,ret_var]) #use one month returns for performance
    xyret.df<-xyret.df[complete.cases(xyret.df),]
    xyret.df<-xyret.df[,c(rank_var,"y")] #use one month returns for performance
    colnames(xyret.df)<-c("x","y")
    #xyret.df<-data.frame(x=x[,rank_var],y=y[,"RET1"]) #use one month returns for performance
    #xyret.df<-xyret.df[complete.cases(xyret.df),]
    benchmark.mean<-mean(xyret.df[,"y"])
    benchmark.mean
    
    n.assets<-max(xyret.df[,"x"])
    n.months<-nrow(xyret.df)/n.assets
    mean.ret.by.rank<-by(xyret.df$y,xyret.df$x,mean)
    plot(mean.ret.by.rank,main="Return by Rank",xlab="Rank")
    abline(h=benchmark.mean)
    
    asset.class.returns<-vector(mode="numeric",length=n.assets)
    asset.class.sd<-vector(mode="numeric",length=n.assets)
    asset.class.names<-vector(mode="character",length=n.assets)
    for (i in 1:n.assets){
        idx<-seq(i,nrow(xyret.df),n.assets)
        #ret<-prod(1+xyret.df[idx,"y"])^(12/(nrow(xyret.df)/n.assets))-1
        #ret<-mean(xyret.df[idx,"y"])
        asset.class.returns[i]<-mean(xyret.df[idx,"y"])
        asset.class.sd[i]<-sd(xyret.df[idx,"y"])
        asset.class.names[i]<-substr(row.names(x)[i],1,nchar(row.names(x)[i])-8)
        #xy.df<-data.frame(x[,rank.names],y=y[,"RANK1"])
    }
    barplot(asset.class.returns,main="Asset Class Returns",names.arg=as.list(asset.class.names),
            col="blue",horiz=TRUE,las=1)
    abline(v=benchmark.mean)
    barplot(asset.class.sd,main="Asset Class Std Devs",names.arg=as.list(asset.class.names),
            col="red",horiz=TRUE,las=1)
    
    
    ret.table.sum<-matrix(0,nrow=n.assets,ncol=n.assets)
    ret.table.cnt<-matrix(0,nrow=n.assets,ncol=n.assets)
    colnames(ret.table.cnt)<-asset.class.names
    for (i in 1:n.assets){
        idx<-seq(i,nrow(xyret.df),n.assets)
        for (j in idx){
            # xyret.df[j,"x"] is x value (rank)
            # xyret.df[j,"y"] is y value
            ret.table.sum[xyret.df[j,"x"],i]<-ret.table.sum[xyret.df[j,"x"],i]+xyret.df[j,"y"]
            ret.table.cnt[xyret.df[j,"x"],i]<-ret.table.cnt[xyret.df[j,"x"],i]+1       
        }
    }
    ret.table.freq<-round(100*ret.table.cnt/sum(ret.table.cnt),2) #pct frequency
    ret.table.avg<-ret.table.sum/ret.table.cnt
    ret.table.avg<-rbind(ret.table.avg,asset.class.returns)
    ret.table.avg<-rbind(ret.table.avg,asset.class.sd)
    #ret.table.avg<-cbind(ret.table.avg,rowMeans(ret.table.avg))
    ret.table.avg<-cbind(ret.table.avg,c(rowSums(ret.table.sum)/n.months,benchmark.mean,mean(asset.class.sd)))
    colnames(ret.table.avg)<-c(asset.class.names,"MEAN")
    
    rownames(ret.table.avg)<-as.list(c(paste(rep("Rank",n.assets),seq(1,n.assets),sep=""),"MEAN","SD"))
    ret.table.avg<-round(ret.table.avg*100,2)
    topn<-vector("numeric",n.assets)
    for (i in 1:n.assets){
        topn[i]<-sum(ret.table.sum[1:i,])/sum(ret.table.cnt[1:i,])
    }
    out<-list(ret.table.freq,ret.table.avg,asset.class.returns,mean.ret.by.rank,
           asset.class.sd,benchmark.mean,topn)
    names(out)<-c("Freq.Table","Return.Table","AC.Returns","Rank.Returns",
            "AC.SD","Benchmark.Return","Top.N")
    out
}

rank.names<-colnames(x)[grep("rank",colnames(x))]
n.rank.names<-length(rank.names)
results<-matrix(NA,nrow=n.rank.names,ncol=3)
rownames(results)<-rank.names
colnames(results)<-c("Top1","Top2","Top3")
for (i in 1:n.rank.names){
    temp<-xtable(x,y,rank.names[i],"RET12")
    results[i,1]<-temp$Top.N[1]
    results[i,2]<-temp$Top.N[2]
    results[i,3]<-temp$Top.N[3]
}
results[which.max(results[,1]),]
results[which.max(results[,2]),]
results[which.max(results[,3]),]
