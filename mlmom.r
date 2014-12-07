#ML Mom
# Q) Can Machine Learning help determine a combination of momentum features to help predict future
# outperformance?
# 
# Possible features:
#   Rank of Raw Momentum (return) RANK_FROM_TO
#   Raw Momentum (return) RET_FROM_TO
#   Rank of Return/SD SHRPRANK_FROM_TO
#   Raw Return/SD SHRP_FROM_TO

#   Acceleration: Change in momentum from one period to another
#       Difference in return  
#       Difference in rank 
#       Difference in sd
#       Difference is sd rank
#       Difference in return/sd 
#       Difference in rank of return/sd 
#   Time periods for features
#       For momentum: mom(from, to) is the momentum from 'from' months back to 'to' months back
#       e.g., if this is January, mom(12,7) would produce a return from the beginning of Jan
#       in the previous year through June of the previous year.  mom(12,1) would represnt the return
#       of the previous year.  mom(1,1) would represent the previous month.
#       Let 'from' and 'to' vary from 12 to 1 by 1. 
#       For acceleration: acc(p1,p2) is the momentum in period p2 minus the momentum in period p1.
#       p2 is the preceding p2 months and p1 is the p1 months before p2.  So in January, acc(9,3)
#       would be the momentum from Oct-Dec (mom(3,1)) of the previous year less the momentum from
#       Jan thru Sep (mom(12,4))
#       Let p1 vary from 1 to 6 and p2 vary from 1 to 11 subject to the condition that p2>=p1; 
#       Var names: ACC_RET_P1_P2; ACC_RANK_P1_P2; ACC_SHRP_P1_P2; ACC_SHRP_RANK_P1_P2 - these are 'raw' values
#                   Ranks of above: RANK_ACC_RET_P1_P2; RANK_ACC_RANK_P1_P2; RANK_ACC_SHRP_P1_P2; RANK_ACC_SHRP_RANK_P1_P2
#                   chgsd_P1_P2; chgsdrank_P1_P2

#   Y variables:
#       Rate of return
#       Rank of return
#       Over next 1 month,3,6 and 12 months (how long is information valid, rate of decay?)
#       Should we care if rank is in topN, e.g., Top3. 

#   Models: Random Forest, SVM, NN,..others logistic
#   Data sets: 10 S&P GICs sectors; 7 Broad Asset Classes, 10 Countries
#   Questions: How to measure result?  What transformations are needed depending on ML Model. Too many features?

# y,x

acdt<-function(AC,DT){
    return(paste(AC,format(DT,"%Y%m%d"),sep=""))
}
library(xts)
library(quantmod)
library(timeSeries)
library(PerformanceAnalytics)
####
#Flexible Asset Allocation
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
source(paste(momdir,"momentum.r",sep=""))
load(paste(datadir,"bbreturnsdaily.rdata",sep=""))
sec.names<-c("S5FINL","S5HLTH","S5UTIL","S5COND","S5INFT","S5INDU","S5CONS","S5MATR","S5TELS","S5ENRS")
data<-returns.daily[,sec.names]/100
data<-data[complete.cases(data),]
rm(returns.daily)

month.end.dates<-index(data)[endpoints(data)]

#### Create data frame with potential Y values: 1, 3,6 and 12 month trailing return and ranks
y.ret<-mom_ac_hist_ret(data,lookback=1,data.type="returns")
y.rank<-mom_ac_ret_rank(data,lookback=1,data.type="returns")
y<-matrix(NA,nrow=length(month.end.dates)*ncol(data),ncol=9,
          dimnames=list(NULL,c("ACDT","RET1","RANK1","RET3","RANK3","RET6","RANK6","RET12","RANK12")))
y<-data.frame(y)
i<-1
for (dt in 1:nrow(y.ret)){
    for (ac in 1:ncol(y.ret)){
        y[i,"ACDT"]<-acdt(colnames(y.ret)[ac],index(y.ret)[dt])
        y[i,"RET1"]<-y.ret[dt,ac]
        y[i,"RANK1"]<-y.rank[dt,ac]
        i<-i+1    
    }
}

fill.y.matrix<-function(y,m){
    ret.str<-paste("RET",m,sep="")
    rank.str<-paste("RANK",m,sep="")
    y.ret<-mom_ac_hist_ret(data,lookback=m,data.type="returns")
    y.rank<-mom_ac_ret_rank(data,lookback=m,data.type="returns")
    i<-1
    for (dt in 1:nrow(y.ret)){
        for (ac in 1:ncol(y.ret)){
            idx<-acdt(colnames(y.ret)[ac],index(y.ret)[dt])
            y[y[,"ACDT"]==idx,ret.str]<-as.numeric(y.ret[dt,ac])
            y[y[,"ACDT"]==idx,rank.str]<-as.numeric(y.rank[dt,ac])
            i<-i+1    
        }
    }
    return(y)
}

for (m in c(3,6,12)){
    y<-fill.y.matrix(y,m)
}
rm(y.ret,y.rank)
#### End of Create data frame with potential Y values: 1, 3,6 and 12 month trailing return and ranks

x<-matrix(NA,nrow=length(month.end.dates)*ncol(data),ncol=78*4+12+36*2+15*2)

cnames<-NULL
for (TO in 1:12){
    for (FROM in TO:12){
        cnames<-c(cnames,paste("ret_",FROM,"_",TO,sep=""))
    }
}
for (TO in 1:12){
    for (FROM in TO:12){
        cnames<-c(cnames,paste("rank_",FROM,"_",TO,sep=""))
    }
}

for (TO in 1:12){
    for (FROM in TO:12){
        cnames<-c(cnames,paste("shrp_",FROM,"_",TO,sep=""))
    }
}
for (TO in 1:12){
    for (FROM in TO:12){
        cnames<-c(cnames,paste("shrprank_",FROM,"_",TO,sep=""))
    }
}

for (i in 1:12){
    cnames<-c(cnames,paste("sd_",i,sep=""))
}
for (P2 in 1:6){
    for (P1 in P2:(12-P2)){
        cnames<-c(cnames,paste("acc_",P1,"_",P2,sep=""))
    }
}
for (P2 in 1:6){
    for (P1 in P2:(12-P2)){
        cnames<-c(cnames,paste("accrank_",P1,"_",P2,sep=""))
    }
}
for (P2 in c(1,3,6)){
    for (P1 in c(11,9,6,3,1)){
        cnames<-c(cnames,paste("chgsd_",P1,"_",P2,sep=""))
    }
}
for (P2 in c(1,3,6)){
    for (P1 in c(11,9,6,3,1)){
        cnames<-c(cnames,paste("chgsdrank_",P1,"_",P2,sep=""))
    }
}
colnames(x)<-cnames
rownames(x)<-y[,"ACDT"]
#x.ret<-mom_ac_hist_ret(data,lookback=1,data.type="returns")

dates.vec<-month.end.dates
month.end.pts<-endpoints(data)
ac.vec<-colnames(data)
monthly.returns<-mom_ac_hist_ret(data,lookback=1,data.type="returns")
for (DT in 1:length(dates.vec)){
    for (AC in 1:length(ac.vec)){
        idx<-acdt(ac.vec[AC],dates.vec[DT])
        # Ret and Ranks
        for (TO in 1:12){
            for (FROM in TO:12){
                if (DT-FROM < 1){
                    temp.ret<-NA
                    temp.rankret<-NA
                    temp.shrp<-NA
                    temp.rankshrp<-NA
                } else {
                    temp.ret<-apply(1+monthly.returns[(DT-FROM):(DT-TO),],2,prod)-1
                    temp.sd<-apply(data[((month.end.pts[DT-FROM]+1):month.end.pts[DT+1-TO]),],2,sd)
                    temp.shrp<-temp.ret/temp.sd
                    temp.rankret<-rank(-temp.ret,ties.method="random")
                    temp.rankshrp<-rank(-temp.shrp,ties.method="random")
                    temp.ret<-temp.ret[AC]
                    temp.rankret<-temp.rankret[AC]
                    temp.shrp<-temp.shrp[AC]
                    temp.rankshrp<-temp.rankshrp[AC]
                }
                colname<-paste("ret_",FROM,"_",TO,sep="")
                x[idx,colname]<-temp.ret
                colname<-paste("rank_",FROM,"_",TO,sep="")
                x[idx,colname]<-temp.rankret
                colname<-paste("shrp_",FROM,"_",TO,sep="")
                x[idx,colname]<-temp.shrp
                colname<-paste("shrprank_",FROM,"_",TO,sep="")
                x[idx,colname]<-temp.rankshrp
            }
        }
        #End of Ret and Ranks
        #Start of SD
        for (SD in 1:12){
            if (DT-SD < 0){
                temp.sd<-NA
            } else {
                temp.sd<-sd(data[((month.end.pts[DT+1-SD]+1):month.end.pts[DT+1]),AC],na.rm=T)
            }
            colname<-paste("sd_",SD,sep="")
            x[idx,colname]<-temp.sd
        }
        #End of SD
        #Start of ACC
        for (P2 in 1:6){
            for (P1 in P2:(12-P2)){
                if (DT-P1-P2 < 1){
                    temp.ret<-NA
                    temp.rank<-NA
                } else {
                    temp.retP1<-apply(1+monthly.returns[(DT-P1-P2):(DT-P2-1),],2,prod)-1
                    temp.retP2<-apply(1+monthly.returns[(DT-P2):(DT-1),],2,prod)-1
                    temp.ret<-temp.retP2-temp.retP1
                    temp.rank<-rank(-temp.ret,ties.method="random")
                    temp.ret<-temp.ret[AC]
                    temp.rank<-temp.rank[AC]
                }
                colname<-paste("acc_",P1,"_",P2,sep="")
                x[idx,colname]<-temp.ret
                colname<-paste("accrank_",P1,"_",P2,sep="")
                x[idx,colname]<-temp.rank
            }
       }
        #End of ACC
        #End of SD chg and Rank
        for (P2 in c(1,3,6)){
           for (P1 in c(11,9,6,3,1)){
               temp.sd<-NA
               temp.rank<-NA
               if (DT-P1-P2 >= 1 & P1>=P2 & P1+P2<=12){
                   temp.sdP1<-apply(data[((month.end.pts[DT+1-P1-P2]+1):month.end.pts[DT+1-P2]),],2,sd)
                   temp.sdP2<-apply(data[((month.end.pts[DT+1-P2]+1):month.end.pts[DT+1]),AC],2,sd)
                   temp.sd<-temp.sdP2-temp.sdP1
                   temp.rank<-rank(temp.sd,ties.method="random")
                   temp.sd<-temp.sd[AC]
                   temp.rank<-temp.rank[AC]
               }
               colname<-paste("chgsd_",P1,"_",P2,sep="")
               x[idx,colname]<-temp.sd
               colname<-paste("chgsdrank_",P1,"_",P2,sep="")
               x[idx,colname]<-temp.rank
           }
        }
        #End of SD chg and Rank
    } #end for each AC 
} #end for each Date
save(x,y,file="mlmom1_sector.rdata")

