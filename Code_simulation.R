
library(abind)
library(ggplot2)

# load dataset  -----------------------------------------------------------

load("Data_for_simulation_withoutID.RData")

dta$agegr=cut(dta$base_age+1,breaks = c(0,20,30,35,Inf),
              right = F,include.lowest = F)

# counterfactual transition probability calculation------------------------

for (change in c("=35","=25","=15","=10")) {
  
  # infertility rate
  load("Infertrate_ERF_risk assessment.RData") #loading ERF
  ref=as.numeric(gsub("=","",change))
  rr=sapply(exp(sim[as.character(round(dta$x1,1)),1]-sim[as.character(round(ref,1)),1]),function(x) max(x,1))
  rr[which(dta$x1<=ref)]=1
  dta[,paste("r.infert.",change,sep="")]=dta$r.infert.idv/rr

  # stillbirth rate
  load("ERFs 4 types for stillbirth and PM25.RData") # loadong ERF
  ref=as.numeric(gsub("=","",change))
  rr=sapply(exp(ERFs[cbind(as.character(round(dta$x2,1)),as.character(dta$agegr),1)]-
                    ERFs[cbind(as.character(round(ref,1)),as.character(dta$agegr),"1")]),function(x) max(x,1))
  rr[which(dta$x2<=ref)]=1
  dta[,paste("r.stb.",change,sep="")]=dta$r.stb.idv/rr

  # infant mortality rate
  load("ERF_pregnancyexp_infant_dta.RData") # loading ERF
  ref=as.numeric(gsub("=","",change))
  rr=sapply(exp(sim[as.character(round(dta$x2,1)),1]-sim[as.character(round(ref,1)),1]),function(x) max(x,1))
  rr[which(dta$x2<=ref)]=1
  dta[,paste("r.infdeath.",change,sep="")]=dta$r.infdeath.idv/rr
  
  print(change)
}


# event simulation --------------------------------------------------------

set.seed(1020)
simu.infert=t(apply(data.frame(dta$r.infert.idv),1,function(x) rbinom(n=500,size=1,prob=x)))
simu.misc=t(apply(data.frame(dta$r.misc),1,function(x) rbinom(n=500,size=1,prob=x)))
simu.stb=t(apply(data.frame(dta$r.stb.idv),1,function(x) rbinom(n=500,size=1,prob=x)))
simu.infdeath=t(apply(data.frame(dta$r.infdeath.idv),1,function(x) rbinom(n=500,size=1,prob=x)))
simu.origin=abind(simu.infert,simu.misc,simu.stb,simu.infdeath,along=3)

simu=NULL
for (change in c("=35","=25","=15","=10")) {
  simu.infert=t(apply(data.frame(dta[,paste("r.infert.",change,sep="")]),1,function(x) rbinom(n=500,size=1,prob=x)))
  simu.misc=t(apply(data.frame(dta$r.misc),1,function(x) rbinom(n=500,size=1,prob=x)))
  simu.stb=t(apply(data.frame(dta[,paste("r.stb.",change,sep="")]),1,function(x) rbinom(n=500,size=1,prob=x)))
  simu.infdeath=t(apply(data.frame(dta[,paste("r.infdeath.",change,sep="")]),1,function(x) rbinom(n=500,size=1,prob=x)))
  simu.ref=abind(simu.infert,simu.misc,simu.stb,simu.infdeath,along=3)
  simu=abind(simu,simu.ref,along=4)
}
simu=abind(simu.origin,simu,along=4)
dimnames(simu)[[3]]=c("infertility","miscarriage","stillbith","infantdeath")
dimnames(simu)[[4]]=c("real","=35","=25","=15","=10")

# aggregating simulation result ------------------------------------------------------

proc1=simu[,,1,] # infertility
proc2=apply(simu[,,1:2,],c(1,2,4),function(x) as.numeric(all(x==c(0,1)))) # miscarriage
dim(proc2)=dim(simu)[c(1,2,4)]
proc3=apply(simu[,,1:3,],c(1,2,4),function(x) as.numeric(all(x==c(0,0,1)))) # stillbirth
dim(proc3)=dim(simu)[c(1,2,4)]
proc4=apply(simu,c(1,2,4),function(x) as.numeric(all(x==c(0,0,0,1)))) # infant death
dim(proc4)=dim(simu)[c(1,2,4)]
# save(simu,proc1,proc2,proc3,proc4,file="Overall_simu_proc.RData")

# overall
weight=dta$survey.weight
tmp1=apply(proc1[,,],c(2,3),function(x) sum(weight[which(x==0)]))  
tmp2=0-apply(proc2[,,],c(2,3),function(x) sum(weight[which(x==1)]))  
tmp3=0-apply(proc3[,,],c(2,3),function(x) sum(weight[which(x==1)]))  
tmp4=0-apply(proc4[,,],c(2,3),function(x) sum(weight[which(x==1)])) 
tmp5=tmp1+tmp2+tmp3+tmp4
tmpw=abind(tmp1,tmp2,tmp3,tmp4,tmp5,along = 3)

dimnames(tmpw)[[2]]=dimnames(simu)[[4]]
dimnames(tmpw)[[3]]=c("pregnancy","miscarriage","stillbirth","infantdeath","infant")
save(tmpw,file="simulation_result_overall.RData")

# figure_overall effect ----------------------------------------------

load("simulation_result_overall.RData")
tmp=tmpw
ncol=dim(tmp)[2]
dd2=rbind(data.frame(scenario=dimnames(tmp)[[2]][-1],
                     end="Pregnancy",
                     mean=apply(tmp[,2:ncol,1]-tmp[,1,1],2,mean),
                     lo=apply(tmp[,2:ncol,1]-tmp[,1,1],2,quantile,prob=0.025),
                     up=apply(tmp[,2:ncol,1]-tmp[,1,1],2,quantile,prob=0.975)
  ),
  data.frame(scenario=dimnames(tmp)[[2]][-1],
             end="Birth",
             mean=apply(apply(tmp[,2:ncol,1:3],c(1,2),sum)-apply(tmp[,1,1:3],1,sum),2,mean),
             lo=apply(apply(tmp[,2:ncol,1:3],c(1,2),sum)-apply(tmp[,1,1:3],1,sum),2,quantile,prob=0.025),
             up=apply(apply(tmp[,2:ncol,1:3],c(1,2),sum)-apply(tmp[,1,1:3],1,sum),2,quantile,prob=0.975)
  ),
  data.frame(scenario=dimnames(tmp)[[2]][-1],
             end="Infant",
             mean=apply(tmp[,2:ncol,5]-tmp[,1,5],2,mean),
             lo=apply(tmp[,2:ncol,5]-tmp[,1,5],2,quantile,prob=0.025),
             up=apply(tmp[,2:ncol,5]-tmp[,1,5],2,quantile,prob=0.975)
  )
)

dd2$end=factor(dd2$end,levels=unique(dd2$end))
dd2$scenario=factor(dd2$scenario,levels=unique(dd2$scenario),
                    labels = c("WHO IT1: 35"~mu~"g/m"^3,
                               "WHO IT2: 25"~mu~"g/m"^3,
                               "WHO IT3: 15"~mu~"g/m"^3,
                               "WHO IT4: 10"~mu~"g/m"^3))

dd2$num=data.frame(Var1="all",Freq=sum(dta$survey.weight))[,"Freq"]
dd2$pctmean=dd2$mean/dd2$num*1000
dd2$pctlo=dd2$lo/dd2$num*1000
dd2$pctup=dd2$up/dd2$num*1000

fig=ggplot(data=dd2) +  
  geom_bar(aes(x=as.numeric(end),y = pctmean),stat="identity",
           fill="#4DBBD5CC",show.legend = F,width=0.6)+
  geom_linerange(aes(x=as.numeric(end), ymin = pctlo, ymax=pctup))+
  facet_wrap(vars(scenario),labeller = label_parsed)+
  scale_x_continuous(name=NULL,breaks = 1:3,labels=c("Pregnancy","Birth","Infant"))+
  scale_y_continuous(name="Increasing number per 1000 reprodutcive-aged\n women with pregnancy potential",
                     expand=c(0.02,0.1),breaks=seq(0,25,5))+
  theme_test() +  
  theme(  
    legend.position = "none",  
    axis.text = element_text(size=14,color="black"),
    axis.title = element_text(size=14),
    strip.text = element_text(size=15),
    strip.background = element_rect(fill=NA)
  )

print(fig)


