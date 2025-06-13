## =====================================================================================================
## This section runs PRIM accross the experimental results data set
## =====================================================================================================
#cloud
# Set project root dynamically based on the current working directory
root <- file.path(getwd(), "")
#Number.Cores<-30

#load libraries
library(sdtoolkit, lib = file.path(root, "Rlibraries"))
library(data.table, lib = file.path(root, "Rlibraries"))

#Set parameters
dir.prim <- file.path(root, "RDM Outputs")
 prim.file<-"prim.data_7_06_2015.csv"

#load prim data
prim.data <- data.table(read.csv(file.path(dir.prim, prim.file)))

#compute relative variables to be used in prim as inputs
  prim.data[,Relative.A_N:=Are.N/Ace.N]
  prim.data[,Relative.A_S:=Are.S/Ace.S]
  prim.data[,ProductivityGap:=Relative.A_S/Relative.A_N]
  prim.data[,Relative.Gamma:=Gamma.re/Gamma.ce]
  prim.data[,Relative.Eta:=Eta.re/Eta.ce]
  prim.data[,Relative.Eta.Gamma:=Relative.Eta*Relative.Gamma]
  prim.data[,Relative.Nu:=Nu.re/Nu.ce]
  prim.data[,Relative.k:=exp(k.re)/exp(k.ce)]
  prim.data[,Relative.k.Nu:=(1/Relative.k)*Relative.Nu]
  prim.data[,Share.re_N:=Yre_N/(Yce_N+Yre_N)]
  prim.data[,Share.re_S:=Yre_S/(Yce_S+Yre_S)]
  prim.data[,DiffusionGap:=Share.re_S/Share.re_N]
#changes in consumption
  prim.data[,Policy.Costs.N:=Consumption.Total_N.300-Consumption.Total_N.300.fwa]
  prim.data[,Policy.Costs.S:=Consumption.Total_S.300-Consumption.Total_S.300.fwa]
  prim.data[,Policy.Costs.N.100:=Consumption.Total_N-Consumption.Total_N.fwa]
  prim.data[,Policy.Costs.S.100:=Consumption.Total_S-Consumption.Total_S.fwa]
  prim.data[,Total.Policy.Cost.100:=(Policy.Costs.N.100+Policy.Costs.S.100)/(Consumption.Total_N.fwa+Consumption.Total_S.fwa)]
  prim.data[,Total.Policy.Cost:=Policy.Costs.N+Policy.Costs.S]
  prim.data[,Total.Policy.Utility:=Utility.Consumer.Total_N+Utility.Consumer.Total_S]

#Define outputs of interest
 prim.data[,Temp.Threshold.2C:=ifelse(Delta.Temp<=2,1,0)]
 prim.data[,Temp.Threshold.3C:=ifelse(Delta.Temp<=3,1,0)]
 prim.data[,Stabilization:=ifelse(Policy.Duration+(Policy.Start.Time+2012)>=2123,0,
                                                 ifelse(Delta.Temp>2,0,
 												                         ifelse(Delta.Temp.300yr<=2,1,0)))]
 prim.data[,Cost.N:=ifelse(Policy.Costs.N>=0,1,0)]
 prim.data[,Cost.S:=ifelse(Policy.Costs.S>=0,1,0)]
 prim.data[,Cost.Both:=ifelse(Cost.N+Cost.S>=2,1,0)]

#Define digit significance of policy values
 prim.data[,ce.tax_N:=signif(ce.tax_N, digits = 2)]
 prim.data[,ce.tax_S:=signif(ce.tax_S, digits = 2)]
 prim.data[,RD.subsidy_N:=signif(RD.subsidy_N, digits = 2)]
 prim.data[,RD.subsidy_S:=signif(RD.subsidy_S, digits = 2)]
 prim.data[,RD.subsidy.GF_N:=signif(RD.subsidy.GF_N, digits = 2)]
 prim.data[,Tec.subsidy_N:=signif(Tec.subsidy_N, digits = 2)]
 prim.data[,Tec.subsidy_S:=signif(Tec.subsidy_S, digits = 2)]
 prim.data[,Tec.subsidy.GF_N:=signif(Tec.subsidy.GF_N, digits = 2)]

#summary of performance
 sum.table<-as.data.frame(prim.data[, j=list(Temp.Threshold.2C=mean(Temp.Threshold.2C),
									      # Temp.Threshold.3C=mean(Temp.Threshold.3C),
										   Stabilization=mean(Stabilization)),
                                    by = list( policy.name)])

#Filter data for analysis
dir.inputs <- file.path(root, "RDM Inputs")
 Policies.File<-"Policies.csv"
 Climate.File<-"Climate.csv"
Policies <- read.csv(file.path(dir.inputs, Policies.File))
Climate <- read.csv(file.path(dir.inputs, Climate.File))

#Select prim inputs,
 inputs.vector<-c("Relative.A_N",
                  "Relative.A_S",
				          "ProductivityGap",
				          "epsilon",
				          "rho",
				          "Gamma.re",
				          "Gamma.ce",
				          "Eta.re",
				          "Eta.ce",
				          "Nu.re",
				          "Nu.ce"
				          "RelPrice_N",
				          "RelPrice_S",
				          "Relative.Gamma",
				          "Relative.Eta",
				          "Relative.Eta.Gamma",
				          "Relative.Nu",
				          "Relative.k"
				          "Relative.k.Nu",
				          "Share.re_N",
				          "DiffusionGap",
				          "Share.re_S",
				          "Beta.Delta.Temp"
				          "qsi",
				          "Delta.S"
				          "Climate.Coef.fwa"
                  "CO2.2032",
				          "CO2.2032",
                  "CO2.2042",
	                "CO2.2052",
	                "CO2.2062",
	                "CO2.2072",
	                "CO2.2082",
	                "CO2.2092",
                  "CO2.2062",
				          "Delta.Temp.GrowthRate.fwa"
                  "Delta.Temp.2032",
                  "Delta.Temp.2057"
				          "CO2.Concentration",
				          "CO2.Concentration.2050",
				          "Policy.Start.Time",
				          "Policy.Duration",
				          "ce.tax_N",
				          "RD.subsidy_N",
				          "RD.subsidy.GF_N",
				          "Tec.subsidy_N",
				          "Tec.subsidy.GF_N",
				          "ce.tax_S",
				          "RD.subsidy_S",
				          "Tec.subsidy_S"
				         )


 target.policy<-c(
                 "FWA", # P0. FWA
				 "Nordhaus", # P1. I.Carbon Tax [Both]
				 "Nordhaus+TechnologyPolicy.Both",# P2. I.Carbon Tax+I.Tech-R&D[Both]
				 "Nordhaus+TraditionalGreenClimateFund",# P3. H.Carbon Tax+ Co-Tech[GCF]+R&D[AR]
				 "Nordhaus+TraditionalGreenClimateFund+R&DS", # P4. H.Carbon Tax+Co-Tech[GCF]+ I.R&D[Both]
				 "Nordhaus+CoR&DGreenClimateFund", # 'P5. H.Carbon Tax+Co-R&D[GCF]+Tech[AR]
				 "Nordhaus+CoR&DGreenClimateFund+TecS", # P6. H.Carbon Tax+Co-R&D[GCF]+I.Tech[Both]
				 "Nordhaus+R&DGreenClimateFund" #P7. H.Carbon Tax+Co-Tech-R&D[GCF]
				 )


#select target policy to be analyzed
 target.policy<-target.policy[3]
 target.policy
#subset prim data
 prim<-subset(prim.data,prim.data$policy.name==target.policy)
 prim.inputs<-prim[,inputs.vector,with=FALSE]
 #prim$objective <- prim[,"Temp.Threshold.2C",with=FALSE]
 prim$objective <- prim[,"Stabilization",with=FALSE]
 prim$objective<- prim[,ifelse(objective==1,0,1)]

#analyse correlation of inputs
 cor(prim.inputs)


#run prim
sdprim(prim.inputs,prim$objective, thresh=0.5, threshtype=">",peel_crit = 2,repro = FALSE)  #objective is achive: > 0.5


#calculate statistics of boxes
 total.futures<-sum(prim[,"objective",with=FALSE])
#box 1

 box1<-data.frame(subset(prim,epsilon<9.0))
 #box1<-data.frame(subset(prim,epsilon<4))
 box.1.coverage<-sum(box1$objective)/total.futures
 box.1.density<-mean(box1$objective)
 box.1.coverage
 box.1.density

#box 2
 box2<-data.frame(subset(prim,epsilon<7.6 & Beta.Delta.Temp<4.0))
 box.2.coverage<-sum(box2$objective)/total.futures
 box.2.density<-mean(box2$objective)
 box.2.coverage
 box.2.density

#box 3
 box3<-data.frame(subset(prim,epsilon>9.0 & epsilon<=10 & Beta.Delta.Temp>6.0))
 box.3.coverage<-sum(box3$objective)/total.futures
 box.3.density<-mean(box3$objective)
 box.3.coverage
 box.3.density

#total covered cases
 box.1.coverage+box.2.coverage+box.3.coverage


#using PCA
library(psych, lib = file.path(root, "Rlibraries"))
 fit <- principal(prim.inputs, nfactors=4, rotate="varimax",scores=TRUE)
 PCA.factors<-data.frame(fit$scores)


##analyze taxonomy
 #create policy taxonomy
  policy.chars<-c("Policy.Start.Time","Policy.Duration",
                "ce.tax_N","RD.subsidy_N","RD.subsidy.GF_N",
				 "Tec.subsidy_N","Tec.subsidy.GF_N","ce.tax_S",
				 "RD.subsidy_S","Tec.subsidy_S")
   prim.data[,e1:=ifelse(Policy.Duration<100,1.0,0)]
   prim.data[,e2:=ifelse(ce.tax_N>ce.tax_S,1.0,
                         ifelse(ce.tax_N<ce.tax_S,-1.0,0))]
   prim.data[,e3:=ifelse(Tec.subsidy_N>Tec.subsidy_S,1.0,
                         ifelse(Tec.subsidy_N<Tec.subsidy_S,-1.0,0))]
   #prim.data[,e4:=ifelse(Tec.subsidy.GF_N>Tec.subsidy_S,1.0,
                     ifelse(Tec.subsidy.GF_N<Tec.subsidy_S,-1.0,0))]
   prim.data[,e5:=ifelse(RD.subsidy_N>RD.subsidy_S,1.0,
                         ifelse(RD.subsidy_N<RD.subsidy_S,-1.0,0))]
   #prim.data[,e6:=ifelse(RD.subsidy.GF_N>RD.subsidy_S,1.0,
                         ifelse(RD.subsidy.GF_N<RD.subsidy_S,-1.0,0))]
   prim.data[,e7:=ifelse(Tec.subsidy_N>Tec.subsidy_S+Tec.subsidy.GF_N,1.0,
                         ifelse(Tec.subsidy_N<Tec.subsidy_S+Tec.subsidy.GF_N,-1.0,0))]
   prim.data[,e8:=ifelse(RD.subsidy_N>RD.subsidy_S+RD.subsidy.GF_N,1.0,
                         ifelse(RD.subsidy_N<RD.subsidy_S+RD.subsidy.GF_N,-1.0,0))]
 #Choose subset options
 target.policy<-(
                 #"Nordhaus"
				 #"Nordhauds+TechnologyPolicy"
				 #"Nordhaus+TraditionalGreenClimateFund"
				 "Nordhaus+R&DGreenClimateFund"
				 )

#P1.Nordhaus
 #subset to policy
   prim<-subset(prim.data,prim.data$policy.name=="Nordhaus")
 #subset to target
   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
   #unique(prim[,c("e1","e2"),with=FALSE])
 #frequency table
  prim[,freq:=1]
  prim[,taxonomy:=paste(e2,sep=",")]
  P1.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
                                    by = list(taxonomy,e2)])
  P1.Taxonomy[order(-P1.Taxonomy$freq),]
  prim<-data.frame(prim)
  prim.P1<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")]
  prim.P1$policy.name<-"P1"



#PX.Nordhauds+TechnologyPolicy
 #subset to policy
#   prim<-subset(prim.data,prim.data$policy.name=="Nordhauds+TechnologyPolicy")
 #subset to target
#   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
   #unique(prim[,c("e2"),with=FALSE])
 #frequency table
#  prim[,freq:=1]
#  prim[,taxonomy:=paste(e2,sep=",")]
#  P2.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
#                                    by = list(taxonomy,e2)])
#  P2.Taxonomy[order(-P2.Taxonomy$freq),]

#see portfolios
#  prim<-data.frame(prim)
#  prim.P2<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")]
#  prim.P2$policy.name<-"PX"


#P2.Nordhauds+TechnologyPolicy.Both
 #subset to policy
   prim<-subset(prim.data,prim.data$policy.name=="Nordhaus+TechnologyPolicy.Both")
 #subset to target
   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
 #frequency table
  prim[,freq:=1]
  prim[,taxonomy:=paste(e2,e3,e5,sep=",")]
  P2.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
                                    by = list(taxonomy,e2,e3,e5)])
  P2.Taxonomy[order(-P2.Taxonomy$freq),]

#see portfolios
  prim<-data.frame(prim)
  prim.P2<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")]
  prim.P2$policy.name<-"P2"



 #P3.Nordhaus+TraditionalGreenClimateFund
 #subset to policy
   prim<-subset(prim.data,prim.data$policy.name=="Nordhaus+TraditionalGreenClimateFund")
 #subset to target
   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
   #unique(prim[,c("e1","e2","e3","e4"),with=FALSE])
 #frequency table
  prim[,freq:=1]
  prim[,taxonomy:=paste(e2,e3,e7,sep=",")]
  P3.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
                                    by = list(taxonomy,e2,e3,e7)])
  P3.Taxonomy[order(-P3.Taxonomy$freq),]

  prim<-data.frame(prim)
  prim.P3<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")]
  prim.P3$policy.name<-"P3"


 #P4.Nordhaus+TraditionalGreenClimateFund+R&DS
 #subset to policy
   prim<-subset(prim.data,prim.data$policy.name=="Nordhaus+TraditionalGreenClimateFund+R&DS")
 #subset to target
   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
 #frequency table
  prim[,freq:=1]
  prim[,taxonomy:=paste(e2,e3,e5,e7,sep=",")]
  P4.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
                                    by = list(taxonomy,e2,e3,e5,e7)])
  P4.Taxonomy[order(-P4.Taxonomy$freq),]

  prim<-data.frame(prim)
  prim.P4<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")]
  prim.P4$policy.name<-"P4"


#analyze the structure of portfolios
  target.portfolios<-c("1,0,1,0,1,-1")
  subset(prim[,policy.chars],prim$taxonomy%in%target.portfolios)


#P5.Nordhaus+CoR&DGreenClimateFund
 #subset to policy
   prim<-subset(prim.data,prim.data$policy.name=="Nordhaus+CoR&DGreenClimateFund")
 #subset to target
   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
   #unique(prim[,c("e1","e2","e3","e4"),with=FALSE])
 #frequency table
  prim[,freq:=1]
  prim[,taxonomy:=paste(e2,e3,e5,e8,sep=",")]
  P5.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
                                    by = list(taxonomy,e2,e3,e5,e8)])
  P5.Taxonomy[order(-P5.Taxonomy$freq),]

  prim<-data.frame(prim)
  prim.P5<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")]
  prim.P5$policy.name<-"P5"


#analyze the structure of portfolios
  target.portfolios<-c("1,0,1,0,1,-1")
  subset(prim[,policy.chars],prim$taxonomy%in%target.portfolios)


#P6.Nordhaus+CoR&DGreenClimateFund+TecS
 #subset to policy
   prim<-subset(prim.data,prim.data$policy.name=="Nordhaus+CoR&DGreenClimateFund+TecS")
 #subset to target
   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
   #unique(prim[,c("e1","e2","e3","e4"),with=FALSE])
 #frequency table
  prim[,freq:=1]
  prim[,taxonomy:=paste(e2,e3,e5,e8,sep=",")]
  P6.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
                                    by = list(taxonomy,e2,e3,e5,e8)])
  P6.Taxonomy[order(-P6.Taxonomy$freq),]

   prim<-data.frame(prim)
  prim.P6<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")]
  prim.P6$policy.name<-"P6"


#analyze the structure of portfolios
  target.portfolios<-c("1,0,1,0,1,-1")
  subset(prim[,policy.chars],prim$taxonomy%in%target.portfolios)


#P7.Nordhaus+R&DGreenClimateFund
 #subset to policy
   prim<-subset(prim.data,prim.data$policy.name=="Nordhaus+R&DGreenClimateFund")
 #subset to target
   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
   #unique(prim[,c("e1","e2","e3","e4","e5","e6"),with=FALSE])
 #frequency table
  prim[,freq:=1]
  prim[,taxonomy:=paste(e2,e3,e5,e7,e8,sep=",")]
  P7.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
                                    by = list(e2,e3,e5,e7,e8)])
  P7.Taxonomy[order(-P7.Taxonomy$freq),]

  prim<-data.frame(prim)
  prim.P7<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")]
  prim.P7$policy.name<-"P7"

#understanding change in futures

  subset(prim.P8$Future.ID,!(prim.P8$Future.ID%in%prim.P3$Future.ID))

#with all these policies how many futures are not vulnerable
  all.futures<-unique(c(
                        prim.P1$Future.ID,
                        prim.P2$Future.ID,
#                        prim.P3$Future.ID,
                        prim.P4$Future.ID,
#                        prim.P5$Future.ID,
                        prim.P6$Future.ID,
                        prim.P7$Future.ID))

#build a table with the cost of each policy
  #prim.all<-rbind(prim.P1,prim.P2,prim.P3,prim.P4,prim.P5,prim.P6,prim.P7,prim.P8)
  #prim.all<-rbind(prim.P1,prim.P2,prim.P3,prim.P4,prim.P5,prim.P6,prim.P7)
  prim.all<-rbind(prim.P1,prim.P2,prim.P4,prim.P6,prim.P7)
  prim.all$Policy.Costs.S<-round(prim.all$Policy.Costs.S,digits=0)
  prim.all$Policy.Costs.N<-round(prim.all$Policy.Costs.N,digits=0)
  prim.all$Total.Policy.Cost<-round(prim.all$Total.Policy.Cost,digits=0)


######################################################################################################################################################
#find dominated policies
#################################################################################################################################################
  futures.key<-apply(data.frame(all.futures=all.futures),1,function(x) { policy.table.future<-subset(prim.all[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")],prim.all$Future.ID==x);
                                      pivot<-subset(policy.table.future[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")],policy.table.future$Total.Policy.Cost==max(policy.table.future$Total.Policy.Cost));
                                     subset(pivot[,c("Future.ID","policy.name")],pivot$Total.Policy.Cost==max(pivot$Total.Policy.Cost))
                                               })
  futures.key<-do.call("rbind",futures.key)
  futures.key$Freq<-1
  futures.policy.table<-aggregate(futures.key$Freq,list(policy.name=futures.key$policy.name),sum)
  colnames(futures.policy.table)<-c("policy.name","Freq")
  futures.policy.table[order(-futures.policy.table$Freq),]

#do prim in social planners option
#prim analysis to understand under which conditions each portfolio is implemented
#merge with inputs
  futures.key<-merge(futures.key,data.frame(unique(prim.data[,c("Future.ID",inputs.vector),with=FALSE])),by="Future.ID")
#flag
  futures.key$flag<-ifelse(futures.key$policy.name=="P5",1,0)
#let's check discount rates
  rhos<-unique(futures.key$rho)
  prim.pivot<-subset(futures.key,futures.key$rho==rhos[1])
#sdprim
 sdprim(prim.pivot[,inputs.vector],prim.pivot[,"flag"], thresh=0.5, threshtype=">",peel_crit = 2,repro = FALSE)

######################################################################################################################################################

######################################################################################################################################################
#now analyse for both independently
######################################################################################################################################################

#emerging region
 futures.key.S<-apply(data.frame(all.futures=all.futures),1,function(x) {
                                      pivot<-subset(prim.all[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")],prim.all$Future.ID==x);
									  pivot<-subset(pivot[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")],pivot$Policy.Costs.S==max(pivot$Policy.Costs.S));
									  subset(pivot[,c("Future.ID","policy.name")],pivot$Total.Policy.Cost==max(pivot$Total.Policy.Cost))
                                                })
  futures.key.S<-do.call("rbind",futures.key.S)
  colnames(futures.key.S)<-c("Future.ID","policy.name.S")
#advanced region
 futures.key.N<-apply(data.frame(all.futures=all.futures),1,function(x) {
                                     pivot<-subset(prim.all[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")],prim.all$Future.ID==x);
									 pivot<-subset(pivot[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")],pivot$Policy.Costs.N==max(pivot$Policy.Costs.N));
									 subset(pivot[,c("Future.ID","policy.name")],pivot$Total.Policy.Cost==max(pivot$Total.Policy.Cost))
                                               })
  futures.key.N<-do.call("rbind",futures.key.N)
  colnames(futures.key.N)<-c("Future.ID","policy.name.N")

#merge both
  futures.key.NS<-merge(futures.key.S,futures.key.N,by="Future.ID")
  futures.key.NS$Cop<-ifelse(futures.key.NS$policy.name.N==futures.key.NS$policy.name.S,1,0)
#cooperation
  futures.policy.table.Cop<-subset(futures.key.NS,futures.key.NS$Cop==1)
  Cop.agg.table<-aggregate(futures.policy.table.Cop$Cop,list(policy.name=futures.policy.table.Cop$policy.name.S),sum)
  colnames( Cop.agg.table)<-c("policy.name","Freq")
  Cop.agg.table[order(-Cop.agg.table$Freq),]
 #non-cooperation
  non.cop.future.ids<-subset(futures.key.NS,futures.key.NS$Cop==0)

#do prim on non-cooperation
#prim analysis to understand under which conditions each portfolio is implemented
#merge with inputs
  prim.analysis<-merge(futures.key.NS,data.frame(unique(prim.data[,c("Future.ID",inputs.vector),with=FALSE])),by="Future.ID")
#let's check discount rates
  #rhos<-unique(futures.key$rho)
  #prim.pivot<-subset(futures.key,futures.key$rho==rhos[1])
#sdprim
  sdprim(prim.analysis[,inputs.vector],prim.analysis[,"Cop"], thresh=0.5, threshtype="<",peel_crit = 1,repro = FALSE)

#merge all the policy tables into prim
#change format to least cost.policy
 futures.key$Freq<-NULL
 colnames(futures.key)<-c("Future.ID","least.cost.policy")
#
 colnames(futures.key.NS)<-c("Future.ID","policy.name.S","policy.name.N","Cop")

 #merge both tables
 futures.policies.table<-merge(futures.key,futures.key.NS,by="Future.ID")
 #futures.policies.table$Check<-ifelse(futures.policies.table$least.cost.policy==futures.policies.table$policy.name.S,1,0)
#read prim data
 #create robust mapping of alternatives
  policy.names<-data.frame(least.cost.policy=c("P1","P2","P3","P4","P5","P6","P7"),
                           policy.name=c("Nordhaus","Nordhaus+TechnologyPolicy.Both","Nordhaus+TraditionalGreenClimateFund","Nordhaus+TraditionalGreenClimateFund+R&DS","Nordhaus+CoR&DGreenClimateFund","Nordhaus+CoR&DGreenClimateFund+TecS","Nordhaus+R&DGreenClimateFund"))

  policy.chars<-c("Policy.Start.Time","Policy.Duration",
                "ce.tax_N","RD.subsidy_N","RD.subsidy.GF_N",
				 "Tec.subsidy_N","Tec.subsidy.GF_N","ce.tax_S",
				 "RD.subsidy_S","Tec.subsidy_S")
  uncertainties<-c("epsilon","rho","Climate.Model","Beta.Delta.Temp")

#create robust mapping table
  all.futures.mapping<-futures.policies.table[,c("Future.ID","least.cost.policy")]
#subset to only selected policies
  all.policies<-subset(policy.names,policy.names$least.cost.policy%in%unique(futures.policies.table$least.cost.policy))
  all.policies$policy.name<-as.character(all.policies$policy.name)
  prim.data$policy.name<-as.character(prim.data$policy.name)
  policy.vector<-data.frame(prim.data[,c("Future.ID","policy.name",policy.chars,uncertainties),with=FALSE])
#map policy response to
  robust.mapping<-subset(all.futures.mapping,all.futures.mapping$least.cost.policy==all.policies$least.cost.policy[1])
  policy.pivot<-subset(policy.vector,policy.vector$policy.name==all.policies$policy.name[1])
  robust.mapping<-merge(robust.mapping,policy.pivot,by="Future.ID")
  for (i in 2:nrow(all.policies))
  {
   robust.pivot<-subset(all.futures.mapping,all.futures.mapping$least.cost.policy==all.policies$least.cost.policy[i])
   policy.pivot<-subset(policy.vector,policy.vector$policy.name==all.policies$policy.name[i])
   robust.pivot<-merge(robust.pivot,policy.pivot,by="Future.ID")
   robust.mapping<-rbind(robust.mapping,robust.pivot)
  }
  robust.mapping.stabilization<-robust.mapping


###################################################################################################################################################################################
#process the temperature rise target
###################################################################################################################################################################################
objective<-"Temp.Threshold.2C"
policy.names<-data.frame(least.cost.policy=c("P1","P2","P3","P4","P5","P6","P7"),
                           policy.name=c("Nordhaus","Nordhaus+TechnologyPolicy.Both","Nordhaus+TraditionalGreenClimateFund","Nordhaus+TraditionalGreenClimateFund+R&DS","Nordhaus+CoR&DGreenClimateFund","Nordhaus+CoR&DGreenClimateFund+TecS","Nordhaus+R&DGreenClimateFund"))
policy.chars<-c("Policy.Start.Time","Policy.Duration",
                "ce.tax_N","RD.subsidy_N","RD.subsidy.GF_N",
				 "Tec.subsidy_N","Tec.subsidy.GF_N","ce.tax_S",
				 "RD.subsidy_S","Tec.subsidy_S")
uncertainties<-c("epsilon","rho","Climate.Model","Beta.Delta.Temp")


#P1.Nordhaus
 #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P1.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")]
   prim.P1.t$policy.name<-"P1"

#P2.Nordhauds+TechnologyPolicy.Both
 #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+TechnologyPolicy.Both"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P2.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")]
   prim.P2.t$policy.name<-"P2"


#P3.Nordhaus+TraditionalGreenClimateFund
  #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+TraditionalGreenClimateFund"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P3.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")]
   prim.P3.t$policy.name<-"P3"

#P4.Nordhaus+TraditionalGreenClimateFund+R&DS
   #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+TraditionalGreenClimateFund+R&DS"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P4.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")]
   prim.P4.t$policy.name<-"P4"

#P5.Nordhaus+CoR&DGreenClimateFund
  #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+CoR&DGreenClimateFund"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P5.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")]
   prim.P5.t$policy.name<-"P5"

#P6.Nordhaus+CoR&DGreenClimateFund+TecS
 #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+CoR&DGreenClimateFund+TecS"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P6.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")]
   prim.P6.t$policy.name<-"P6"

 #P7.Nordhaus+R&DGreenClimateFund
  #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+R&DGreenClimateFund"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P7.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")]
   prim.P7.t$policy.name<-"P7"


#with all these policies how many futures are not vulnerable
  all.futures<-unique(c(
                        prim.P1.t$Future.ID,
                        prim.P2.t$Future.ID,
#                        prim.P3.t$Future.ID,
                        prim.P4.t$Future.ID,
#                        prim.P5.t$Future.ID,
                        prim.P6.t$Future.ID,
                        prim.P7.t$Future.ID))

#build a table with the cost of each policy
#  prim.all<-rbind(prim.P1.t,prim.P2.t,prim.P3.t,prim.P4.t,prim.P5.t,prim.P6.t,prim.P7.t)
   prim.all<-rbind(prim.P1.t,prim.P2.t,prim.P4.t,prim.P6.t,prim.P7.t)
  prim.all$Policy.Costs.S<-round(prim.all$Policy.Costs.S,digits=0)
  prim.all$Policy.Costs.N<-round(prim.all$Policy.Costs.N,digits=0)
  prim.all$Total.Policy.Cost<-round(prim.all$Total.Policy.Cost,digits=0)

 #find dominated policies
  futures.key<-apply(data.frame(all.futures=all.futures),1,function(x) { policy.table.future<-subset(prim.all[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")],prim.all$Future.ID==x);
                                      pivot<-subset(policy.table.future[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")],policy.table.future$Total.Policy.Cost==max(policy.table.future$Total.Policy.Cost));
                                     subset(pivot[,c("Future.ID","policy.name")],pivot$Delta.Temp==min(pivot$Delta.Temp))
                                               })
  futures.key<-do.call("rbind",futures.key)
  futures.key$Freq<-1
  futures.policy.table<-aggregate(futures.key$Freq,list(policy.name=futures.key$policy.name),sum)
  colnames(futures.policy.table)<-c("policy.name","Freq")
  futures.policy.table[order(-futures.policy.table$Freq),]

#reshape
  futures.key$Freq<-NULL
  colnames(futures.key)<-c("Future.ID","least.cost.policy")
  futures.policies.table<- futures.key

#create robust mapping table
  all.futures.mapping<-futures.policies.table[,c("Future.ID","least.cost.policy")]
#subset to only selected policies
  all.policies<-subset(policy.names,policy.names$least.cost.policy%in%unique(futures.policies.table$least.cost.policy))
  all.policies$policy.name<-as.character(all.policies$policy.name)
  prim.data$policy.name<-as.character(prim.data$policy.name)
  policy.vector<-data.frame(prim.data[,c("Future.ID","policy.name",policy.chars,uncertainties),with=FALSE])
#map policy response to
  robust.mapping<-subset(all.futures.mapping,all.futures.mapping$least.cost.policy==all.policies$least.cost.policy[1])
  policy.pivot<-subset(policy.vector,policy.vector$policy.name==all.policies$policy.name[1])
  robust.mapping<-merge(robust.mapping,policy.pivot,by="Future.ID")
  for (i in 2:nrow(all.policies))
  {
   robust.pivot<-subset(all.futures.mapping,all.futures.mapping$least.cost.policy==all.policies$least.cost.policy[i])
   policy.pivot<-subset(policy.vector,policy.vector$policy.name==all.policies$policy.name[i])
   robust.pivot<-merge(robust.pivot,policy.pivot,by="Future.ID")
   robust.mapping<-rbind(robust.mapping,robust.pivot)
  }
  robust.mapping.temperature.rise<-robust.mapping

#merge both robust mappings
  robust.mapping.stabilization$objective<-"Stabilization"
  robust.mapping.temperature.rise$objective<-"Temperature.Rise"
#subset temperature rise
  robust.mapping.temperature.rise<-subset(robust.mapping.temperature.rise,!(robust.mapping.temperature.rise$Future.ID%in%robust.mapping.stabilization$Future.ID))
#add futures with no solution
  bad.futures<-subset(policy.vector,policy.vector$policy.name=="FWA")
  bad.futures<-subset(bad.futures,!(bad.futures$Future.ID%in%unique(c(robust.mapping.stabilization$Future.ID,robust.mapping.temperature.rise$Future.ID))))
  bad.futures$least.cost.policy<-"Expand"
  bad.futures$RD.subsidy_N<-2
  bad.futures$RD.subsidy.GF_N<-2
  bad.futures$Tec.subsidy_N<-0.5
  bad.futures$Tec.subsidy.GF_N<-0.5
  bad.futures$ce.tax_S<-1
  bad.futures$ce.tax_N<-1
  bad.futures$RD.subsidy_S<-2
  bad.futures$Tec.subsidy_S<-0.5
  bad.futures$Policy.Start.Time<-9.9
  bad.futures$Policy.Duration<-300
  bad.futures$objective<-"Failure"
#rbind all futures together
  dim(robust.mapping.stabilization)
  dim(robust.mapping.temperature.rise)
  dim(bad.futures)
  robust.mapping<-rbind(robust.mapping.stabilization,robust.mapping.temperature.rise,bad.futures)
  dim(robust.mapping)
#write final file
  dir.prim <- file.path(root, "RDM Outputs")
  write.csv(robust.mapping, file.path(dir.prim, "robust_mapping.csv"), row.names = FALSE)
