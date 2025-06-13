#calibrate climate scenarios
#read the data
## Use project-relative path for climate calibration data
dir.climate.data <- file.path(getwd(), "ClimateDataCalibration")
climate.data <- read.csv(file.path(dir.climate.data, "AllGCMs.csv"))

#PART1: CALIBRATION OF DELTA TEMP VS LOG CO2 FUNCTION
climate.models<-unique(climate.data$Climate.Model)
delta.temp.disater<-7.5 #[degrees Celsius] increase in average global temperature

  model.data<-as.data.frame(subset(climate.data,climate.data$Climate.Model==climate.models[1]))
  model<-lm(tas.anomaly~log(co2.ppm),data =model.data )
  my.coeff.intercept<-as.numeric(model$coefficients[1])
  my.coeff.beta<-as.numeric(model$coefficients[2])
  my.RSquared <- summary(model)$adj.r.squared
  my.p.value.intercept<-summary(model)$coefficients[1,4]
  my.p.value.beta<-summary(model)$coefficients[2,4]
  result<-data.frame(Climate.Model=climate.models[1], Beta.Delta.Temp=my.coeff.beta, Intercept=my.coeff.intercept, AdjR.squared=my.RSquared, Intercept.P.value= my.p.value.intercept ,Beta.P.value=my.p.value.beta)
  for (j in 2:length(climate.models))
  {
    model.data<-as.data.frame(subset(climate.data,climate.data$Climate.Model==climate.models[j]))
    model<-lm(tas.anomaly~log(co2.ppm),data =model.data )
    my.coeff.intercept<-as.numeric(model$coefficients[1])
    my.coeff.beta<-as.numeric(model$coefficients[2])
    my.RSquared <- summary(model)$adj.r.squared
    my.p.value.intercept<-summary(model)$coefficients[1,4]
    my.p.value.beta<-summary(model)$coefficients[2,4]
    pivot<-data.frame(Climate.Model=climate.models[j], Beta.Delta.Temp=my.coeff.beta, Intercept=my.coeff.intercept, AdjR.squared=my.RSquared, Intercept.P.value= my.p.value.intercept ,Beta.P.value=my.p.value.beta)
	result<-rbind(result,pivot)
  }
#estimate CO2 concentrations for delta temp disaster
   result$CO2.base<-exp(-1*result$Intercept/result$Beta.Delta.Temp)
   result$CO2.Disaster<-result$CO2.base*exp(delta.temp.disater/result$Beta.Delta.Temp)
   result$Delta.Temp.Disaster<-delta.temp.disater
#print climate model parameters
write.csv(result, file.path(dir.climate.data, "Climate_ScenariosTable1.csv"), row.names = FALSE)


#PART2: CALIBRATION OF S EQUATION
#read the data
## Reset path to the climate calibration data using the project directory
dir.climate.data <- file.path(getwd(), "ClimateDataCalibration")
climate.data <- read.csv(file.path(dir.climate.data, "AllGCMs.csv"))
#The historical record across RCPs is the same, thus we aggregate the models at RCP level
 climate.data<-aggregate(climate.data[,"co2.ppm"],list(Climate.Model=climate.data$Climate.Model,Year=climate.data$Year),mean)
 colnames(climate.data)[ncol(climate.data)]<-"co2.ppm"
#load initial scenario parameters
climate.param.ini <- read.csv(file.path(dir.climate.data, "Climate_ScenariosTable1.csv"))
 climate.param.ini<-climate.param.ini[,c("Climate.Model","CO2.Disaster")]
#merge CO2 Disaster with raw data
  dim(climate.data)
  climate.data<-merge(climate.data,climate.param.ini,by="Climate.Model")
  dim(climate.data)
#calculate quality of the environment
  climate.data$S<-climate.data$CO2.Disaster-climate.data$co2.ppm
  climate.data$S<-ifelse(climate.data$S<0,0,climate.data$S)
#order data set
  climate.data<-climate.data[order(climate.data$Climate.Model,climate.data$Year),]
  #list available models
  climate.models<-unique(climate.data$Climate.Model)
  length(climate.models)
#load data of world consumption of fossil fuels
fossil.fuel <- read.csv(file.path(dir.climate.data, "FossilFuelConsumption.csv"))
  fossil.fuel<-fossil.fuel[,c("Year","Fossil.Fuels.Consumption")] # Quadrillion Btu

#loop across models
#start with first climate
  model.data<-subset(climate.data,climate.data$Climate.Model==climate.models[1])
#we make the same assumption as Acemoglous et. al, such that Delta.S=0.5(qsi*Y(t)/S(t-1));
#thus the original model: S(t)=-qsi*Y(t)+(1+Delta.S)*S(t-1) is reduced to: S(t)-S(t-1)=-0.5*qsi*Y(t) and can be estimated with lm
  model.data$S.diff<-c(NA,diff(model.data$S,differences=1))
#merge with fossil fuels
  model.data<-merge(model.data,fossil.fuel,by="Year") # here we only consider the data that matches out Y data record
#estimate model
#estimate qsi
  #plot(model.data$Fossil.Fuels.Consumption,model.data$S.diff)
  model1<-lm(S.diff~Fossil.Fuels.Consumption-1,data=model.data)
  qsi<-as.numeric(model1$coefficients)/-0.5
#estimate Delta.S
  model.data$S_lag<-model.data$S-model.data$S.diff
  #plot(model.data$Fossil.Fuels.Consumption,model.data$S_lag)
  model2<-lm(S_lag~Fossil.Fuels.Consumption-1,data=model.data)
  Delta.S<-0.5*qsi/as.numeric(model2$coefficients)
#calculate S_hat
  model.data$S_hat<--1*qsi*model.data$Fossil.Fuels.Consumption+(1+Delta.S)*model.data$S_lag
  #ts.plot(as.ts(model.data$S),as.ts(model.data$S_hat),gpars = list(col = c("black", "red")))
  s.parameters<-data.frame(Climate.Model=climate.models[1],qsi=qsi,Delta.S=Delta.S,S.0=model.data$S_hat[model.data$Year==2012])
for (j in 2:length(climate.models))
 {
  model.data<-subset(climate.data,climate.data$Climate.Model==climate.models[j])
  model.data$S.diff<-c(NA,diff(model.data$S,differences=1))
  model.data<-merge(model.data,fossil.fuel,by="Year") # here we only consider the data that matches out Y data record
  model1<-lm(S.diff~Fossil.Fuels.Consumption-1,data=model.data)
  qsi<-as.numeric(model1$coefficients)/-0.5
  model.data$S_lag<-model.data$S-model.data$S.diff
  model2<-lm(S_lag~Fossil.Fuels.Consumption-1,data=model.data)
  Delta.S<-0.5*qsi/as.numeric(model2$coefficients)
  model.data$S_hat<--1*qsi*model.data$Fossil.Fuels.Consumption+(1+Delta.S)*model.data$S_lag
  #ts.plot(as.ts(model.data$S),as.ts(model.data$S_hat),gpars = list(col = c("black", "red")))
  parameters.dummy<-data.frame(Climate.Model=climate.models[j],qsi=qsi,Delta.S=Delta.S,S.0=model.data$S_hat[model.data$Year==2012])
  s.parameters<-rbind(s.parameters,parameters.dummy)
  }

#Create a table with parameters for all climate models
climate.param.ini <- read.csv(file.path(dir.climate.data, "Climate_ScenariosTable1.csv"))
 climate.param.ini<-climate.param.ini[,c("Climate.Model","Beta.Delta.Temp","CO2.base","CO2.Disaster","Delta.Temp.Disaster")]
#merge
 climate.param<-merge(climate.param.ini,s.parameters,by="Climate.Model")
write.csv(climate.param, file.path(dir.climate.data, "Climate.csv"), row.names = FALSE)
