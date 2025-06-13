#define vector for output
## Set project root dynamically based on the current working directory
root <- file.path(getwd(), "")

#this script has been created to find the optimal value of policies for a future id,
  library(deSolve, lib = file.path(root, "Rlibraries"))
  library(optimx, lib = file.path(root, "Rlibraries"))
  dir.harness <- file.path(root, "RDM Harness")
#Source Experimental Design
  dir.exp <- file.path(root, "RDM Inputs")
  experiment.version<-"Exp.design_calib.csv"
  Exp.design <- read.csv(file.path(dir.exp, experiment.version))
#run the model once

#Source Model
  dir.model <- file.path(root, "TechChange Model")
  model.version<-"InternationalGreenTechChangeModel_9_19_2015_calib.r"
  source(file.path(dir.model, model.version))

target.run<-1
params<-c(
                         S.0=as.numeric(Exp.design[target.run,'S.0']),
                         TimeStep=as.numeric(Exp.design[target.run,'TimeStep']),
                         EndTime=as.numeric(Exp.design[target.run,'EndTime']),
                         alfa=as.numeric(Exp.design[target.run,'alfa']),
                         epsilon=as.numeric(Exp.design[target.run,'epsilon']),
                         Gamma.re=as.numeric(Exp.design[target.run,'Gamma.re']),
                         k.re=as.numeric(Exp.design[target.run,'k.re']),
                         Gamma.ce=as.numeric(Exp.design[target.run,'Gamma.ce']),
                         k.ce=as.numeric(Exp.design[target.run,'k.ce']),
                         Eta.re=as.numeric(Exp.design[target.run,'Eta.re']),
                         Eta.ce=as.numeric(Exp.design[target.run,'Eta.ce']),
                         Nu.re=as.numeric(Exp.design[target.run,'Nu.re']),
                         Nu.ce=as.numeric(Exp.design[target.run,'Nu.ce']),
                         qsi=as.numeric(Exp.design[target.run,'qsi']),
                         Delta.S=as.numeric(Exp.design[target.run,'Delta.S']),
						 Delta.Temp.Disaster=as.numeric(Exp.design[target.run,'Delta.Temp.Disaster']),
						 Beta.Delta.Temp=as.numeric(Exp.design[target.run,'Beta.Delta.Temp']),
						 CO2.base=as.numeric(Exp.design[target.run,'CO2.base']),
						 CO2.Disaster=as.numeric(Exp.design[target.run,'CO2.Disaster']),
                         labor.growth_N=as.numeric(Exp.design[target.run,'labor.growth_N']),
						 labor.growth_S=as.numeric(Exp.design[target.run,'labor.growth_S']),
                         lambda.S=as.numeric(Exp.design[target.run,'lambda.S']),
						 sigma.utility=as.numeric(Exp.design[target.run,'sigma.utility']),
						 rho=as.numeric(Exp.design[target.run,'rho']),
                         Yre.0_N=as.numeric(Exp.design[target.run,'Yre.0_N']),
                         Yce.0_N=as.numeric(Exp.design[target.run,'Yce.0_N']),
                         Yre.0_S=as.numeric(Exp.design[target.run,'Yre.0_S']),
                         Yce.0_S=as.numeric(Exp.design[target.run,'Yce.0_S']),
						 size.factor=as.numeric(Exp.design[target.run,'size.factor']),
						 Run.ID= as.numeric(Exp.design[target.run,'Run.ID']),
						 policy.name = as.character(Exp.design[target.run,'policy.name']),
						 dir.harness=dir.harness)

TechChangeMod(c(0.03,1.0,0.02,0.01,0.05,1.0,0.02,0.5),params)

## =====================================================================================================
## This section reads the output of simulations and reshapes it into time series split by region,
## =====================================================================================================
#Define directory parameters
 dir.inputs <- file.path(root, "RDM Inputs")
 dir.harness <- file.path(root, "RDM Harness")
 dir.output <- file.path(root, "RDM Outputs")

#create vector with file names
 experiment.version<-"Exp.design_calib.csv"
 filenames <- list.files(dir.harness, pattern = "*.csv", full.names = FALSE)
#source function to process harnessed output data
 source(file.path(dir.inputs, "harness_processing.r"))
#run post-processing in parallel
  library(data.table, lib = file.path(root, "Rlibraries"))
  library(snow, lib = file.path(root, "Rlibraries"))
  modelruns<-process.harness.data(filenames[1],dir.inputs,experiment.version,dir.harness)
#print time series for model
  write.csv(modelruns, file.path(root, "ParameterCalibration", "model.runs_calib.csv"), row.names = FALSE)


## =====================================================================================================
## This section merges the historical table with the calibration run
## =====================================================================================================

#this script puts together the data for the historic calibration
  dir.output <- file.path(root, "RDM Outputs")
  dir.historic <- file.path(root, "ParameterCalibration")

#read historic data
  historic <- read.csv(file.path(dir.historic, "historic_energy_both_regions _v1.csv"))
#read simulated data
  data <- read.csv(file.path(dir.historic, "model.runs_calib.csv"))
  data<-data[,c("Run.ID","time","Region","Y","Yce","Yre","Ace","Are","L","policy.name",
                "epsilon","rho","alfa","Eta.re","Eta.ce","Gamma.re","Gamma.ce","Nu.re","Nu.ce",
   				"k.re","k.ce","labor.growth","size.factor")]
  data$time<-data$time-29
 data<-Reduce(function(...) { merge(..., ) }, list(data,historic))
 #subset and create OECD and NONOECD regions
 data.historic<-subset(data,data$Run.ID==1)
 data.historic$Region<-gsub("N","OECD",data.historic$Region)
 data.historic$Region<-gsub("S","NONOECD",data.historic$Region)
 data.historic$Run.ID<-data.historic$Run.ID+1
 data<-rbind(data,data.historic)
   write.csv(data, file.path(dir.output, "historic_calib.csv"), row.names = FALSE)
