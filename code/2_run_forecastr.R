# Taku River Sockeye Salmon Preseason Forecasting
# author: Sara E. Miller 
# contact: sara.miller@alaska.gov; 907-465-4245
# Last edited: January 2025
# This code runs the preseason forecast for Taku River sockeye salmon

# load libraries----
library(devtools) # Load the devtools package.
install_github("SalmonForecastR/ForecastR-Package")
library(forecastR)
library(meboot)
library(moments)
# font_import() # uncomment and run once, then comment out
#loadfonts(device = "win")
#windowsFonts(Times=windowsFont("TT Times New Roman"))

# set graphics----
#theme_set(theme_report(base_size = 14))

# set inputs for the current season
year <- 2025 # forecast year
year.subfolder <- "2026_preseason" # subfolder for forecast
initial_year <- 1984

data.directory <- file.path('forecasts','preseason', 'data', year.subfolder)
if(!dir.exists(file.path("forecasts/preseason/output",year.subfolder))){dir.create(file.path("forecasts/preseason/output",year.subfolder))}
results.directory <- file.path('forecasts','preseason', 'output', year.subfolder)

# load data----
read.csv(file.path(data.directory,'data_file_WithAge.csv')) -> data.withage.raw 
data.withage <- prepData(data.withage.raw,out.labels="v2")
# ------------------------------------------------
# fit a few different models and calculate forecasts
# Uses: fitModel(),calcFC(), plotModelFit()

# fit ARIMA model to the data set with age classes (no BoxCox transformation)
arimafit.withage.nobc <- fitModel(model= "TimeSeriesArima", data = brood, settings = list(BoxCox=FALSE),tracing=FALSE)

#check the components of the model fit
names(arimafit.withage.nobc)
names(arimafit.withage.nobc$"Age 4")

# extract the residuals (obs-fitted)
arimafit.withage.nobc$"Age 4"$residuals


# calculate the forecast based on that model fit
arimafc.withage.nobc <- calcFC(fit.obj= arimafit.withage.nobc ,data =data.withage$data, fc.yr= data.withage$specs$forecastingyear,  settings = list(BoxCox=FALSE), tracing=TRUE)	

# check the components of the fc output
names(arimafc.withage.nobc)
arimafc.withage.nobc$pt.fc


plotModelFit(arimafit.withage.nobc, options= list(plot.which = "fitted_ts",age.which="all",plot.add=FALSE),fc.add = arimafc.withage.nobc)
title(sub="ARIMA - No Box Cox")


# Fit the SIBREGSIMPLE model to the same data set with age classes 
sibregfit.withage <- fitModel(model= "SibRegSimple", data = brood, settings = NULL ,tracing=FALSE)
sibregfc.withage <- calcFC(fit.obj= sibregfit.withage ,data =data.withage$data, fc.yr= data.withage$specs$forecastingyear,  settings = NULL, tracing=TRUE)	
plotModelFit(sibregfit.withage, options= list(plot.which = "fitted_ts",age.which="all",plot.add=FALSE),fc.add = sibregfc.withage)
title(sub="Simple Sib Reg")



# -----------------------------------
# do a retrospective test for a model


sibreg.retro <- doRetro(model= "SibRegSimple", data = data.withage$data, 
                        retro.settings = list(min.yrs=15), 
                        fit.settings = NULL, 
                        fc.settings = list(boot=NULL),
                        tracing=FALSE,out.type="short")

# check the components of the retrospective output
names(sibreg.retro)

# extract the residuals from the retrospective
sibreg.retro$retro.resids


# extract one of the versions of the performance summary
sibreg.retro$retro.pm.bal


arima.retro <- doRetro(model= "TimeSeriesArima", data = data.withage$data, 
                       retro.settings = list(min.yrs=15), 
                       fit.settings = list(BoxCox=FALSE), 
                       fc.settings = list(BoxCox=FALSE),
                       tracing=FALSE,out.type="short")

names(arima.retro)
arima.retro$retro.pm.bal
arima.retro$retro.resids



# --------------------------------------
# do a bootstrap test for a model


arima.boot <- doBoot(data= data.withage, args.fitmodel= list(model= "TimeSeriesArima", settings = list(BoxCox=FALSE)),
                     args.calcfc = list(fc.yr= 2018,  settings = list(BoxCox=FALSE)),
                     args.boot = list(boot.type="meboot", boot.n=100, plot.diagnostics=FALSE),
                     full.out = TRUE, plot.out=TRUE)

head(arima.boot)


forecastR:::box.plot(arima.boot)



# ------------------------------------------
# calculate several forecasts and rank the models


settings.use <- list(Naive1 = list(model.type="Naive",settings=list(avg.yrs=1)),
                     Naive3 = list(model.type="Naive",settings=list(avg.yrs=3)),
                     Naive5 = list(model.type="Naive",settings=list(avg.yrs=5)),
                     SibRegSimple = list(model.type="SibRegSimple",settings=NULL),
                     SibRegLogPower =  list(model.type="SibRegLogPower",settings=NULL),
                     SibRegKalman =  list(model.type="SibRegKalman",settings=NULL),
                     TimeSeriesArimaBC = list(model.type="TimeSeriesArima",settings=list(BoxCox=TRUE)),
                     TimeSeriesArimaNoBC = list(model.type="TimeSeriesArima",settings=list(BoxCox=FALSE)),
                     TimeSeriesExpSmoothBC = list(model.type="TimeSeriesExpSmooth",settings=list(BoxCox=TRUE)),
                     TimeSeriesExpSmoothNoBC = list(model.type="TimeSeriesExpSmooth",settings=list(BoxCox=FALSE))
)



multiresults.ptfconly <- multiFC(data.file=data.withage.raw,settings.list=settings.use,
                                 do.retro=FALSE,retro.min.yrs=15,
                                 out.type="short",
                                 int.type = "None", int.n = 100, 
                                 boot.type = "meboot",
                                 tracing=FALSE)
multiresults.ptfconly



multiresults.retro <- multiFC(data.file=data.withage.raw,settings.list=settings.use,
                              do.retro=TRUE,retro.min.yrs=15,
                              out.type="short",
                              int.type = "None", int.n = 100, 
                              boot.type = "meboot",
                              tracing=FALSE)

# check the components of the multifc output
names(multiresults.retro$retro.pm)

# extract the fc table
multiresults.retro$table.ptfc

# extract 1 version of the retrospective performance summary
multiresults.retro[["retro.pm"]][["retro.pm.bal"]]


# do three alternative model rankings
ranktest1 <- rankModels(multiresults.retro$retro.pm$retro.pm.bal)
ranktest2 <- rankModels(multiresults.retro$retro.pm$retro.pm.bal, columnToRank = c("MRE","MAE") )
ranktest3 <- rankModels(multiresults.retro$retro.pm$retro.pm.bal, relative.bol=TRUE )

ranktest1$Total
ranktest2$Total
ranktest3$Total







#########################################################
# Example of looping through multiple stocks (i.e. input file)



# create a list of file names

datafile.list <- c("SampleData/2017_Alsea_Esc.csv",
                   "SampleData/2017_Nehalem_Esc.csv",
                   "SampleData/GSH_Terminal_Run_2017.csv",
                   "SampleData/SampleFile_WithAge_ExclTotal.csv")



# start a pdf
pdf("Example_Output.pdf",onefile=TRUE,height=8.5,width=11)

# loop through the files
for(file.use in datafile.list){
  
  print("------------------------")
  print(file.use)
  file.raw <- read.csv(file.use)  
  file.prepped <- prepData(file.raw,out.labels="v2")
  
  
  # Fit ARIMA model to the data set with age classes (no BoxCox transformation)
  arimafit.withage.nobc <- fitModel(model= "TimeSeriesArima", data = file.prepped$data, settings = list(BoxCox=FALSE),tracing=FALSE)
  arimafc.withage.nobc <- calcFC(fit.obj= arimafit.withage.nobc ,data =file.prepped$data, fc.yr= file.prepped$specs$forecastingyear,  settings = list(BoxCox=FALSE), tracing=TRUE)	
  names(arimafc.withage.nobc)
  arimafc.withage.nobc$pt.fc
  plotModelFit(arimafit.withage.nobc, options= list(plot.which = "fitted_ts",age.which="all",plot.add=FALSE),fc.add = arimafc.withage.nobc)
  title(main=paste(file.use, "ARIMA - No Box Cox"),outer=TRUE,line=-1)
  
  
}

#close the pdf
