# Taku River Sockeye Salmon Preseason Forecasting
# author: Sara E. Miller 
# contact: sara.miller@alaska.gov; 907-465-4245
# Last edited: January 2025
# This code runs the preseason forecast for Taku River sockeye salmon

# load code----
install.packages("remotes") 
remotes::install_git("https://gitlab.com/transboundary-committee/sourcecode/r-packages/tbrforecasting")
require(TBRforecasting)
# load data----
read.csv(file.path(data.directory,'Taku_sockeye_brood_table.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> brood

str(brood)
age.cols <- paste(substr(colnames(brood)[3:ncol(brood)],5,5),
                  substr(colnames(brood)[3:ncol(brood)],6,6), sep=".")

colnames(brood) <- c("Brood_Year", "escapement", age.cols)
str(brood)

res <- TBRforecasting::broodTableToForecastR(brood)
str(res)

