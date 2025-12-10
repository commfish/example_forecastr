# Taku River Sockeye Salmon Preseason Forecasting
# author: Sara E. Miller 
# contact: sara.miller@alaska.gov; 907-465-4245
# Last edited: January 2025
# This code runs the preseason forecast for Taku River sockeye salmon
https://github.com/SalmonForecastR/ForecastR-Releases/wiki/4-Using-the-ForecastR-package
# load libraries----
library(tidyverse) 
library(ggplot2)
library(broom)
library(extrafont)
library(scales)
library(devtools)
#library(forecast)
library(caret)
library(MLmetrics)
library(zoo) # rollmean function
library(cowplot)
library(nlme) # AR1 model
library(ggpubr) # regression line and stats in ggplot 
devtools::install_github("commfish/fngr")
library(fngr)
library(dplyr)
# font_import() # uncomment and run once, then comment out
loadfonts(device = "win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
source('forecasts/preseason/code/2026_preseason/functions.r')
install.packages("remotes") 
remotes::install_git("https://gitlab.com/MichaelFolkes/psctools")
remotes::install_git("https://gitlab.com/transboundary-committee/sourcecode/r-packages/TBRforecasting")

require(PSCtools)
# set graphics----
theme_set(theme_report(base_size = 14))

# set inputs for the current season
year <- 2025 # forecast year
year.subfolder <- "2026_preseason" # subfolder for forecast
initial_year <- 1984

data.directory <- file.path('forecasts','preseason', 'data', year.subfolder)
if(!dir.exists(file.path("forecasts/preseason/output",year.subfolder))){dir.create(file.path("forecasts/preseason/output",year.subfolder))}
results.directory <- file.path('forecasts','preseason', 'output', year.subfolder)

# load data----
read.csv(file.path(data.directory,'Taku_sockeye_brood_table.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> brood

str(brood)

res <- broodTableToForecastR(brood)
str(res)
tail(res$data.long.ages.EU)

# analysis----
# create data set for forecast models
brood_table %>%
  rowwise() %>%
  mutate(returns = sum(age_01,	age_02, age_11,	age_03, age_12, age_21, age_04, age_13, age_22, age_31, age_05, age_14,
                       age_23, age_32, age_15, age_24, age_33, age_42, na.rm = TRUE), # add all ages for total return by brood year
         escapement = as.numeric(escapement)) %>%
  mutate(returns = as.numeric(returns)) %>%
  as.data.frame() -> brood_table	# model data set

# spawner-recruit figure
brood_table %>%
  filter(brood_year >= initial_year & brood_year <= (year-8)) %>%
  mutate(lnRS = log(as.numeric(returns)/as.numeric(escapement))) %>%
  as.data.frame() %>%
  mutate(escapement = as.numeric(escapement)) %>%
  lm(lnRS ~ escapement, data = .) -> model.m1 # linear model of ln(R/S) ~ spawners
summary(model.m1)$coefficients[1, 1] -> intercept #extract intercept from model
summary(model.m1)$coefficients[2, 1] -> slope #extract slope from model

brood_table %>%
  filter(brood_year >= initial_year & brood_year <= (year-6)) -> brood_table_fig # filter data years for figure (complete returns only)

replacement_line = seq(from = 0, to = 350000, by = 5000)
as.data.frame(replacement_line) %>%
  mutate(pred = (exp(intercept + slope*replacement_line)*replacement_line)) %>% # predicted curve based on regression model
ggplot(aes(x = replacement_line, y = pred)) +
  geom_line(color ="black") + 
  geom_line(aes(x = replacement_line, y = replacement_line), color ="grey50", lty = 2) +
  geom_point(aes(x = escapement, y = returns), data = brood_table_fig, color ="grey50", size=3) + 
  scale_x_continuous(labels = comma,breaks = seq(0, 350000, 50000), limits = c(0, 350000)) +
  scale_y_continuous(labels = comma,breaks = seq(0, 350000, 50000), limits = c(0, 350000)) +
  labs(y = "Recruits", x =  "Spawners") +
  theme_bw() + theme(panel.grid.major = element_blank(),plot.title = element_text(hjust = 0.5),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) -> plot1
ggsave(paste0(results.directory, "/stock_recruit.png"), dpi = 500, height = 3, width = 6, units = "in")

##https://quant.stackexchange.com/questions/51207/how-to-compute-prediction-interval-if-using-simple-moving-average-t-o-predict