
rm(list = ls())

library("readxl")
library(tidyverse)
library(zoo)
library(profvis)
library(ggplot2)
library(data.table)
library(openxlsx)
library(data.table)
library(Rcpp)
library(memoise)


source("./Models/UtilityFunctions.R")

source("./Models/Inputs.R")

sourceCpp("./Models/sourceRcpp.cpp")
sourceCpp("./Models/future_value_cpp.cpp")

s_ben <- Sys.time()
source("./Models/BenefitModel.R")
e_ben <- Sys.time()

source("./Models/WorkforceModel.R")
# get_wf_data()


wf_projection <- readRDS("./Models/wf_data.rds")
source("./Models/LiabilityModel.R")

source("./Models/FundingModel.R")

