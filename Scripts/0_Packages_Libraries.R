
list.of.packages <- c(
  "tictoc", 
  "haven", "foreign", "readxl", "tidyverse", "stringr", "rje",
  "matlib", "nloptr", "slam", "quantreg", "apollo", "optimParallel","parallel",
  "gtsummary", "stargazer", "shiny", "kableExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# webshot::install_phantomjs()

library(MASS)
library(Matrix)
library(matlib)
library(nloptr)
library(parallel)
library(apollo)
library(optimParallel)

library(gtsummary)
library(stargazer)
library(shiny)
library(kableExtra)

library(tictoc)
library(haven)
library(foreign)
library(readxl)
library(stringr)
library(rje)
library(tidyverse)

rm(list = ls())