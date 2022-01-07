
list.of.packages <- c("tictoc", "haven", "foreign", "readxl", "tidyverse", "stringr", "matlib", "quantreg", "gtsummary", "stargazer", "shiny", "apollo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tictoc)
library(haven)
library(foreign)
library(readxl)
library(tidyverse)
library(stringr)

library(matlib)
library(Matrix)

library(quantreg)

library(gtsummary)
library(stargazer)
library(shiny)

library(apollo)

rm(list = ls())


