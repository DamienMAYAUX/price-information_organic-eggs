
list.of.packages <- c("haven", "foreign", "readxl", "tidyverse", "quantreg", "gtsummary", "stargazer", "shiny", "apollo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(haven)
library(foreign)
library(readxl)
library(tidyverse)
library(stringr)

library(quantreg)

library(gtsummary)
library(stargazer)
library(shiny)

library(apollo)

rm(list = ls())


