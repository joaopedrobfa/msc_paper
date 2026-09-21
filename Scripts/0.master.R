#------------------------------------------------------------------------------#
# 0. Setup and libraries
#------------------------------------------------------------------------------#

library(here)
setwd(here())

library(tidyverse)
library(PNADcIBGE)
library(stringr)
library(didimputation)
library(haven)
library(did)
library(fixest)
library(polars)
library(DIDmultiplegtDYN)
library(stargazer)
library(kableExtra)
