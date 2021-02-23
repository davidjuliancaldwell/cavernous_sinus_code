#########################################
# load libraries
library(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggsci)
library(gridExtra)
library(cowplot)
library(here)
library(sjPlot)
library(coin)
library(emmeans)
library(readxl)

rootDir = here()
dataDir = 'C:/Users/david/OneDrive - UW/Cavernous sinus project'


### read data
data_file <- read_excel(file.path(dataDir,"Cavernous sinus cases, data collection sheet.xlsx"))
