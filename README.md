# Code for "Extremile scalar-on-function regression with applicaton to climate scenarios"
This repository contains the scripts to create all the plots of "Extremile scalar-on-function regression with application to climate scenarios" by M.L. Battagliola and M. Bladt (2024) (preprint available [here](https://arxiv.org/abs/2405.20817)).

The raw data for the application come from datasets created within the [CH2018 Swiss Climate Change Scenarios project](https://www.nccs.admin.ch/nccs/en/home/climate-change-and-impacts/swiss-climate-change-scenarios/ch2018---climate-scenarios-for-switzerland.html), and can be downloaded [here](https://map.geo.admin.ch/?lang=en&topic=meteoschweiz&bgLayer=voidLayer&E=2658433.38&N=1217236.43&zoom=1.0881550434129363&layers=ch.bafu.gefahren-basiskarte,ch.meteoschweiz.klimaszenarien-raumklima,ch.meteoschweiz.messwerte-lufttemperatur-10min&layers_opacity=0.7,1,1&catalogNodes=15046,15055,15126,15138&layers_visibility=true,true,false). 

The statistical analysis rely on [ExtrFunReg package](https://github.com/LauraBattagliola/ExtrFunReg), which allows to perform extremile regression when covariates are functions.
