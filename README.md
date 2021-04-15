# AQAproject

This page provides R code for the analysis of the article "The effects of an air quality alert program on premature mortality: a difference-in-differences evaluation in the region of Paris" written by Anna Alari (INSERM, France), Lara Schwarz (UCSD, San Diego), Léo Zabrocki (Paris School of Economics), Géraldine Le Nir (Air Quality Monitoring Associations, Airparif, France), Basile Chaix (INSERM, France) and Tarik Benmarhnia (UCSD, San Diego). 

The R files contained: 
- one code for PM10 datamangement. 
This step creates a variable "eligibility", which indicates, for each day, if the day is eligibile to Air Quality informational Alerts (AQAs) or Air Quality Warning Alert according to the daily level of PM10 concentrations recorded by background and traffic monitoring stations. Threshold levels were defined according to the regulamentary values of AirParif : https://www.airparif.asso.fr/procedure-dinformation-et-dalerte. Days that were considered as "eligible" after the end of 2007 (when AQA policy was implemented) are also compared with the effective list of days that were declared as AQ alerts (https://www.airparif.asso.fr/historique-des-episodes-de-pollution). 
In order to define our final variable "elipic" (the one used in the PSM step and in the DID models) we use: 
  * the list of eligible days detected with our data from monitoring stations and according to the first threshold value criterias for the period before AQA reglementation (before the end of 2007)
  * the official list of AQA days provided in the AirParif website (because it rappresents a reliable information to know when AQA measures were applyed) for the period after AQA reglementation


 

- one code for the bootstrap procedure of the Propensity Score Matching and DID models


