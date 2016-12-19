# Tariffs-17-18

Tariff price comparison for 17/18

Features of the app:  
1) Price comparision of any 16/17 HRG against its 17/18 HRG counterpart  
2) Semantics analysis - still under construction  

This project comprises five sections:  
1) Background - contains all the links to the raw data files  
2) Data - two types of data, raw or processed.  
3) Scripts-Analytics - Contains the scripts to transform raw data into data tables that feed into Shiny App  
    3a) CR_Clean_Hrg_Price_Data - cleans raw data file containing HRGs and their prices  
     3b) AN_Hrgs_Combined_Tariff - Links the 16/17 HRGs with their 17/18 HRG counterparts and merges on prices.   
4) Scripts-App - Contains the server and UI file to run the shiny app  
5) Outputs - contains non-shiny outputs such as wordcloud  
