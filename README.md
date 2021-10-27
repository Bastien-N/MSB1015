# MSB1015 Scientific Programing
# Bastien Nihant
Scripts should be ran in the order: 
1_cleaning_final.R  THEN 2_analysis_RF.R 
Any manual inputs are at the biggining of each script.

(Both scripts) Recent package updates may cause problems running the code if you have old version of library dependencies installed.
In case of error related to the libraries, setting the allPackagesUpdate to TRUE may fix the issue, but will make the library loading fairly long.

(Script 2) The cross validation loop in 2_analysis_RF.R can take up to several hours,
set the skipCVloop parameter to TRUE to avoid it and load pre-ran results (Default is TRUE)