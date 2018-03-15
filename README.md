CESD Utilities repository
===============================================

This folder contains the working copies of CESD utilities/scripts for processing specific data types.

Description
-----------
This folder contains the working copies of CESD utilities/scripts for processing specific data types. The initial scripts are a mix of Fortran and SQL as that was what was used historically to process data from CTD/ADCP/Drifters. The goal is to update the scripts where possible to be more user friendly/understandable to process similar data in a consistent manner, most likely in R.

Folder structure:
-----------------
*	**1-Documentation**: Store documentation regarding the project methodology here. *e.g. specific methodology, data processing steps*
* **2-Data**: folder to store all observation, measurement, model, and processed data.  
	*	**1-RawData**: Store the raw data files generated here (Preferably in an open format, *i.e. .txt or .csv*
	*	**2-ProcessedData**: Store cleaned, processed, and QC/QA'd data here.  
*	**3-Code**: folder for all scripts used in order to process the data. *i.e. QA/QC, data manipulation, reformatting, or deriving values*
*	**4-Products** folder is to store all finished data products derived from the data found in the **Data** folder using the scripts in **Code** folder unless it was processed using a graphical method (not reproducible).
	*	**1-Reports**: Store published documents/reports/papers here
	*	**2-OpenData
