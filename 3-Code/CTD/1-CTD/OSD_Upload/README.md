`OSD_Upload` Script
==================
This script is being developed to replace the `ctd_global.load` legacy script used by COERS in order to load data to Randy Losier's tables (`LOSIERR.SABS_OSDINF/OSDHYD`) on the DFO Oracle production database (PTRAN). 

**2018-07-17** 

- Currently able to login to tables, determine the last `DEPLOY` number, and format ASC files from the SeaBird software, and create a file in the working directory or both the cast information as well as the hydrographic data collected in the format `DEPLOY_OSD_INF/HYD.csv`. 
- User needs to manually enter the LATITUDE/LONGITUDE/HEXFILE/CONFILE.