COERS PTRAN Table Reference Guide
=================================
*This information was received from Susan Haigh on December 13, 2017 *

1.	**ADCP DATA**: There are three tables for adcp and they are all related.  
  a. `LOSIERR.ADCPPLAN`: describes the adcp setup  
  b. `LOSIERR.ADCPINF`: gives the position and a two dimensional time series of the pressure gauge data (related to `LOSIERR.ADCPPLAN` through `CRUNO` and `STATION`)  
  c. `LOSIERR.ADCPVEL`: for each time segment in the `LOSIERR.ADCPINF` table, gives the vertically varying currents (related to `LOSIERR.ADCPPLAN` through `CRUNO` and `STATION`, related to `LOSIERR.ADCPINF` through `CRUNO`, `STATION` and `SEGMENT`)

2.	**CTD DATA**:
a.	`LOSIERR.SABS_OSDINF`: gives location, date & time of CTD cast  
b.	`LOSIERR.SABS_OSDHYD`: gives depth varying temperature, salinity and density of CTD cast.    Related to `LOSIERR.SABS_OSDINF` through `DEPLOY`
c.	`LOSIERR.PRINCE`:  long term data for Prince 5, 6 and 7 stations  
d.	`LOSIERR.SABS_MESDHYD`: Jennifer Martin’s data.  The stations numbers are for the following locations:   

| Site | Location          | Northing (dd.ddd°, WGS84) | Easting (dd.ddd°, WGS84) |
|------|-------------------|---------------------------|--------------------------|
| 17   | Brandy Cove       | 45.084                    | -67.086                  |
| 15   | Deadmans          | 45.048                    | -66.777                  |
| 3    | Lime Kiln         | 45.055                    | -66.820                  |
| 25   | Passamaquoddy Bay | 45.067                    | -66.967                  |
| 16   | Wolves            | 44.989                    | -66.737                  |
| 46   |                   | 44.893                    | -66.668                  |
| 57   |                   | 44.843                    | -66.594                  |
| 51   |                   | 45.134                    | -67.027                  |

3.	**S4PROBE DATA**:  
a.	`LOSIERR.S4PROBE`

4.	**DRIFTER DATA**:  
a.	`LOSIERR.CASTDRFT`

5.	**TEMPERATURE-DEPTH RECORDERS**  
a.	`LOSIERR.TDRINF` – gives location of deployment  
b.	`LOSIERR.TDRLOG` – gives timeseries of temperature and depth for a given station. Related to `LOSIERR.TDRINF` through `station`.
