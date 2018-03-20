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

| STATION (in database) | Description       | Location                  |
|-----------------------|-------------------|---------------------------|
| 17                    | Brandy Cove       | 45° 5.01’N, 67° 5.16’W    |
| 15                    | Deadmans          | 45° 2.882’N, 66° 46.644’W |
| 3                     | Lime Kiln         | 45°3.27’N, 66°49.2’W      |
| 25                    | Passamaquoddy Bay | 45° 4.0’N, 66° 58’W       |
| 16                    | Wolves            | 44° 59.35’N, 66°44.2W     |
| 46                    |                   | 44° 53.6’N, 66° 40.05’W   |
| 57                    |                   | 44° 50.583’N, 66°35.667’W |
| 51                    |                   | 45° 8.05’N, 67° 1.6’W     |

3.	**S4PROBE DATA**:  
a.	`LOSIERR.S4PROBE`

4.	**DRIFTER DATA**:  
a.	`LOSIERR.CASTDRFT`

5.	**TEMPERATURE-DEPTH RECORDERS**  
a.	`LOSIERR.TDRINF` – gives location of deployment  
b.	`LOSIERR.TDRLOG` – gives timeseries of temperature and depth for a given station. Related to `LOSIERR.TDRINF` through `station`.
