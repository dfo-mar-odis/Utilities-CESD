echo "enter Oracle username"
read Ousername
Uusername=losierr     
if [ "$Ousername" = "sarahjscouten" ]
then
Uusername=scoutens
password=Aquatic12
else
password=For5tat5
fi
rm adcpdepthrequest.sql
rm tide.tab            
rm tide.hgt            
rm rtrandep.dat          
rm rtranvel.dat          
rm rtraninf.dat        
rm xxx2.sql
rm pppp.sql
rm axx.sql
rm tidechk.sql
rm clean.sql
rm update.sql
rm xxx.sql
rm check.sql
rm insert.sql
echo " This script allows you to load data on the database or verify a previous load "
echo " by producing a figure. Make sure that you have enablee Xming or from the server"
echo " used the -X syntac to enable the graphic interface ie ssh -X 142.2.15.232"
echo " "
echo 'Do you want to do a load or verify a load l/v?'
read loadveri
if [ "$loadveri" = "v" -o "$loadveri" = "V" ]
then
./adcpplot.com
exit 1
else
echo '      This script will check Databases for updates first.'
echo ""
echo "
set pages 100;
set trim on;
SET WRAP OFF;
SET LINESIZE 132;
set numwidth 6;
column area format a20;
column morenote format a20;
select max(deploy) adcpplan_deploy,cruno,station,area,morenote from losierr.adcpplan 
group by deploy,cruno,station,area,morenote order by deploy,cruno,station,area;
exit;
" > check.sql
sqlplus $Ousername/$password@ptran @check.sql
echo " "
echo "....................................................................."
echo "....................................................................."
echo " "
echo "      This  Load  Requires  that  certain  procedures were followed. "
echo "      SentinelV data must be processed using Velocity Software.      "
echo "      ASCIIOUT parameters are described in the vsentinel.doc file.   "
echo " "
echo "....................................................................."
echo "....................................................................."
echo " "
echo " "
ls *.txt
ls *.TXT
ls *.adcp
rm sentfile.dat
rm sentfile2.dat
rm sentinel.bat
rm sentinel2.bat
rm sentinel3.bat
echo "Enter VELOCITY Asciiout output filename ?"
read filename
cp $filename sentfile.dat
dos2unix sentfile.dat sentfile.dat
echo sentfile.dat > sentinel.bat
echo sentfile2.dat >> sentinel.bat
echo "
select max(adcpplan.deploy)+1 from losierr.adcpplan;
select max(adcpplan.cruno) from losierr.adcpplan;
exit;
">xxx.sql
sqlplus $Ousername/$password@ptran @xxx.sql
echo " "
echo "          From the ORACLE selection above, check deployment info"
echo "         The highest value should be your next deployment number"
echo " "
echo "Do you want to continue y/n ? \c"
read reply
if [ "$reply" = "N" -o "$reply" = "n" ]
then
exit 1
fi
echo "Enter number of depth cell displayed in the"
echo "Velocity Program labelled number of cells?"
read ncells
echo $ncells >> sentinel.bat
echo "Enter cell size displayed in the Velocity"
echo "Program labelled cell size?"
read cellsize
echo $cellsize >> sentinel.bat
echo "Enter 1st cell range displayed in the Velocity"
echo "Program labelled first cell range"
read crange
echo $crange >> sentinel.bat
echo "Enter distance from mooring weight to transducer face?"
read mdist
echo $mdist  >> sentinel.bat
./vsentinel_rfmt < sentinel.bat
echo "     Reformat Program Complete "
echo " Start input for database file setup"
echo sentfile2.dat > sentinel2.bat
echo "Enter SentinelV Serial Number ?"
read serial
echo $serial >> sentinel2.bat
echo ""
echo "Enter Consecutive deployment number ?"
read deploy
echo $deploy >> sentinel2.bat
echo "Enter Cruise number ?"
read cruno    
echo $cruno >> sentinel2.bat
echo "Enter Station Number ?"
read station 
echo $station >> sentinel2.bat
echo "Enter Local Magnetic Variation ?"
read lmagvar
echo $lmagvar >> sentinel2.bat
echo "Enter sentinelv Magnetic Variation setup ?"
read magvar
echo $magvar >> sentinel2.bat
echo " Enter Ping Interval used during sentinelv setup?"
read pingint
echo $pingint >> sentinel2.bat
echo " Enter Number of Pings recorded in a burst reading?"
read numpings
echo $numpings >> sentinel2.bat
echo "Was the data collected in a leap year?"
echo "ie. 2000,2004,2008,2012,2016..."
read lyear
echo $lyear >> sentinel2.bat
echo "Was SENTINEL $serial in TRANSECT OR MOORED mode?"
echo "Enter T or M now"
read tmode
case $tmode in
[Tt]) tmode=T;;
[Mm]) tmode=M;;
esac
echo deployment mode=$tmode
if [ $tmode = M ]
 then
 echo "Enter deployment Latitude and Longitude now ?"
 read lat long
 echo $lat,$long >> sentinel2.bat
fi
if [ $tmode = M ]
 then
echo " "
echo "$echodepth    "
echo 0 > sentinel3.bat
fi
echo "Do you want to compute Tidal Harmonics y/n? "
read tihr
if [ $tihr = "Y" -o $tihr = "y" ]
 then
echo y >> sentinel2.bat
echo 9 >> sentinel2.bat
echo " ............................................................"
echo " ............................................................"
echo "    Partial list of Foreman Tidal Constituent Station numbers"
echo "      "
echo "    1     Outer Wood Island                                  "
echo "    5     Seal Cove                                          "
echo "    7     Gannet Rock                                        "
echo "    10    North Head                                         "
echo "    11    West Quoddy Head                                   "
echo "    15    Welshpool                                          "
echo "    16    North Lubec                                        "
echo "    20    Wilson's Beach                                     "
echo "    21    East Quoddy Head                                   "
echo "    24    Eastport                                           "
echo "    25    Fairhaven                                          "
echo "    28    MacMaster Island                                   "
echo "    29    Matthew's Cove                                     "
echo "    30    Back Bay                                           "
echo "    33    Letang Harbour (Lime Kiln)                         "
echo "    40    St. Andrews                                        "
echo "    41    Long Island                                        "
echo "    44    Beaver Harbour                                     "
echo "    46    Dipper Harbour                                     "
echo "    53    Musquash Harbour                                   "
echo "    65    Saint John                                         "
echo "    425   Shelburne NS                                       "
echo "    690   Pushthrough                                        "
echo "    705   St Alban                                           "
echo "    710   Hermitage                                          "
echo "    720   Harbor Breton                                      "
echo "    724   Belloran                                           "
echo "    1915  Rustico PEI                                        "
echo "............................................................."
echo "............................................................."
echo "Enter Foreman Tidal Constituent Station number?"
rm adcpleadscr.bat
read foreman 
echo $foreman > adcpleadscr.bat
echo $foreman  >> sentinel2.bat
else
echo n >> sentinel2.bat
fi
gfortran -fno-automatic -o vsentinal_asciiout vsentinal_asciiout.f
gfortran -fno-automatic -o vsentinal_asciiout_leapyear vsentinal_asciiout_leapyear.f
if [ "$lyear" = "N" -o "$lyear" = "n" ]
then
./vsentinal_asciiout < sentinel2.bat
else
./vsentinal_asciiout_leapyear < sentinel2.bat
fi
echo " "
echo " "
echo 'Enter geographic area of deployment? \c '
read area
#area='Passamaquoddy Bay'
echo 'Enter any comments you want entered on the database? \c '
read addcom         
echo "
select max(adcpplan.deploy) from losierr.adcpplan;
exit;
">update.sql
sqlplus $Ousername/$password@ptran @update.sql
dbplan=losierr.adcpplan
dbinf=losierr.adcpinf
dbvel=losierr.adcpvel
echo "
select max(adcpplan.deploy) from losierr.adcpplan;
exit;
">update.sql
sqlplus $Ousername/$password@ptran @update.sql
dbplan=losierr.adcpplan
dbinf=losierr.adcpinf
dbvel=losierr.adcpvel
echo "*****************************************************************"
echo "NOTE... Your last deployment number in the above ORACLE selection"
echo " If this value = $deploy then this script should be aborted "
echo "*****************************************************************"
echo " "
echo " Abort Script y/n ? \c"
read reply
if [ "$reply" = "y" ]
then
  echo "        This job has been aborted because you are attempting to load"
  echo "        a previously used deployment . See your DBASE administrator."
  exit 1
fi
echo "......................................................."
echo "      This Proceedure file allows you to load"
echo "     edited SENT data to adcp oracle database."
echo "......................................................."
echo " "
echo "Loading SENT deployment $deploy for cruise $cruno and station $station \c"; date
echo " "
echo " This will delete all SENT deployments $deploy cruise $cruno and station $station from the database "
echo " Are you sure you want to proceed with the load y/n ?"
read answer
if [ "$answer" = "y" ]
then
echo "
 -- cleaning out Deployment $cruno from ORACLE DATABASE
 delete from $dbplan  where cruno='$cruno' and station=$station and deploy=$deploy;
 delete from $dbinf where cruno='$cruno' and station=$station;
 delete from $dbvel where cruno='$cruno' and station=$station;
 exit;
 ">clean.sql
sqlplus $Ousername/$password@ptran @clean.sql
rm clean.sql
echo "
-- Loading adcpplan data info for deployment $deploy into ORACLE
LOAD DATA
INFILE rtrandep.dat
-- INFILE SPECIFIES THE DATA TO BE LOADED
APPEND INTO TABLE $dbplan           
(deploy        POSITION(01:05) INTEGER EXTERNAL,
 cruno         POSITION(07:11) char,
 station       POSITION(12:15) INTEGER EXTERNAL,             
 serial        POSITION(16:21) INTEGER EXTERNAL,                 
 unitkhz       POSITION(22:26) INTEGER EXTERNAL,                     
 unit_magvar   POSITION(33:38) DECIMAL EXTERNAL,
 area_magvar   POSITION(27:32) DECIMAL EXTERNAL,
 binsize       POSITION(39:43) DECIMAL EXTERNAL,          
 tranblnk      POSITION(44:50) DECIMAL EXTERNAL,
 mount_depth   POSITION(51:57) DECIMAL EXTERNAL,                 
 bincount      POSITION(58:64) DECIMAL EXTERNAL,                 
 rawpings      POSITION(65:69) integer EXTERNAL,                 
 ping_interval POSITION(70:75) decimal EXTERNAL,                 
 segment_time  POSITION(76:81) decimal EXTERNAL,                 
 measure       POSITION(83:84) char,                             
 depth_ref     POSITION(86:87) char,                         
 data_mode     POSITION(89:89) char,                             
 area          POSITION(90:110) char,                             
 morenote      POSITION(111:190) char)                             
 ">adcpplan.ctl
scp adcpplan.ctl 142.2.12.23:/home/$Uusername
scp rtrandep.dat 142.2.12.23:/home/$Uusername
ssh 142.2.12.23 "(PATH=/usr/local/bin:/usr/ucb:$HOME/bin:/usr/bin/X11:/sbin:/u01/app/oracle/product/11.2.0/bin:.;
export LD_LIBRARY_PATH=/u01/app/oracle/product/11.2.0/lib/;
export ORACLE_HOME=/u01/app/oracle/product/11.2.0;
sqlldr $Ousername/$password@ptran adcpplan.ctl,adcpplan.log,adcpplan.bad;)"
scp 142.2.12.23:/home/$Uusername/adcpplan.log ./
# Back on OLDSOW
more -20 -d adcpplan.log
rm adcpp*.ctl
rm adcpp*.bad
rm adcpp*.log
echo  "update $dbplan set area= '$area' where cruno = '$cruno' and station = $station;" > update.sql
echo  "update $dbplan set morenote= '$addcom' where cruno = '$cruno' and station = $station;" >> update.sql
echo "exit;" >> update.sql
sqlplus $Ousername/$password@ptran @update.sql
rm update.sql
echo "
-- Loading $dbvel data info for $1 into ORACLE
LOAD DATA
INFILE rtranvel.dat
APPEND INTO TABLE $dbvel          
(cruno         POSITION(01:05) char,
 station       POSITION(06:09) INTEGER EXTERNAL,             
 segment       POSITION(10:14) INTEGER EXTERNAL,                 
 depthbot      POSITION(15:21) DECIMAL EXTERNAL,                     
 depthsur      POSITION(22:28) DECIMAL EXTERNAL,
 speed         POSITION(29:34) DECIMAL EXTERNAL,
 direction     POSITION(35:40) DECIMAL EXTERNAL,          
 timedec       POSITION(41:48) DECIMAL EXTERNAL,
 Veleast       POSITION(49:55) DECIMAL EXTERNAL,                 
 Velnorth      POSITION(56:62) DECIMAL EXTERNAL,                 
 Velvert       POSITION(63:69) DECIMAL EXTERNAL,                 
 Velerror      POSITION(70:76) DECIMAL EXTERNAL,                 
 intensity     POSITION(77:83) DECIMAL EXTERNAL)
 ">adcpvel.ctl
scp adcpvel.ctl 142.2.12.23:/home/$Uusername
scp rtranvel.dat 142.2.12.23:/home/$Uusername
ssh 142.2.12.23 "(PATH=/usr/local/bin:/usr/ucb:$HOME/bin:/usr/bin/X11:/sbin:/u01/app/oracle/product/11.2.0/bin:.;
export LD_LIBRARY_PATH=/u01/app/oracle/product/11.2.0/lib/;
export ORACLE_HOME=/u01/app/oracle/product/11.2.0;
sqlldr $Ousername/$password@ptran adcpvel.ctl,adcpvel.log,adcpvel.bad;)"
scp 142.2.12.23:/home/$Uusername/adcpvel.log ./
more -20 -d adcpvel.log
rm adcpv*.ctl
rm adcpv*.bad
rm adcpv*.log
echo "
-- Loading adcpinf data info for $1 into ORACLE
LOAD DATA
INFILE rtraninf.dat
-- INFILE SPECIFIES THE DATA TO BE LOADED
APPEND INTO TABLE $dbinf          
(cruno         POSITION(01:05) char,
 station       POSITION(06:09) INTEGER EXTERNAL,             
 segment       POSITION(10:14) INTEGER EXTERNAL,                 
 sdate         POSITION(15:32) date 'yymmddhh24miss',
 depth         POSITION(33:39) DECIMAL EXTERNAL,                 
 pitch         POSITION(40:46) decimal EXTERNAL,                 
 roll          POSITION(47:53) decimal EXTERNAL,                 
 heading       POSITION(54:60) decimal EXTERNAL,                 
 latitude      POSITION(61:70) decimal external,                 
 longitude     POSITION(71:80) decimal external,             
 tidehgt       POSITION(81:86) decimal external,                 
 flow          POSITION(88:88) char,              
 temp          POSITION(89:94) decimal external,
 btveast       POSITION(95:102) decimal external,                 
 btvnorth      POSITION(103:110) decimal external,                 
 btvert        POSITION(111:118) decimal external,                 
 btverr        POSITION(119:126) decimal external)
 ">adcpinf.ctl
scp adcpinf.ctl 142.2.12.23:/home/$Uusername
scp rtraninf.dat 142.2.12.23:/home/$Uusername
ssh 142.2.12.23 "(PATH=/usr/local/bin:/usr/ucb:$HOME/bin:/usr/bin/X11:/sbin:/u01/app/oracle/product/11.2.0/bin:.;
export LD_LIBRARY_PATH=/u01/app/oracle/product/11.2.0/lib/;
export ORACLE_HOME=/u01/app/oracle/product/11.2.0;
sqlldr $Ousername/$password@ptran adcpinf.ctl,adcpinf.log,adcpinf.bad;)"
scp 142.2.12.23:/home/$Uusername/adcpinf.log ./
more -20 -d adcpinf.log
rm adcpinf.ctl
rm adcpinf.log
rm adcpinf.bad
mv $filename $cruno\_$deploy.adcp
echo " "
# Remove garbage records. Usually 1st segments or last segments of a deployment. These 
# Passed the QC programs however for data consistency should be removed. Tidal range
# is then displayed. The range must be correct.
# Assign surdepth the same value for depth for tidal analysis
echo "
update $dbinf set surdepth=depth where surdepth is null and cruno='$cruno' and station=$station;
drop table deleteens;
create table deleteens as select segment from $dbinf where cruno='$cruno' and station=$station and depth < .75;
delete from $dbinf where $dbinf.segment in (select deleteens.segment from deleteens) and cruno='$cruno' and station=$station;
delete from $dbvel where $dbvel.segment in (select deleteens.segment from deleteens) and cruno='$cruno' and station=$station;
select min(depth) mindepth, max(depth) maxdepth, max(depth)-min(depth) TidalRange from $dbinf where cruno='$cruno' and station=$station;
exit;
">tidechk.sql
sqlplus $Ousername/$password@ptran @tidechk.sql
echo " "
echo "
select distinct(depthbot),count(depthbot) from $dbvel where cruno='$cruno' and station=$station and depthbot < 11 
group by depthbot order by depthbot;
exit;
">xxx.sql
sqlplus $Ousername/$password@ptran @xxx.sql
fi
echo " "
echo " We know that data within 1 bin of the Surface is contaminated"         
echo " Do you want to remove these values y/n"
read reply4
if [ "$reply4" = "y" -o "$reply4" = "Y" ]
then
echo " Removing velocity data within 1 bin of the surface. Press Enter? "
echo "
select cruno,station,binsize from $dbplan where cruno='$cruno' and station=$station;
exit;
">xxx.sql
sqlplus $Ousername/$password@ptran @xxx.sql
echo " From the display above what is the binsize of this deployment?"
read binnsize
echo " Do you want to remove these surface values y/n"
read renter
echo "
delete from $dbvel where depthsur < $binnsize+($binnsize/2) and cruno='$cruno' and station=$station;
select distinct(depthbot) from $dbvel where cruno='$cruno' and station=$station order by depthbot;
exit;
">xxx.sql
sqlplus $Ousername/$password@ptran @xxx.sql
fi
echo " Lets verify the depth value on the adcpinf ORACLE table "
rm adcpdepthrequest.sql
echo "
drop table adcpsegment;
create table adcpsegment as select cruno,station,min(segment) minseg
from losierr.adcpinf where
cruno='$cruno' and
station=$station
group by cruno,station;
drop table adcpsegment2;
create table adcpsegment2 as select adcpsegment.cruno,adcpsegment.station,depth,minseg
from adcpsegment,losierr.adcpinf where
adcpsegment.cruno=losierr.adcpinf.cruno and
losierr.adcpinf.station=adcpsegment.station and
minseg = segment;
drop table adcpsegment3;
create table adcpsegment3 as select adcpsegment2.cruno,adcpsegment2.station,minseg,
depthbot,depthsur,depthbot+depthsur depthadd
from adcpsegment2,losierr.adcpvel where
losierr.adcpvel.cruno='$cruno' and
losierr.adcpvel.station=$station and
minseg = segment;
drop table adcpsegment4;
create table adcpsegment4 as select adcpsegment3.cruno,adcpsegment3.station,adcpsegment3.minseg,
depthbot,depthsur,depthadd,depth,depthadd-DEPTH presdiff
from adcpsegment3,adcpsegment2;
select cruno,station,minseg,presdiff,count(presdiff) count
from adcpsegment4 group by cruno,station,minseg,presdiff order by cruno,station;
exit;
" > adcpdepthrequest.sql
sqlplus $Ousername/$password@ptran @adcpdepthrequest.sql
echo " From the display above what is the value under the column PRESDIFF?"
read presdiff
echo " The Pressure data value on the adcpinf table will now be corrected to total water depth "
echo " Do you want to adjust the depth values in adcpinf y/n?"
echo " If PRESDIFF is zero, you can answer n."
read reply5
if [ "$reply5" = "y" -o "$reply5" = "Y" ]
then
echo "
update $dbinf set depth=depth+$presdiff where cruno='$cruno' and station=$station;
update $dbinf set longitude=longitude*-1.0 where cruno='$cruno' and station=$station and longitude > 0;
exit;
">xxx.sql
else
echo "
update $dbinf set longitude=longitude*-1.0 where cruno='$cruno' and station=$station and longitude > 0;
exit;
">xxx.sql
sqlplus $Ousername/$password@ptran @xxx.sql
fi
echo " "
echo "        PLOT VALIDATION MODULE set for a Depths around 10 meters"
echo " "
echo "Enter Cruise number ?"
read cruno
echo "Enter Station Number ?"
read station
echo "
select min(sdate),max(sdate)
from losierr.adcpinf
where cruno='$cruno' and station=$station;
exit;
" > pppp.sql
sqlplus $Ousername/$password@ptran @pppp.sql
echo ""
echo ""
echo "Enter minimum date now "
read mindate
echo ""
echo ""
echo "Enter maximum date now "
read maxdate
echo "
set pages 100;
drop table aadcpplot;
create table aadcpplot as select
losierr.adcpplan.cruno,losierr.adcpplan.station,deploy,segment,depthsur,depthbot,
speed,direction,
veleast,velnorth,
unit_magvar,area_magvar
from losierr.adcpvel,losierr.adcpplan where
losierr.adcpvel.cruno='$cruno' and
losierr.adcpplan.cruno='$cruno' and
losierr.adcpplan.station=$station and
losierr.adcpvel.station=$station 
and losierr.adcpvel.depthbot >= 7.5 and losierr.adcpvel.depthbot < 8.5;
update aadcpplot
set area_magvar=(area_magvar*-1.0);
update aadcpplot
set direction=direction+360 where direction<area_magvar;
SET VERIFY OFF;
SET ECHO OFF;
SET FEEDBACK OFF;
SET HEADING OFF;
SET WRAP ON;
set trim on;
SET SHOW OFF;
SET VERIFY OFF;
SET PAGESIZE 0;
SET LINESIZE 80;
set numwidth 6;
set null NA;
SET TERMOUT OFF;
column depthbot format 999.99;
column depthsur format 999.99;
COLUMN SPEED  FORMAT 999.9;
COLUMN direction FORMAT 999;
column cordir format 999.9;
COLUMN heading FORMAT 999;
COLUMN temp FORMAT 999.9;
COLUMN longitude   FORMAT 999.99999;
COLUMN latitude    FORMAT 999.99999;
COLUMN tidehgt FORMAT 99.9;
column depthsur format 999.99;
column veleast FORMAT 999.9;
column velnorth FORMAT 999.9;
column velvert FORMAT 999.9;
column btveast  FORMAT 999.9;
column btvnorth FORMAT 999.9;
column btvert   FORMAT 999.9;
column btverr   FORMAT 999.9;
COLUMN mag FORMAT 999.9;
column area format a20;
column data_mode format a2;
column ddd format a20;
column depth format 9999.9;
column intensity format 9999.9;
column flow format a1;
SPOOL aadcpplot.lis;
SELECT deploy,I.segment,depthsur,depthbot,
to_char(sdate,'dd mm yyyy hh24 mi ss') ddd ,
speed,direction-area_magvar cordir
FROM losierr.adcpinf I,aadcpplot V
where depthbot >= 7.5 and depthbot < 8.5 and
I.cruno = V.cruno and
I.station = V.station and
I.segment = V.segment
order by I.segment;
EXIT;
" > axx.sql
sqlplus $Ousername/$password@ptran @axx.sql
echo "
x <- matrix(scan(\"aadcpplot.lis\"),ncol=12,byrow=T)
day <- x[,5]
mon <- x[,6]
yr <- x[,7]
hr <- x[,8]
minutes <- x[,9]
sec <- x[,10]
speed <- x[,11]
dir <- x[,12]
timeint <- c(rep(0.0,length(mon)))
timeint2 <- c(rep(0.0,length(mon)))
#initialize time interval period between sampling
vndel <- c(rep(0,length(mon)))
vedel <- c(rep(0,length(mon)))
dmy <- paste(day,mon,yr,sep=\"/\")
# concatenate my DMY together with a / separator to set into Splus format
HMS <- paste(hr,minutes,sec,sep=\":\")
# concatenate my HMS together with a : separator to set into Splus format
dmyHMS <- paste(dmy,HMS,sep=\" \")
mydate <- strptime(dmyHMS,\"%d/%m/%Y %H:%M:%S\")
# produce the vector mydate using the datetime function chron
Totaltime <- mydate[length(mon)] - mydate[1]
# Total amount of time for deployment in decimal days
ve <- sin(dir*2.0*pi/360.)*speed
vn <- cos(dir*2.0*pi/360.)*speed
# calculating vectors ve and vn from input matrix
timecor <- 0
timecor[1] <- 0
for(x in 2:length(day)){
timecor[x] <- difftime(mydate[x],mydate[x-1],units=\"mins\")}
timecor <- timecor*60
#convert to decimal seconds
firstx <- 2
lastx <- length(sec)
vndel[1] <- vn[1]*timecor[2]/100000
vedel[1] <- ve[1]*timecor[2]/100000
for(i in firstx:lastx){
 vndel[i] <- ((vn[i]*timecor[i]/100000)+vndel[i-1])
 vedel[i] <- ((ve[i]*timecor[i]/100000)+vedel[i-1])
 timeint[i] <- difftime(mydate[i],mydate[i-1],units=\"mins\")+timeint[i-1]
  }
timeint <- timeint/60/24
X11(display = \"\", width = 7, height = 9.5)
pdf(\"adcpplot.pdf\", height = 11.0, width = 8.5,pointsize=10)
a<-layout(matrix(c(1,2,7,7,6,6,5,5,4,4,3,3),6,2,byrow=T),
widths=lcm(c(8,8)), heights=lcm(c(8,2.8,2.8,2.8,2.8,2.8)))
#layout.show(a)
par(mar=c(3,4,2,2))
  #screen(1)
  maxdis <- max(vedel,vndel)
  mindis <- min(vedel,vndel)
  absdel <- max(abs(mindis),abs(maxdis))
  #computing x and y axis limits
  plot(vedel,vndel,type=\"l\",mgp=c(1.4,.25,0),
  xlab=\"Ve Kilometers\",ylab=\"Vn Kilometers\",
  xlim=c(-absdel,absdel),ylim=c(-absdel,absdel))
  segments(-absdel,0,absdel,0)
  segments(0,-absdel,0,absdel)

  #screen(2)
  maxven <- max(ve,vn)
  minven <- min(ve,vn)
  absven <- max(abs(minven),abs(maxven))
  #computing x and y axis limits
  vve2 <- 0
  vvn2 <- 0
  plot(vve2,vvn2,type='l',mgp=c(1.4,.25,0),lwd=.2,
  xlab=\"Ve cm/sec\",ylab=\"Vn cm/sec\",
  xlim=c(-absven,absven),ylim=c(-absven,absven))
  #initializes the plot area
  par(lwd=.2)
  segments(0,0,ve,vn)
  segments(-absven,0,absven,0)
  segments(0,-absven,0,absven)
  #segments allows line segments to a plot using polar coordinates
  #this plots the lines instantly instead of doing a line by line plot

  #screen(3)
  par(mar=c(2,2.2,0,.8))
  plot(mydate,speed,type=\"l\",xlab=\"Days\",ylab=\"Speed(cm/sec)\",mgp=c(1.1,.25,0),tcl=-.3)

  #screen(4)
  par(mar=c(1.5,2.2,0,.8))
  plot(mydate,vn,type=\"l\",mgp=c(1.4,.25,0),tcl=-.3,
  xaxt=\"n\",xlab=\" \",ylab=\"Vn\",ylim=c(-absven,absven))
  zero <- rep(0,length(ve))
  lines(mydate,zero)

  #screen(5)
  plot(mydate,ve,type=\"l\",mgp=c(1.4,.25,0),tcl=-.3,
  xaxt=\"n\",xlab=\" \",ylab=\"Ve\",ylim=c(-absven,absven))
  lines(mydate,zero)

  #screen(6)
  plot(mydate,dir,type=\"l\",mgp=c(1.4,.25,0),tcl=-.3,
  xaxt=\"n\",yaxt=\"n\",xlab=\" \",ylab=\"Direction\",ylim=c(0,360))
  axis(2,at=c(0,90,180,270,360),labels=c(\"0\",\"90\",\"180\",\"270\",\"360\"),las=2,tcl=.3,
  mgp=c(1.4,.25,0))

  #screen(7)
  duration <- max(timeint)
  ve2 <- c(rep(0,length(mon)))
  for(i in firstx:lastx){
   ve2[i] <- (((ve[i]*0.6/5.7)*(duration/maxven)) + timeint[i])}
  #ve is scaled by the axis lengths for proper vector lengths
  xlimx <- timeint[2]
  ylimx <- (timeint[2])+Totaltime
  plot(0,0,type='l',mgp=c(1.4,.25,0),tcl=-.3,
  ylim=c(-absven,absven),xlim=c(xlimx,ylimx),xaxt=\"n\",
  xlab=\" \",ylab=\"Velocity cm/sec\")
  par(lwd=.2)
  segments(timeint,0,ve2,vn)
  par(cex=1.5)
  title(main=\"$cruno station $station\",outer=T,line=-5.5,cex.main=1)
  title(sub=\"$mindate  to  $maxdate\",outer=T,line=-6,cex.sub=.7)
  dev.off()
" > aadcpplot.deck
rm aadcpplot.deck.Rout
R CMD BATCH aadcpplot.deck
echo ""
gimp adcpplot.pdf                       
mv adcpplot.pdf $cruno\_$deploy\_$station.pdf 
fi
rm adcpdepthrequest.sql
rm xxx2.sql
