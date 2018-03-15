rm std00001.dat
echo " "
echo "......................................................."
echo "      This Proceedure loads tables adcpplan,adcpinf,   "
echo "      adcpvel and nav_raw.                             " 
echo "......................................................."
echo "
drop table adcpplan;
create table adcpplan
      (deploy number (5),cruno varchar2 (5),station number (4),serial number (5),
      unitkhz number (5), unit_magvar number (6,1), area_magvar number (6,1),
      binsize number (6,2), tranblnk number (7,2), mount_depth number (7,2),
      bincount number (7,2),rawpings number (5),ping_interval number (6,1),
      segment_time number (6,1),measure varchar2 (3),depth_ref varchar2 (3),
      data_mode varchar2 (1), area varchar2 (20),morenote varchar2 (80));
quit;
">adcpplan.sql
#remsh quoddy "(ORACLE_SID=sabs;ORAENV_ASK=NO;export ORACLE_SID ORAENV_ASK;. /usr/local/bin/oraenv;cd transect_process;sqlplus losier/losier @adcpplan.sql)"
rm adcpplan.sql
echo "
drop table adcpvel;
create table adcpvel
      (cruno varchar2 (5),station number (4),segment number (5),
      depthbot number (6,2), depthsur number (6,2), speed number (6,2),
      direction number (6,1), timedec number (8,4), Veleast number (6,1),
      Velnorth number (6,1),Velvert number (6,1),Velerror number (6,1),
      intensity number (7,1));
quit;
">adcpvel.sql
#remsh quoddy "(ORACLE_SID=sabs;ORAENV_ASK=NO;export ORACLE_SID ORAENV_ASK;. /usr/local/bin/oraenv;cd transect_process;sqlplus losier/losier @adcpvel.sql)"
rm adcpvel.sql
echo "
drop table adcpinf;
create table adcpinf
      (cruno varchar2 (5),station number (4),segment number (5),
      sdate date, 
      Depth number (6,2),pitch number (6,2),roll number (6,2),
      heading number (7,2),latitude number (9,5), longitude number (9,5),
      tidehgt number (5,2), flow varchar2 (1),temp number (6,2), btveast number (6,2),
      btvnorth number (6,2),btvert number (6,2),btverr number (6,2));
quit;
">adcpinf.sql
#remsh quoddy "(ORACLE_SID=sabs;ORAENV_ASK=NO;export ORACLE_SID ORAENV_ASK;. /usr/local/bin/oraenv;cd transect_process;sqlplus losier/losier @adcpinf.sql)"
rm adcpinf.sql
echo "
drop table nav_raw; 
create table nav_raw
      (cruno varchar2 (5),station number (4),
      sdate date,
      latitude number (8,5), longitude number (8,5));
quit;
">adcpnav.sql
#remsh quoddy "(ORACLE_SID=sabs;ORAENV_ASK=NO;export ORACLE_SID ORAENV_ASK;. /usr/local/bin/oraenv;cd transect_process;sqlplus losier/losier @adcpnav.sql)"
rm adcpnav.sql
rm navfile.load
echo " "
echo "....................................................................."
echo "....................................................................."
echo " "
echo "      This  Load  Requires  that  certain  procedures were followed."
echo "      ADCP and RDI data must be process using TRANSECT and turning  "
echo "      on the ASCIIOUT parameter. The output file can be transfered  "
echo "      to wolves using dos2unix and then processed using this script."
echo " "
echo "....................................................................."
echo "....................................................................."
echo " "
echo " "
ls *.TXT
ls *.txt
ls *.000
ls *.adcp
echo "Enter Transect Asciiout output filename ?"
read filename
/usr/local/bin/dos2unix $filename
echo $filename > transect.bat
echo $filename > transect.bat2
echo "Enter RDI Serial Number 150,222,223,382 ?"
read serial
echo $serial >> transect.bat
echo $serial >> transect.bat2
echo "Enter Consecutive deployment number ?"
read deploy
echo $deploy >> transect.bat
echo $deploy >> transect.bat2
if [ $serial = 150 ]
 then
 tmode=T
fi
if [ $serial = 222 ]
 then
 tmode=M
fi
if [ $serial = 223 ]
 then
 echo "Was RDI $serial in TRANSECT OR MOORED mode?"
 echo "Enter T or M now"
 read tmode
 case $tmode in 
     [Tt]) tmode=T;;
     [Mm]) tmode=M;;
 esac
fi
if [ $serial = 382 ]
 then
 echo "Was RDI $serial in TRANSECT OR MOORED mode?"
 echo "Enter T or M now"
 read tmode
 case $tmode in 
     [Tt]) tmode=T;;
     [Mm]) tmode=M;;
 esac
fi
echo "Enter Cruise number ?"
read cruno    
echo $cruno >> transect.bat
echo $cruno >> transect.bat2
echo "Enter Station Number ?"
read station 
echo $station >> transect.bat
echo $station >> transect.bat2
echo "Enter Local Magnetic Variation ?"
read lmagvar
echo $lmagvar >> transect.bat
echo $lmagvar >> transect.bat2
echo "Enter Transect Magnetic Variation setup ?"
read magvar
echo $magvar >> transect.bat
echo $magvar >> transect.bat2
echo "Enter Depth adjustment of transducer head in meters ?"
echo "Enter 0 if not Known."
read dadj    
echo $dadj >> transect.bat
echo $dadj >> transect.bat2
echo "Was the RDI moored facing down in High Resolution Mode y/n? "
read hryn
 case $hryn in
     [Yy]) hryn=Y;;
 esac
echo $hryn >> transect.bat
echo $hryn >> transect.bat2
if [ $serial = 150 ]
 then
 echo "Enter year of deployment 96 or 97 ?"
 echo "If unsure check with the raw navigation file"
 read yyear
 echo $yyear >> transect.bat
fi
echo deployment mode=$tmode
if [ $tmode = M ]
 then
 echo "Enter deployment Latitude and Longitude now ?"
 read lat long
 echo $lat,$long >> transect.bat
 echo $lat,$long >> transect.bat2
fi
if [ $tmode = M ]
 then
echo " "
echo " "
echo "       Shallow moorings can generate echo intensities beyond       "
echo "       the actual depth of the current meter.  This is caused    "
echo "       by sound waves bouncing off the water surface.  A maximum   "
echo "       possible depth value for a particular mooring can eliminate "
echo "       this problem.  To determine this value, run the program     "
echo "       once using a value of 0, and then determine the actual      "
echo "       value, if necessary, once the data is processed.  Rerun     "
echo "       the script using the suggested value.                       "
echo " "
echo "     Enter the maximum depth for this deployment                 "
echo "     Use 0 for the first run, or if unknown.                     "
echo "                                                                 "
echo "     Do not use this adjustment in high resolution               "
echo "        downfacing mode                                         "
echo " "
read echodepth
#let echodepth=$echodepth-2
echo "$echodepth    "
echo $echodepth >> transect.bat
fi
echo y >> transect.bat
echo 9 >> transect.bat
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
echo "    65    Saint John                                         "
echo "    1915  Rustico PEI                                        "
echo "............................................................."
echo "............................................................."
echo "Enter Foreman Tidal Constituent Station number?"
read foreman 
echo $foreman  >> transect.bat
transect_asciiout < transect.bat
echo "Do you want to re-run this program with maximum depth limitation? "
echo "y/n   \c      "
read reply
if [ "$reply" = "y" ]
then
echo " "
echo "Enter the maximum depth for this deployment now              "
echo "and the program will reprocess the datafile using this       "
echo "value.                                                       "
echo " "
read echodepth2
#let echodepth2=$echodepth2-2
echo $echodepth2 >> transect.bat2
echo y >> transect.bat2
echo 9 >> transect.bat2
echo $foreman  >> transect.bat2
transect_asciiout < transect.bat2
fi
#rm transect.bat
echo " "
echo " "
echo 'Enter geographic area of deployment? \c '
read area
#area='Passamaquoddy Bay'
echo 'Enter any comments you want entered on the database? \c '
read addcom         
#addcom='Ministers Island site remediation with Randy and Fred'
echo "
select max(deploy),max(station) from adcpplan
where data_mode='$tmode';
exit;
">update.sql
sqlplus losierr/rmtj01 @update.sql
echo " "
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
echo "     edited RDI data to adcp oracle database."
echo "......................................................."
echo " "
echo "Loading ADCP deployment $deploy for cruise $cruno and station $station \c"; date
echo " "
echo " This will delete all ADCP deployments $deploy cruise $cruno and station $station from the database "
echo " Are you sure you want to proceed with the load y/n ? /c"
read answer
if [ "$answer" = "y" ]
then
echo "
 -- cleaning out Deployment $cruno from ORACLE DATABASE
 delete from adcpplan where cruno='$cruno' and station=$station and deploy=$deploy;
 delete from adcpinf where cruno='$cruno' and station=$station;
 delete from adcpvel where cruno='$cruno' and station=$station;
 delete from nav_raw where cruno='$cruno' and station=$station; 
 exit;
 ">clean.sql
sqlplus losierr/rmtj01 @clean.sql
rm clean.sql
echo "
-- Loading adcpplan data info for deployment $deploy into ORACLE
LOAD DATA
INFILE rtrandep.dat
-- INFILE SPECIFIES THE DATA TO BE LOADED
APPEND INTO TABLE adcpplan          
(deploy        POSITION(01:05) INTEGER EXTERNAL,
 cruno         POSITION(06:10) char,
 station       POSITION(11:14) INTEGER EXTERNAL,             
 serial        POSITION(15:19) INTEGER EXTERNAL,                 
 unitkhz       POSITION(20:24) INTEGER EXTERNAL,                     
 unit_magvar   POSITION(31:36) DECIMAL EXTERNAL,
 area_magvar   POSITION(25:30) DECIMAL EXTERNAL,
 binsize       POSITION(37:42) DECIMAL EXTERNAL,          
 tranblnk      POSITION(43:49) DECIMAL EXTERNAL,
 mount_depth   POSITION(50:56) DECIMAL EXTERNAL,                 
 bincount      POSITION(57:63) DECIMAL EXTERNAL,                 
 rawpings      POSITION(64:68) integer EXTERNAL,                 
 ping_interval POSITION(69:73) decimal EXTERNAL,                 
 segment_time  POSITION(74:80) decimal EXTERNAL,                 
 measure       POSITION(81:83) char,                             
 depth_ref     POSITION(84:86) char,                         
 data_mode     POSITION(87:87) char,                             
 area          POSITION(89:108) char,                             
 morenote      POSITION(109:188) char)                             
 ">adcpplan.ctl
sqlldr losierr/rmtj01 adcpplan.ctl,adcpplan.log,adcpplan.bad
more -20 -d adcpplan.log
rm adcpp*.ctl
rm adcpp*.bad
rm adcpp*.log
echo  "update adcpplan set area= '$area' where cruno = '$cruno' and station = $station;" > update.sql
echo  "update adcpplan set morenote= '$addcom' where cruno = '$cruno' and station = $station;" >> update.sql
echo "exit;" >> update.sql
sqlplus losierr/rmtj01 @update.sql
rm update.sql
echo "
-- Loading adcpvel data info for $1 into ORACLE
LOAD DATA
INFILE rtranvel.dat
APPEND INTO TABLE adcpvel          
(cruno         POSITION(01:05) char,
 station       POSITION(06:09) INTEGER EXTERNAL,             
 segment       POSITION(10:14) INTEGER EXTERNAL,                 
 depthbot      POSITION(15:20) DECIMAL EXTERNAL,                     
 depthsur      POSITION(21:26) DECIMAL EXTERNAL,
 speed         POSITION(27:32) DECIMAL EXTERNAL,
 direction     POSITION(33:38) DECIMAL EXTERNAL,          
 timedec       POSITION(39:46) DECIMAL EXTERNAL,
 Veleast       POSITION(47:53) DECIMAL EXTERNAL,                 
 Velnorth      POSITION(54:60) DECIMAL EXTERNAL,                 
 Velvert       POSITION(61:67) DECIMAL EXTERNAL,                 
 Velerror      POSITION(68:74) DECIMAL EXTERNAL,                 
 intensity     POSITION(75:81) DECIMAL EXTERNAL)
 ">adcpvel.ctl
sqlldr losierr/rmtj01 adcpvel.ctl,adcpvel.log,adcpvel.bad
more -20 -d adcpvel.log
if [ $hryn = Y ]
 then
echo  "
update adcpvel set depthsur=null where cruno='$cruno'
and station=$station;
update adcpinf set depth=null where cruno='$cruno'
and station=$station;
exit;
">update2.sql
sqlplus losierr/rmtj01 @ update2.sql
rm update2.sql
fi
rm adcpv*.ctl
rm adcpv*.bad
rm adcpv*.log
echo "
-- Loading adcpinf data info for $1 into ORACLE
LOAD DATA
INFILE rtraninf.dat
-- INFILE SPECIFIES THE DATA TO BE LOADED
APPEND INTO TABLE adcpinf          
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
sqlldr losierr/rmtj01 adcpinf.ctl,adcpinf.log,adcpinf.bad
more -20 -d adcpinf.log
rm adcpinf.ctl
rm adcpinf.log
rm adcpinf.bad
mv $filename $cruno\_$deploy.adcp
echo " "
fi
#exit
echo " "
echo "....................................................................."
echo "....................................................................."
echo " "
echo "      This  Load  Requires  that  certain  procedures were followed."
echo "      Navigation files from Transect collection can be transfered   "
echo "      to wolves using dos2unix and then processed using this script."
echo " "
echo "....................................................................."
echo "....................................................................."
echo " "
echo " "
echo "....................................................................."
echo "....................................................................."
echo " Enter navigation file for loading *WITHOUT* extension. File should  "
echo "                     be on the list below.                           "
echo "....................................................................."
echo "....................................................................."
echo "   If you are re-loading a deployment, navigation files will have a  "
echo "     .nav extension. Enter full file name in this case.              "
echo "....................................................................."
echo "....................................................................."
echo "   If you are loading a moored deployment, there is no navigation    "
echo "              data.  Enter ctrl c in this case.                   "
echo "....................................................................."
echo "....................................................................."
echo " "
echo " "
rm rfmt_nav.dat
ls *N.000
ls *n.000
ls *.nav
echo " "
echo " "
read navfile
cat $navfile* > navfile.load
echo 'navfile.load' > xxx.scr
echo $cruno >> xxx.scr
echo $station >> xxx.scr
nema_rfmt < xxx.scr         
echo "
-- Loading Navigation data into ORACLE
-- THIS IS AN EXAMPLE OF A COMMENT LINE IN SQL*LOADER
-- THIS PROGRAM ALLOWS YOU TO MOVE EXTERNAL FILES INTO
-- TABLES IN AN ORACLE DATABASE
LOAD DATA
-- REQUIRED AT THE BEGINNING OF THE CONTROL FILE
INFILE rfmt_nav.dat
-- INFILE SPECIFIES THE DATA TO BE LOADED
-- INFILE *    -THE DATA IS FOUND WITHIN THIS COMMAND FILE AFTER
-- COMMAND WORD BEGINDATA.
APPEND INTO TABLE nav_raw          
(cruno         POSITION(01:05) char,
 station       POSITION(06:10) INTEGER EXTERNAL,             
 sdate         POSITION(11:30) date 'ddmmyyyyhh24miss',
 latitude      POSITION(31:40) DECIMAL EXTERNAL,                 
 longitude     POSITION(41:50) DECIMAL EXTERNAL)                 
 ">nav.ctl
sqlldr losierr/rmtj01 nav.ctl,nav.log,nav.bad
more -20 -d nav.log
mv navfile.load $cruno\_$deploy.nav
rm $navfile*
rm nav.ctl
rm nav.log
rm nav.bad
echo "
column sdate format date;
set echo off;
set numwidth 7;
column min(to_char(adcpinf.sdate,'dd-Mon-yyyy,hh24:mi:ss')) heading 'ADCP Start Date' format a25;
column min(to_char(nav_raw.sdate,'dd-Mon-yyyy,hh24:mi:ss')) heading 'Nav Start Date' format a25;
set linesize 152;
spool checklist.dat
select adcpinf.cruno, adcpinf.station,
min(to_char(adcpinf.sdate,'dd-Mon-yyyy,hh24:mi:ss')),
min(to_char(nav_raw.sdate,'dd-Mon-yyyy,hh24:mi:ss'))
from
adcpinf,nav_raw
where adcpinf.cruno='$cruno' and 
adcpinf.station=$station and 
nav_raw.cruno='$cruno' and
nav_raw.station=$station
group by adcpinf.cruno, adcpinf.station;
exit;
">update.sql
#sqlplus losierr/rmtj01 @update.sql
more checklist.dat
