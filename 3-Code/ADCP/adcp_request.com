echo "  This script will produce a plot of ADCP current data.  "
echo ""
echo "  Enter cruise and station for processing.  "
echo ""
echo "  Enter cruise number      "
echo ""
read cruno
echo ""
echo "  Enter station number    "
echo ""
read station
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
echo "
drop table adcpplot;
create table adcpplot as select
adcpplan.cruno,adcpplan.station,deploy,segment,depthsur,depthbot,
speed,direction,
veleast,velnorth,intensity,
unit_magvar,area_magvar
from adcpvel,adcpplan where
adcpvel.cruno='$cruno' and
adcpplan.cruno='$cruno' and
adcpplan.station=$station and
adcpvel.station=$station;
update adcpplot
set area_magvar=(area_magvar*-1.0);
update adcpplot
set direction=direction+360 where direction<area_magvar;
update adcpplot
set direction=direction-area_magvar;
SET VERIFY OFF;
SET ECHO OFF;
SET FEEDBACK OFF;
SET HEADING ON;
SET WRAP ON;
set trim on;
SET SHOW OFF;
SET VERIFY OFF;
SET PAGESIZE 49000;
SET LINESIZE 80;
set numwidth 6;
SET TERMOUT OFF;
column depthbot format 999.99;
column depthsur format 999.99;
COLUMN SPEED  FORMAT 999.9;
column seg format 99999;
COLUMN dir FORMAT 9999;
column cordir format 999.9;
column ve FORMAT 999.9;
column vn FORMAT 999.9;
COLUMN mag FORMAT 999.9;
column ddmmyyyyhhmiss format a20;
column depth format 9999.9;
column int format 9999.9;
SPOOL adcp$cruno\_$station.lis;
SELECT deploy,I.segment seg,depthsur,
to_char(sdate,'dd mm yyyy hh24 mi ss') ddmmyyyyhhmiss ,
speed,direction dir,intensity int,
sin(direction*2.0*3.141592654/360)*speed ve,
cos(direction*2.0*3.141592654/360)*speed vn
FROM adcpinf I,adcpplot V
where 
I.cruno = V.cruno and
I.station = V.station and
I.segment = V.segment
order by seg;
drop table adcpaverage;
create table adcpaverage as select deploy,I.segment seg,depthsur,
to_char(sdate,'dd mm yyyy hh24 mi ss') ddmmyyyyhhmiss ,
speed,direction dir,intensity int,
sin(direction*2.0*3.141592654/360)*speed ve,
cos(direction*2.0*3.141592654/360)*speed vn
FROM adcpinf I,adcpplot V
where
I.cruno = V.cruno and
I.station = V.station and
I.segment = V.segment
order by seg; 
drop table adcpaverdep;
update adcpaverage set ve=0.00001 where ve=0;
update adcpaverage set vn=0.00001 where vn=0;
create table adcpaverdep as select deploy,depthsur,avg(ve) avgve,avg(vn) avgvn,avg(speed) avgspd,avg(speed) vecspd,
avg(dir) avgdir 
from adcpaverage group by deploy,depthsur;
update adcpaverdep set avgdir = atan(AVGVE/AVGVN)*180/3.14159 where (avgve > 0 and avgvn > 0);
update adcpaverdep set avgdir = atan(AVGVE/AVGVN)*180/3.14159+360 where (avgve < 0 and avgvn > 0);
update adcpaverdep set avgdir = atan(AVGVE/AVGVN)*180/3.14159+180 where avgvn < 0;
update  adcpaverdep set vecspd=sqrt((avgve*avgve)+(avgvn*avgvn));
column avgve format 999.9;
column avgvn format 999.9;
column avgspd format 999.9;
column vecspd format 999.9;
column avgdir format 9999;
select * from adcpaverdep;          
drop table adcpaverall;
create table adcpaverall as select deploy,avg(ve) avgve,avg(vn) avgvn,avg(speed) avgspd,avg(speed) vecspd,
avg(dir) avgdir
from adcpaverage group by deploy;
update adcpaverall set avgdir = atan(AVGVE/AVGVN)*180/3.14159 where (avgve > 0 and avgvn > 0);
update adcpaverall set avgdir = atan(AVGVE/AVGVN)*180/3.14159+360 where (avgve < 0 and avgvn > 0);
update adcpaverall set avgdir = atan(AVGVE/AVGVN)*180/3.14159+180 where avgvn < 0;
update  adcpaverall set vecspd=sqrt((avgve*avgve)+(avgvn*avgvn));
select * from adcpaverall;
exit;
" > adcprequest.sql
sqlplus $Ousername/$password@ptran @check.sql
