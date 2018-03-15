echo ""
echo ""
echo "  This script will produce a table of ADCP current data.  "
echo "  Rename      adcpdata.lis      to match your request.    "
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
echo "  Do you want depth selection relative to the bottom or surface b/s? "
read botsur
case $botsur in
 [bB]) botsur=depthbot;;
 [sS]) botsur=depthsur;;
esac
echo "  Do you want to select a range of segments y/n? "                   
read ensyn 
if [ "$ensyn" = "Y" -o "$ensyn" = "y" ]
then
echo "
select min(segment),max(segment)
from adcpinf
where cruno='$cruno' and station=$station;
exit;
" > ppp.sql
sqlplus losierr/rmtj01 @ ppp.sql
echo ""
echo ""
echo "Enter minimum segment number now "
read minensemble
echo ""
echo ""
echo "Enter maximum segment number now "
read maxensemble
fi
echo ""
echo "
set pages 100;
select distinct($botsur), count($botsur) from adcpvel
where adcpvel.cruno='$cruno' and
adcpvel.station=$station
group by $botsur  order by $botsur ;
ACCEPT DEPTH NUMBER PROMPT 'ENTER DEPTH ? '
drop table adcpplot;
create table adcpplot as select
adcpplan.cruno,adcpplan.station,deploy,segment,depthsur,depthbot,
speed,direction,
veleast,velnorth,
unit_magvar,area_magvar
from adcpvel,adcpplan where
adcpvel.cruno='$cruno' and
adcpplan.cruno='$cruno' and
adcpplan.station=$station and
adcpvel.station=$station
and adcpvel.$botsur=&DEPTH;
update adcpplot
set area_magvar=(area_magvar*-1.0);
update adcpplot
set direction=direction+360 where direction<area_magvar;
update adcpplot
set direction=direction-area_magvar;
SET VERIFY OFF;
SET ECHO OFF;
SET FEEDBACK OFF;
SET HEADING OFF;
SET WRAP ON;
set trim on;
SET SHOW OFF;
SET VERIFY OFF;
SET PAGESIZE 0;
SET LINESIZE 99;
set numwidth 6;
set null NA;
SET TERMOUT OFF;
column depthbot format 999.99;
column depthsur format 999.99;
COLUMN SPEED  FORMAT 999.9;
COLUMN direction FORMAT 999.9;
COLUMN ve FORMAT 999.99;
COLUMN vn FORMAT 999.99;
column cordir format 999.9;
COLUMN heading FORMAT 999;
COLUMN temp FORMAT 999.9;
COLUMN longitude   FORMAT 999.99999;
COLUMN latitude    FORMAT 999.99999;
COLUMN tidehgt FORMAT 99.9;
column depthsur 999.99;
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
SPOOL adcpdata.lis;
SELECT deploy,I.segment,depthsur,depthbot,
to_char(sdate,'dd mm yyyy hh24 mi ss') ddd ,
speed,direction,
sin(direction*2.0*3.141592654/360)*speed ve,
cos(direction*2.0*3.141592654/360)*speed vn
FROM adcpinf I,adcpplot V
where $botsur=&depth and
I.cruno = V.cruno and
I.station = V.station and
I.segment = V.segment " > xx.sql
if [ "$ensyn" = "Y" -o "$ensyn" = "y" ]
then
echo " and I.segment >= $minensemble and I.segment <= $maxensemble" >> xx.sql
fi
echo " order by I.segment;
EXIT;
" >> xx.sql
sqlplus losierr/rmtj01 @xx.sql
