oshkosh = LOAD 'hdfs:/home/ubuntu/final/Oshkosh/OshkoshWeather.csv' using PigStorage(',');
real_oshkosh = FILTER oshkosh BY $1 >= 1 AND $1 <= 12 AND $2 >= 1 AND $2 <= 31 AND $4 > -100 AND $4 < 200;
oshkosh_data = FOREACH real_oshkosh GENERATE $0 AS year, $1 AS month, $2 AS day, 
   SUBSTRING((chararray)$3,0,INDEXOF((chararray)$3,':',0)) AS hour,
   SUBSTRING((chararray)$3,INDEXOF((chararray)$3,':',0)+1,INDEXOF((chararray)$3,':',0)+3) AS minute, 
   SUBSTRING((chararray)$3,INDEXOF((chararray)$3,' ',0)+1,INDEXOF((chararray)$3,' ',0)+3) AS ampm, 
   (float)$4 AS tempF;

oshkosh_data1 = FOREACH oshkosh_data GENERATE ToDate(CONCAT(year, '/', month, '/', day),'yyyy/MM/dd') AS ymd1, 
ToDate(CONCAT(year, '/', month, '/', day, ' ', hour, ':', minute, ' ', ampm),'yyyy/MM/dd hh:mm aa') AS timestamp, tempF;

oshkosh_data1a = FOREACH oshkosh_data1 GENERATE ymd1, AddDuration(ymd1, 'P1D') AS ymd2, timestamp, tempF;
group1 = GROUP oshkosh_data1a BY ymd1;
group2 = GROUP oshkosh_data1a BY ymd2;
uniongroup =  UNION group2, group1;
uniongroup1 = FOREACH uniongroup GENERATE $0 as ymd, FLATTEN(oshkosh_data1a); 
uniongroup2 = FOREACH uniongroup1 GENERATE $0 as ymd, $3 as timestamp, $4 As tempF;
group3 = GROUP uniongroup2 BY ymd;
minmax = FOREACH group3 GENERATE group, MIN(uniongroup2.tempF) as mintempF, MAX(uniongroup2.tempF) as maxtempF;
minmaxjoin = JOIN group3 BY group, minmax by group; 
flatjoin = FOREACH minmaxjoin GENERATE $0, FLATTEN($1), $2, $3, $4;
flatjoin1 = FOREACH flatjoin GENERATE $0 as ymd, $2 as timestamp, $3 as tempF, $5 as minTempF, $6 as maxTempF;
flatjoin2c = FILTER flatjoin1 BY tempF == minTempF OR tempF == maxTempF;
flatjoin2c2 = FILTER flatjoin1 BY tempF == minTempF OR tempF == maxTempF;
flatjoin3 = JOIN flatjoin2c by ymd, flatjoin2c2 by ymd;
flatjoin4 = FOREACH flatjoin3 GENERATE $1 AS begintime, $6 AS endtime, ABS($2 - $7) AS difftemp, MinutesBetween($6, $1) as diffminutes, $8 as mintemp, $9 as maxtemp, 'Oshkosh' AS city; 
flatjoin5 = FILTER flatjoin4 BY diffminutes > 0 and diffminutes < (24*60);
flatjoin6 = ORDER flatjoin5 BY difftemp DESC;
maxgroup = GROUP flatjoin6 ALL;
maxtemp = FOREACH maxgroup GENERATE MAX(flatjoin6.difftemp) AS maxtempdiff;
maxgroup2 = JOIN flatjoin6 by difftemp, maxtemp by maxtempdiff;
maxgroup3 = ORDER maxgroup2 BY diffminutes;
maxgroup4 = LIMIT maxgroup3 1;

-- *********************************************************************

iowacity = LOAD 'hdfs:/home/ubuntu/final/IowaCity/IowaCityWeather.csv' using PigStorage(',');
real_iacity = FILTER iowacity BY $1 >= 1 AND $1 <= 12 AND $2 >= 1 AND $2 <= 31 AND $4 > -100 AND $4 < 200;
iacity_data = FOREACH real_iacity GENERATE $0 AS year, $1 AS month, $2 AS day, 
   SUBSTRING((chararray)$3,0,INDEXOF((chararray)$3,':',0)) AS hour,
   SUBSTRING((chararray)$3,INDEXOF((chararray)$3,':',0)+1,INDEXOF((chararray)$3,':',0)+3) AS minute, 
   SUBSTRING((chararray)$3,INDEXOF((chararray)$3,' ',0)+1,INDEXOF((chararray)$3,' ',0)+3) AS ampm, 
   (float)$4 AS tempF;

iacity_data1 = FOREACH iacity_data GENERATE ToDate(CONCAT(year, '/', month, '/', day),'yyyy/MM/dd') AS ymd1, 
ToDate(CONCAT(year, '/', month, '/', day, ' ', hour, ':', minute, ' ', ampm),'yyyy/MM/dd hh:mm aa') AS timestamp, tempF;

iacity_data1a = FOREACH iacity_data1 GENERATE ymd1, AddDuration(ymd1, 'P1D') AS ymd2, timestamp, tempF;
group1ia = GROUP iacity_data1a BY ymd1;
group2ia = GROUP iacity_data1a BY ymd2;
uniongroupia =  UNION group2ia, group1ia;
uniongroup1ia = FOREACH uniongroupia GENERATE $0 as ymd, FLATTEN(iacity_data1a); 
uniongroup2ia = FOREACH uniongroup1ia GENERATE $0 as ymd, $3 as timestamp, $4 As tempF;
group3ia = GROUP uniongroup2ia BY ymd;
minmaxia = FOREACH group3ia GENERATE group, MIN(uniongroup2ia.tempF) as mintempF, MAX(uniongroup2ia.tempF) as maxtempF;
minmaxjoinia = JOIN group3ia BY group, minmaxia by group; 
flatjoinia = FOREACH minmaxjoinia GENERATE $0, FLATTEN($1), $2, $3, $4;
flatjoin1ia = FOREACH flatjoinia GENERATE $0 as ymd, $2 as timestamp, $3 as tempF, $5 as minTempF, $6 as maxTempF;
flatjoin2cia = FILTER flatjoin1ia BY tempF == minTempF OR tempF == maxTempF;
flatjoin2c2ia = FILTER flatjoin1ia BY tempF == minTempF OR tempF == maxTempF;
flatjoin3ia = JOIN flatjoin2cia by ymd, flatjoin2c2ia by ymd;
flatjoin4ia = FOREACH flatjoin3ia GENERATE $1 AS begintime, $6 AS endtime, ABS($2 - $7) AS difftemp, MinutesBetween($6, $1) as diffminutes, $8 as mintemp, $9 as maxtemp, 'IowaCity' AS city; 
flatjoin5ia = FILTER flatjoin4ia BY diffminutes > 0 and diffminutes < (24*60);
flatjoin6ia = ORDER flatjoin5ia BY difftemp DESC;
maxgroupia = GROUP flatjoin6ia ALL;
maxtempia = FOREACH maxgroupia GENERATE MAX(flatjoin6ia.difftemp) AS maxtempdiff;
maxgroup2ia = JOIN flatjoin6ia by difftemp, maxtempia by maxtempdiff;
maxgroup3ia = ORDER maxgroup2ia BY diffminutes;
maxgroup4ia = LIMIT maxgroup3ia 1;

-- ********************************************************
final = UNION maxgroup4, maxgroup4ia;
final1 = FOREACH final generate $0 AS begintime, $1 AS endtime, $2 AS difftemp, $3 AS diffminutes, $4 AS mintemp, $5 AS maxtemp, $6 as city;
finalgroup = GROUP final1 ALL;
finalgroup1 = FOREACH finalgroup GENERATE MAX(final1.difftemp) AS maxtempdiffall;
finalgroup2 = JOIN final1 by difftemp, finalgroup1 by maxtempdiffall;
finalgroup3 = FOREACH finalgroup2 GENERATE city, SPRINTF('%4.2f degrees',difftemp) AS outtemp,  SPRINTF('%s to %s', ToString(begintime, 'MMMMMMMM dd,yyyy hh:mmaa'), ToString(endtime, 'MMMMMMMM dd,yyyy hh:mmaa')) AS outtime;
DUMP finalgroup3;




