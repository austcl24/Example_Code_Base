oshkosh = LOAD 'hdfs:/home/ubuntu/final/Oshkosh/OshkoshWeather.csv' using PigStorage(',');
real_oshkosh = FILTER oshkosh BY $1 >= 1 AND $1 <= 12 AND $2 >= 1 AND $2 <= 31 AND $4 > -100 AND $4 < 200;
oshkosh_data = FOREACH real_oshkosh GENERATE $0 AS year, $1 AS month, $2 AS day, (chararray)$3 AS time, (float)$4 AS tempF;

oshkosh_data1 = FOREACH oshkosh_data GENERATE year, month, day, SUBSTRING(time,0,INDEXOF(time,':',0)) AS hour, SUBSTRING(time,INDEXOF(time,' ',0)+1,INDEXOF(time,' ',0)+3) AS ampm, tempF;
group1 = GROUP oshkosh_data1 BY (year, month, day, hour, ampm);
groupavg = FOREACH group1 GENERATE group, AVG(oshkosh_data1.tempF) AS avgtemp;
groupavg1 = FOREACH groupavg GENERATE FLATTEN(group.(year, month, day, hour, ampm)), avgtemp;
groupavg1a = FOREACH groupavg1 GENERATE $0 as year, $1 as month, $2 as day, $3 as hour, $4 as ampm, $5 as avgtemp;

group2 = GROUP groupavg1a BY (year, month, day);
group2min = FOREACH group2 GENERATE group.year AS year, group.month AS month, group.day AS day, MIN(groupavg1a.avgtemp) AS mintemp;

joineddata = JOIN groupavg1a BY (year, month, day, avgtemp), group2min BY (year, month, day, mintemp);
joineddata1 = FOREACH joineddata GENERATE $0 as year, $1 as month, $2 AS day, SPRINTF('%s %s', $3,$4) AS hourly, $5 AS mintemp;

byhour = GROUP joineddata1 BY hourly;
byhour1 = FOREACH byhour GENERATE group, COUNT(joineddata1.hourly) AS hourlycount;
byhour2 = ORDER byhour1 BY hourlycount DESC;
byhour3 = LIMIT byhour2 1;
byhour4 = FOREACH byhour3 GENERATE group;
DUMP byhour4;


