oshkosh = LOAD 'hdfs:/home/ubuntu/final/Oshkosh/OshkoshWeather.csv' using PigStorage(',');
real_oshkosh = FILTER oshkosh BY $1 >= 1 AND $1 <= 12 AND $2 >= 1 AND $2 <= 31 AND $4 > -100 AND $4 < 200;
oshkosh_data = FOREACH real_oshkosh GENERATE $0 AS year, $1 AS month, $2 AS day, 
   SUBSTRING((chararray)$3,0,INDEXOF((chararray)$3,':',0)) AS hour,
   SUBSTRING((chararray)$3,INDEXOF((chararray)$3,':',0)+1,INDEXOF((chararray)$3,':',0)+3) AS minute, 
   SUBSTRING((chararray)$3,INDEXOF((chararray)$3,' ',0)+1,INDEXOF((chararray)$3,' ',0)+3) AS ampm, 
   (float)$4 AS tempF, (float)$11 AS windMPH, 'Oshkosh' AS city;

iowacity = LOAD 'hdfs:/home/ubuntu/final/IowaCity/IowaCityWeather.csv' using PigStorage(',');
real_iacity = FILTER iowacity BY $1 >= 1 AND $1 <= 12 AND $2 >= 1 AND $2 <= 31 AND $4 > -100 AND $4 < 200;
iacity_data = FOREACH real_iacity GENERATE $0 AS year, $1 AS month, $2 AS day, 
   SUBSTRING((chararray)$3,0,INDEXOF((chararray)$3,':',0)) AS hour,
   SUBSTRING((chararray)$3,INDEXOF((chararray)$3,':',0)+1,INDEXOF((chararray)$3,':',0)+3) AS minute, 
   SUBSTRING((chararray)$3,INDEXOF((chararray)$3,' ',0)+1,INDEXOF((chararray)$3,' ',0)+3) AS ampm, 
   (float)$4 AS tempF, (float)$11 AS windMPH, 'Iowa City' as city;

mergedata = UNION oshkosh_data, iacity_data;
groupeddata = GROUP mergedata BY (city, hour, ampm);
averagedata = FOREACH groupeddata GENERATE group, AVG(mergedata.tempF) as tempF, AVG(mergedata.windMPH) as windMPH;
averagedata1 = FOREACH averagedata GENERATE FLATTEN(group), tempF, windMPH, ABS(50-tempF) AS tempdiff;
averagedata2 = GROUP averagedata1 ALL;
mindiff = FOREACH averagedata2 GENERATE MIN(averagedata1.tempdiff) AS mintempdiff;
averagedata3 = JOIN averagedata1 BY tempdiff, mindiff by mintempdiff;
averagedata4 = FOREACH averagedata3 GENERATE $0 AS city, $1 AS hour, $2 AS ampm, $3 AS tempF, $4 AS windMPH, $5 AS tempdiff;

leastwindy = GROUP averagedata4 ALL;
minwind = FOREACH leastwindy GENERATE MIN(averagedata4.windMPH) AS minwindMPH;
leastwindy2 = JOIN averagedata4 BY windMPH, minwind by minwindMPH;
leastwindy3 = FOREACH leastwindy2 GENERATE city, SPRINTF('%d %s', (int)hour, ampm);
DUMP leastwindy3;
