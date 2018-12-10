oshkosh = LOAD 'hdfs:/home/ubuntu/final/Oshkosh/OshkoshWeather.csv' using PigStorage(',');
real_oshkosh = FILTER oshkosh BY $1 >= 1 AND $1 <= 12 AND $2 >= 1 AND $2 <= 31 AND $4 > -100 AND $4 < 200;
oshkosh_data = FOREACH real_oshkosh GENERATE $0 AS year, $1 AS month, $2 AS day, $3 AS time, (float)$4 AS tempF;
group1 = GROUP oshkosh_data BY (year, month, day);
groupsum = FOREACH group1 GENERATE group, ToDate(CONCAT(group.year, '/', group.month, '/', group.day),'yyyy/MM/dd') AS ymd, COUNT(oshkosh_data.tempF) AS readings, SUM(oshkosh_data.tempF) AS sumtemp;
groupsum2 = FOREACH groupsum GENERATE ymd,readings, sumtemp;

segment0 = FOREACH groupsum GENERATE ymd as keyval, AddDuration(ymd, 'P0D') AS ymd;
segment0j = JOIN segment0 BY ymd, groupsum2 BY ymd;
segment0j1 = FOREACH segment0j GENERATE keyval as keyval, readings as readings, sumtemp as sumtemp;
segment1 = FOREACH groupsum GENERATE ymd as keyval, AddDuration(ymd, 'P1D') AS ymd;
segment1j = JOIN segment1 BY ymd, groupsum2 BY ymd;
segment1j1 = FOREACH segment1j GENERATE keyval as keyval, readings as readings, sumtemp as sumtemp;
segment2 = FOREACH groupsum GENERATE ymd as keyval, AddDuration(ymd, 'P2D') AS ymd;
segment2j = JOIN segment2 BY ymd, groupsum2 BY ymd;
segment2j1 = FOREACH segment2j GENERATE keyval as keyval, readings as readings, sumtemp as sumtemp;
segment3 = FOREACH groupsum GENERATE ymd as keyval, AddDuration(ymd, 'P3D') AS ymd;
segment3j = JOIN segment3 BY ymd, groupsum2 BY ymd;
segment3j1 = FOREACH segment3j GENERATE keyval as keyval, readings as readings, sumtemp as sumtemp;
segment4 = FOREACH groupsum GENERATE ymd as keyval, AddDuration(ymd, 'P4D') AS ymd;
segment4j = JOIN segment4 BY ymd, groupsum2 BY ymd;
segment4j1 = FOREACH segment4j GENERATE keyval as keyval, readings as readings, sumtemp as sumtemp;
segment5 = FOREACH groupsum GENERATE ymd as keyval, AddDuration(ymd, 'P5D') AS ymd;
segment5j = JOIN segment5 BY ymd, groupsum2 BY ymd;
segment5j1 = FOREACH segment5j GENERATE keyval as keyval, readings as readings, sumtemp as sumtemp;
segment6 = FOREACH groupsum GENERATE ymd as keyval, AddDuration(ymd, 'P6D') AS ymd;
segment6j = JOIN segment6 BY ymd, groupsum2 BY ymd;
segment6j1 = FOREACH segment6j GENERATE keyval as keyval, readings as readings, sumtemp as sumtemp;

segmentjoined = UNION segment0j1, segment1j1, segment2j1, segment3j1, segment4j1, segment5j1, segment6j1;
joingroup = GROUP segmentjoined by keyval;
sumgroup = FOREACH joingroup GENERATE group AS keyval, (SUM(segmentjoined.sumtemp)/SUM(segmentjoined.readings)) AS average;
orderedgroup = ORDER sumgroup by average DESC;
final = LIMIT orderedgroup 1;
final2 = FOREACH final GENERATE keyval, AddDuration(keyval, 'P6D') AS ymd;
final3 = FOREACH final2 GENERATE SPRINTF('%02d/%02d/%4s', (int)GetMonth(keyval), (int)GetDay(keyval), GetYear(keyval)) AS fromval, SPRINTF('%02d/%02d/%4s', (int)GetMonth(ymd), (int)GetDay(ymd), GetYear(ymd)) AS toval;

dump final3;

