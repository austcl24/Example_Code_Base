oshkosh = LOAD 'hdfs:/home/ubuntu/final/Oshkosh/OshkoshWeather.csv' using PigStorage(',');
iowacity = LOAD 'hdfs:/home/ubuntu/final/IowaCity/IowaCityWeather.csv' using PigStorage(',');

real_oshkosh = FILTER oshkosh BY $1 >= 1 AND $1 <= 12 AND $2 >= 1 AND $2 <= 31 AND $4 > -100 AND $4 < 200;
real_iaCity = FILTER iowacity BY $1 >= 1 AND $1 <= 12 AND $2 >= 1 AND $2 <= 31 AND $4 > -100 AND $4 < 200;
oshkosh_data = FOREACH real_oshkosh GENERATE $0 AS year, $1 AS month, $2 AS day, (float)$4 AS tempF, 'Oshkosh' AS City, (
  CASE $1
    WHEN 1 THEN 'Winter'
    WHEN 2 THEN 'Winter'
    WHEN 3 THEN 'Spring'
    WHEN 4 THEN 'Spring'
    WHEN 5 THEN 'Spring'
    WHEN 6 THEN 'Summer'
    WHEN 7 THEN 'Summer'
    WHEN 8 THEN 'Summer'
    WHEN 9 THEN 'Fall'
    WHEN 10 THEN 'Fall'
    WHEN 11 THEN 'Fall'
    WHEN 12 THEN 'Winter'
  END
) AS Season; 

iaCity_data = FOREACH real_iaCity GENERATE $0 AS year, $1 AS month, $2 AS day, (float)$4 AS tempF, 'IowaCity' AS City, (
  CASE $1
    WHEN 1 THEN 'Winter'
    WHEN 2 THEN 'Winter'
    WHEN 3 THEN 'Spring'
    WHEN 4 THEN 'Spring'
    WHEN 5 THEN 'Spring'
    WHEN 6 THEN 'Summer'
    WHEN 7 THEN 'Summer'
    WHEN 8 THEN 'Summer'
    WHEN 9 THEN 'Fall'
    WHEN 10 THEN 'Fall'
    WHEN 11 THEN 'Fall'
    WHEN 12 THEN 'Winter'
  END
) AS Season;

merged_data = UNION oshkosh_data, iaCity_data;
grouped_data = GROUP merged_data BY (City, Season);
season_summary = FOREACH grouped_data GENERATE group, AVG(merged_data.tempF) AS average;

season_detail = FOREACH season_summary GENERATE FLATTEN(group), (
 CASE group.City
    WHEN 'Oshkosh' THEN average*-1.0
    ELSE average
  END
) AS average; 

season_grouped = GROUP season_detail BY (Season);
final = FOREACH season_grouped GENERATE group, ABS(SUM(season_detail.average)) AS difference;

DUMP final;
