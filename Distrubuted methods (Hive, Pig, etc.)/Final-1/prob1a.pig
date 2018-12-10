oshkosh = LOAD 'hdfs:/home/ubuntu/final/Oshkosh/OshkoshWeather.csv' using PigStorage(',');
realdays = FILTER oshkosh BY $1 >= 1 AND $1 <= 12 AND $2 >= 1 AND $2 <= 31 AND $4 > -100 AND $4 < 200;
day_data = FOREACH realdays GENERATE $0 AS year, $1 AS month, $2 AS day, (float)$4 AS tempF;
day_group = GROUP day_data BY (year, month, day);
day_extremes = FOREACH day_group GENERATE group, MAX(day_data.tempF) AS hottest, MIN(day_data.tempF) AS coldest;

extremecold = FILTER day_extremes BY coldest <= -10;
coldall = GROUP extremecold ALL;
coldcount = FOREACH coldall GENERATE 'Cold Days' as Type, COUNT(extremecold.$0);

extremehot = FILTER day_extremes BY hottest >= 95;
hotall = GROUP extremehot ALL;
hotcount = FOREACH hotall GENERATE 'Hot Days' as Type, COUNT(extremehot.$0);
totals = UNION hotcount, coldcount;

DUMP totals;

