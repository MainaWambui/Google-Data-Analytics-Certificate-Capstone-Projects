/*Check if all months have been loaded in the sql tables */
SELECT DISTINCT MONTH(CAST(started_at as date)) AS Months_in_data FROM dbo.[2021_data]
ORDER BY Months_in_data ASC;
/*Check if all months have been loaded in the sql tables */
SELECT DISTINCT MONTH(CAST(started_at as date)) AS Months_in_data FROM dbo.[2020_data]
ORDER BY Months_in_data ASC;
/* join the two databases */
/*Remove nulls in data */
SELECT * INTO Cyclistic_data FROM
(
SELECT * FROM dbo.[2020_data] where start_station_id IS NOT NULL and end_station_id IS NOT NULL
UNION
SELECT * FROM dbo.[2021_data] where start_station_id IS NOT NULL and end_station_id IS NOT NULL
) as joined_data;
/* Check inconsistent data eg end date > start date */
DELETE FROM DBO.Cyclistic_data WHERE CAST(dbo.Cyclistic_data.ended_at AS date) 
> CAST(dbo.Cyclistic_data.started_at AS date);
/*Added column for Duration of Cycle */
ALTER TABLE dbo.Cyclistic_data 
ADD Cycle_length AS DATEDIFF(MINUTE, CAST(ended_at AS date), CAST(started_at AS date));
/* Delete columns with empty string */
DELETE FROM DBO.Cyclistic_data WHERE start_station_id = '' AND end_station_id = '';