-- part A
--step 1 of reading in dataset is creating a table with all variables
CREATE TABLE flexample(policyID,statecode,county,eq_site_limit,hu_site_limit,
             fl_site_limit,fr_site_limit,tiv_2011,tiv_2012,eq_site_deductible,
             hu_site_deductible,fl_site_deductible,fr_site_deductible,
             point_latitude,point_longitude,line,construction,point_granularity);
--step 2 of reading in dataset is setting the right mode for the file type
.mode csv
--and then importing the raw file
.import DScourseS21/ProblemSets/PS3/FL_insurance_sample.csv flexample

--part B
--use limit to only show a certain number of rows
SELECT * FROM flexample LIMIT 10;

--part C
--use distinct to list only unique values
SELECT DISTINCT county FROM flexample;

--part D 
-- use the avg function to compute average
SELECT AVG (tiv_2012 - tiv_2011) FROM flexample;
--398118.229567354

--part E
--create a one-way frequency table to list amount of different categories
SELECT construction, COUNT(*) FROM flexample GROUP BY construction;
