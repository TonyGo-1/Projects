--I would like to query some variables of interest. I would like to query the location_key,date,country_code,country_name,and all 
--the the "search trend" columns

--Instead of listing all search_trends manually (because there's a lot) under select, I first use a DECLARE function to introduce a 
--new variable I can use. In this case, I am making an empty list called "st" (short for search_trends). The SET statement uses a 
--subquery to provide the criteria for information I want to extract. STRING_AGG combines all "column_names" by a comma 
--("," the delimiter). We are querying from the metadata of the bigquery public database from the covid19 open data dataset. 
--The Information_Schema.columns will return us the metadata of the columns. I'm interested in columns that start with search_trends
--so I will specify that in my WHERE statement.

DECLARE st STRING;

SET st = (
  SELECT STRING_AGG(column_name, ', ')
  FROM `bigquery-public-data.covid19_open_data.INFORMATION_SCHEMA.COLUMNS`
  WHERE column_name LIKE '%search_trends%'
);

--EXECUTE IMMEDIATE is used for dynamic SQL queries. FORMAT is taking the subquery and returning us the columns we select as well as the values for the placeholder (%s). The placeholder will be replaced with the search trend columns as stated by the ",st" at the end of the subquery. We use triple quotations (""") because we are executing a string with multiple lines.

EXECUTE IMMEDIATE FORMAT("""
  SELECT
    location_key,
    date,
    country_code,
    country_name,
    %s
  FROM `bigquery-public-data.covid19_open_data.covid19_open_data`
  WHERE country_code = 'US'
     OR country_name = 'United States of America'
  ORDER BY date
  LIMIT 1000
""", st);