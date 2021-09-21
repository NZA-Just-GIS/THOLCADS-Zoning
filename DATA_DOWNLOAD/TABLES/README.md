# Organization
This directory contains three .csv files and two .xlsx files produced by running the R code in the `scripts` folder.

- `ADS_FINAL.csv` - output table with the processed % Black, % "foreign-born," and average building age variables.
  - **UNIQUE_ID** - unique identifier assigned to each HOLC neighborhood.
  - **STATE** - state in which the neighborhood is located.
  - **CITY** - city in which the neighborhood is located.
  - **METRO** - metropolitan area in which the neighborhood is located.
  - **HOLC_GRADE** - HOLC grade.
  - **HOLC_ID** - unique HOLC neighborhood identifier for each city.
  - **REGION** - region in which the neighborhood is located.
  - **ADS_TYPE** - one of six types of area description sheet (ADS) formats that I identify. These are:
    - *early37* - ADS from early 1937; 56 cities; 2,068 neighborhoods
    - *late37* - ADS from late 1937; 55 cities; 2,118 neighborhoods
    - *x3940* - ADS from 1939-40; 18 cities; 2,574 neighborhoods
    - *para* - "paragraph" format; not used; 26 cities; 836 neighborhoods
    - *unique* - format unique to York, PA; not used; 1 city; 27 neighborhoods
    - *none* - no ADS provided
   - **P_BLACK** - Black percentage listed on the ADS.
   - **BLK_TEXT** - written description provided for the % Black variable.
   - **MIN_AGE** - minimum building age listed in the ADS.
   - **MID_AGE** - midpoint building age.
   - **MAX_AGE** - maximum building age listed in the ADS.
   - **P_FOR_BORN** - "Foreign-born" percentage listed on the ADS. 53 NULL values. These are places where a "foreign-born" population was listed but no accompanying percentage was provided.
   - **FB_TEXT** - written description provided for the % "foreign-born variable.


- `SHAPES` - Esri shapefile and identical GeoJSON file. Both contain HOLC neighborhood polygons with attributes produced by the scripts herein. Those attributes include % Black, % "foreign born", and average building age midpoint.
- `TABLES` - output tables produced by the scripts. Please reference the README file in that folder for further information.

# Correspondence
For any issues with these scripts, please [create an issue](https://github.com/snmarkley1/HOLC_ADS/issues).

## License
The data collected and presented are licensed under the [Creative Commons Attribution 4.0 International license](https://creativecommons.org/licenses/by/4.0/), and the underlying code used to format, analyze, and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).
