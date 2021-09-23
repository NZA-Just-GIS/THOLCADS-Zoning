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

- `ADS_organized.xlsx` - tabulated ADS for early37 (**e37**), late37 (**l37**) , and x3940 (**x3940**) types. The name of each sheet in the Excel sheet corresponds to its ADS type. See code in `01_load_holc_data.R` to get a description of the attribute names.

- `Cities_by_Region.xlsx` - cities included in this analysis by their metro and region. The sheets are separated by region, and the **HOLC Neighborhoods** column lists the number of neighborhoods by city.

- `HOLC_Cities.csv` - identical to the `holc_cities.csv` file in the `tables` folder.

- `Sum_Stats.xlsx` - summary statistics (means) for HOLC grades by region. Column headings should be self-evident, except possibly **Missing For. Born**. This number represents the number of HOLC neighborhoods where a "foreign born" population is named but no percentage is listed. These cases could not be used to calculate the mean **Foreign Born (%)** value.

# Correspondence
For any issues with these scripts, please [create an issue](https://github.com/snmarkley1/HOLC_ADS/issues).

## License
The data collected and presented are licensed under the [Creative Commons Attribution 4.0 International license](https://creativecommons.org/licenses/by/4.0/), and the underlying code used to format, analyze, and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).
