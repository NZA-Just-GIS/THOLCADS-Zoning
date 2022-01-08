# Organization
Data housed in this folder are produced partially or wholly from manual processes. These tables are necessary inputs that are called in by the scripts. Upon running the code in the `scripts` folder, this folder will be further populated with a GeoJSON file imported from the [Digital Scholarship Lab (DSL)](https://dsl.richmond.edu/panorama/redlining/#loc=4/40.886/-105.499&text=downloads).

- `age_fix.csv` - contains minumum, maximum, and midpoint building ages for select cities and neighborhoods where the OCR process misread the scanned area description sheets (ADS).

- `chicago_fix.csv` - contains the unique identifier (**unique_id**) and % Black values (**black**) for 202 Chicago neighborhoods. This table is imported into the `03_organize_blk.R` script, joined with the dataframe in there, and used to override its % Black estimates. This step is necessary because there is an error in how those entries were digitized.

- `holc_cities.csv` - contains information about cities mapped in the HOLC's City Survey Program. Columns are as follows:
  - **state** - state that the city is in.
  - **city** - city according to HOLC.
  - **metro** - metropolitan area city is in.
  - **nhoods** - number of HOLC neighborhoods in a given city.
  - **sheets** - percent of HOLC neighborhoods that have area description sheets (ADS) listed on the [DSL Site](https://dsl.richmond.edu/panorama/redlining/#loc=4/40.88/-105.469).
  - **region** - region of the US in which the city is located. Region definitions are mostly based on those offered by the [US Census Bureau](https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf). Exceptions: Baltimore, MD is classified as Northeast (NE) instead of South (S), and every Pennsylvania city west of Harrisburg is categorized as Midwest (MW) instead of NE.
  - **yr_sheet** - date, if any, listed on the area description sheet (manually recorded).
  - **ads_type** - type of ADS sheet. The HOLC changed their ADS format throughout the history of the City Survey Program, and not all ADS types are compatible. For processing purposes, cities need to be separated by type before recombining. I identified six types and work with only the first three in this project: 
    - *early37* - nhoods = 2,114; cities = 56
    - *late37* - nhoods = 2,185; cities = 55
    - *x3940* - nhoods = 2,595; cities = 18
    - *para* - nhoods = 836; cities = 26; these sheets provide only descriptive paragraphs.
    - *unique* - nhoods = 27; cities = 1; no other city has a format mathcing that of York, PA.
    - *none* - nhoods = 1,117; cities = 46; these maps have no accompanying area description sheets

- `holc_json.GeoJSON` - GeoJSON file of the ADS variables pulled from the [DSL](https://dsl.richmond.edu/panorama/redlining/#loc=4/40.886/-105.499&text=downloads). The commented out code to import this data can be found in script `01_load_holc_data.R`, but it is stored locally here in case the DSL makes any changes to the file on their end. This data was last imported on December 1, 2021.

- `numbers.csv` - crosswalk connecting numbers (**no**) to written-out numbers (**alpha_no**)

# Correspondence
For any issues with these scripts, please [create an issue](https://github.com/[removed]/HIST_HU_URB/issues).

## License
The data collected and presented are licensed under the [Creative Commons Attribution 4.0 International license](https://creativecommons.org/licenses/by/4.0/), and the underlying code used to format, analyze, and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).
