# Organization
This folder contains the output tables that users can download and incorporate into their own work. The tables are as follows:

- `ADS_FINAL.csv`
  - **UNIQUE_ID** - unique identifier for all HOLC neighborhoods included in the dataset.
  - **STATE** - state in which HOLC city is located.
  - **CITY** - HOLC city.
  - **METRO** - metropolitan area in which HOLC city is located.
  - **HOLC_GRADE** - HOLC neighborhood GRADE: A, B, C, or D (one E)
  - **HOLC_ID** - HOLC neighborhood ID unique to each city.
  - **REGION** - region in which HOLC city is located.
  - **ADS_TYPE** - type of area description sheet (ADS): *early37*, *late37*, *x3940*.
  - **P_BLACK** - Black percentage of the population.
  - **BLK_TEXT** - actual text recorded by the field agent in % Black section of the ADS.
  - **MIN_AGE** - youngest building age listed on the ADS.
  - **MID_AGE** - midpoint between minimum and maximum building ages listed on the ADS.
  - **MAX_AGE** - oldest building age listed on the ADS.
  - **P_FOR_BORN** - Foreign born percentage of the population. NOTE: NAs indicate neighborhoods in which the appraiser listed a foreign born group but did not list any percentage or other quantitative descriptor (e.g., "few", "many", etc.).
  - **FB_TEXT** - actual text recorded by the field agent in the % Foreign Born section of the ADS.

# Correspondence
For any issues with these scripts, please [create an issue](https://github.com/[removed]/HHUUD10/issues).

## License
The data collected and presented are licensed under the [Creative Commons Attribution 4.0 International license](https://creativecommons.org/licenses/by/4.0/), and the underlying code used to format, analyze, and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).
