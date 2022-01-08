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
   - **P_FB** - "Foreign-born" percentage listed on the ADS. 53 NULL values. These are places where a "foreign-born" population was listed but no accompanying percentage was provided.
   - **FB_GROUP** - "Foreign-born" group listed on the ADS, if any.
   - **OCC_CLASS** - occupation class denoted in the ADS (see [manuscript](https://osf.io/preprints/socarxiv/dktah/) for a description of the methods).
      - *Upper*, *Up_Mid*, *Mid_Mix*, *Low_Mid*, *Lower*, *Other_NA*
   - **MID_INC** - midpoint family income.
   - **MID_AGE** - midpoint building age.
   - **REPAIR** - repair status of the properties in that neighborhood (see [manuscript](https://osf.io/preprints/socarxiv/dktah/) for a description of the methods).
      - *Good*, *Fair-Good*, *Fair*, *Fair-Poor*, *Poor*, *Other_NA*
   - **MORT_AV** - mortgage availability for home buyers.
      - *Good*, *Fair-Good*, *Fair*, *Fair-Poor*, *Poor*, *Other_NA*
   - **MORT_FHA** - binary variable indicating if the appraiser remarked that the area was suitable for Federal Housing Administration (FHA) loans.
      - *1 = yes*
      - *0 = no*
   - **BLACK_TXT** - ADS text for P_BLACK.
   - **FB_TXT** - ADS text for P_FB and FB_GROUP.
   - **OCC_TXT** - ADS text for OCC_CLASS.
   - **INC_TXT** - ADS text for MID_INC (OCC_CLASS for *x3940* neighborhoods).
   - **MORT_TXT** - ADS text for MORT_AV.
   - **FB_FLAG** - binary indicating imputation for P_FB.
   - **INC_FLAG** - binary indicating imputation for MID_INC.
   - **AGE_FLAG** - binary indicating imputation for MID_AGE.

- `ADS_organized.xlsx` - tabulated ADS for early37 (**e37**), late37 (**l37**) , and x3940 (**x3940**) types. The name of each sheet in the Excel sheet corresponds to its ADS type. See code in `01_load_holc_data.R` to get a description of the attribute names.

- `Cities_by_Region.xlsx` - cities included in this analysis by their metro and region. The sheets are separated by region, and the **HOLC Neighborhoods** column lists the number of neighborhoods by city.

- `HOLC_Cities.csv` - identical to the `holc_cities.csv` file in the `tables` folder.

- `Sum_Stats.xlsx` - summary statistics (means) for HOLC grades by region. Column headings should be self-evident, except possibly **Missing Family Income** and **Missing For. Born**. These values represents the number of HOLC neighborhoods where the corresponding variables are missing from the area description sheets. These cases could not be used to calculate the mean **Family Income** and **Foreign Born (%)** values.

# Correspondence
For any issues with these scripts, please [create an issue](https://github.com/snmarkley1/HOLC_ADS/issues).

## License
The data collected and presented are licensed under the [Creative Commons Attribution 4.0 International license](https://creativecommons.org/licenses/by/4.0/), and the underlying code used to format, analyze, and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).

### Citation
> Markley, Scott. Tabulating HOLC Area Description Sheet Data. *Open Science Framework*. https://doi.org/10.17605/OSF.IO/QYTJ8. (2021).
