# Introduction
Here I provide provide code for [**Tabulating HOLC Area Description Sheet Data**](https://osf.io/qytj8/). This code converts the % Black, % Foreign Born, and Building Age data provided in the area description sheets (ADS) of the Home Owners's Loan Corporation's (HOLC) city survey maps from the mid-to-late 1930s to .csv and .xlsx files. In these formats, users may now easily integrate these important historical data into their analyses.

### Citation
> Markley, Scott. Tabulating HOLC Area Description Sheet Data. *Open Science Framework* (https://osf.io/qytj8/) (2021).

# Abstract
In the 1930s, the Home Owners’ Loan Corporation (HOLC) oversaw a massive federal program that graded thousands of urban neighborhoods based on their anticipated risk to property investors. The Digital Scholarship Lab at the University of Richmond has graciously digitized the maps and field notes produced by this infamous program and have made them [freely available](https://dsl.richmond.edu/panorama/redlining/#loc=4/40.886/-105.499&text=downloads) to the public. While the maps have received considerable attention, the field notes used to assign these risk grades—available for most cities in their “area description sheets”—remain virtually unusable to most researchers. Addressing this problem, I convert three of the most consequential variables from the description sheets for 129 cities into an accessible and analyzable tabular format. These include the Black population percentage, foreign born population percentage, and the average building age. In addition, I organize the area description sheets into three semi-compatible tables, allowing future researchers to tabulate other variables.

# Organization
- `DATA_DOWNLOAD` - Folders for the data tables (.csv and .xlsx), map layers (Esri Shapefile and GeoJSON), and bar graphs (.tif) produced with this code.
- `scripts` - R scripts used to produce the downloads.
- `tables` - Input data resources. These include data that have been partially or wholly produced using manual data entry.

## Note
If users wish to run the code in the `scripts` folder, they should delete the `DATA_DOWNLOAD` folder after cloning the repo. Otherwise, the code will spit errors.

# Codebook
The codebook for the .csv and .xlsx output tables is provided in the `scripts` folder and in the associated writeup available at [Open Science Framework](https://osf.io/qytj8/).

# Correspondence
For any issues with these scripts, please [create an issue](https://github.com/snmarkley1/HOLC_ADS/issues).

## License
The data collected and presented are licensed under the [Creative Commons Attribution 4.0 International license](https://creativecommons.org/licenses/by/4.0/), and the underlying code used to format, analyze, and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).
