# Introduction
Here I provide provide code for [**Tabulated HOLC Area Description Sheet Data**](https://osf.io/qytj8/). This code converts the % Black, % "Foreign Born", "Foreign-Born" Group, Family Income, Occupation Class, Building Age, Home Repair Status, and Mortgage Availability (for homebuyers) data provided in the area description sheets (ADS) of the Home Owners's Loan Corporation's (HOLC) city survey maps from the mid-to-late 1930s to .csv and .xlsx files. In these formats, users may now easily integrate these important historical data into their analyses.

### Citation
> Markley, Scott. Tabulating HOLC Area Description Sheet Data. *Open Science Framework*. https://doi.org/10.17605/OSF.IO/QYTJ8. (2021).

# Abstract
In the late 1930s, an agency of the United States government called the “Home Owners’ Loan Corporation” (HOLC) graded thousands of urban neighborhoods on the perceived risk they posed to mortgage lenders and property investors. To make these determinations, HOLC field agents collected vast amounts of socioeconomic, demographic, and housing data about these places and presented their findings in an impressive set of maps. While these “redlining” maps have received considerable academic and media attention, the data used to assign risk grades—available for most cities in their “area description” sheets—remain virtually unusable. Correcting this issue, I convert eight of the most consequential variables from 129 cities into an accessible and analyzable tabular format. These include the Black population percentage, “foreign-born” population percentage and group, family income, occupation class, average building age, home repair status, and mortgage availability. This data product will allow researchers to gain a more complete picture of the HOLC’s City Survey program, and it will provide a valuable new source of historical socio-demographic data at the neighborhood level.

# Organization
- `DATA_DOWNLOAD` - Folders for the data tables (.csv and .xlsx), map layers (Esri Shapefile and GeoJSON), and bar graphs (.tif) produced with this code. Users wishing to run through this code should delete the DATA_DOWNLOAD folder after cloning.
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
