# Organization
This folder contains the R scripts used to create the files in the `DATA_DOWNLOAD` folder. Users should run the code in the order of the file numbers. Here, I provide a brief description for each script.

- `00_preamble.R` - is a simple R script that sets up the workspace and loads the libraries used in the subsequent R scripts.

- `01_load_holc_data.R` - pulls in and organizes the HOLC neighborhood GeoJSON file from the [Digital Scholarship Lab (DSL)](https://dsl.richmond.edu/panorama/redlining/#loc=4/41.218/-97.194&text=downloads)

- `02_organize_fb.R` - prepares the % Foreign Born entry on the HOLC area description sheets.

- `03_organize_blk.R` - prepares the % Black variable.

- `04_organize_age.R` - prepares the Building Age variable.

- `05_organize_repair_mortgages.R` - prepares the Repair and Mortgage Availability variables.

- `06_organize_income.R` - prepares the Income variable.

- `07_organize_occupation.R` - prepares the Occupation Class variable.

- `08_impute_output_tables.R` - imputes missing Income, Building Age, and "Foreign Born" variables and corresponding flagged binary variables; generates a cleaned output table; and produces a summary statistics table. The two tables are located in the `DATA_DOWNLOAD` folder.

- `09_bar_graphs.R` - produces the bar graphs in the `DATA_DOWNLOAD` folder.

- `10_gis_files_map.R` - produces the Esri Shapefile and GeoJSON file in the `DATA_DOWNLOAD` folder. Also creates an interactive map that users can explore within R or export as an HTML file.


# Correspondence
If you run into any problems with these scripts, please [create an issue](https://github.com/[removed]/HHUUD10/issues).

## License
The data collected and presented are licensed under the [Creative Commons Attribution 4.0 International license](https://creativecommons.org/licenses/by/4.0/), and the underlying code used to format, analyze, and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).
