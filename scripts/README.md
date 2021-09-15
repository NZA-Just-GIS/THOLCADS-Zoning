# Organization
This folder contains the R scripts used to create the files in the `DATA_DOWNLOAD` folder. Users should run the code in the order of the file numbers. Here, I provide a brief description for each script.

- `00_preample.R` - is a simple R script that sets up the workspace and loads the libraries used in the subsequent R scripts.

- `01_load_holc_data.R` - pulls in and organizes the HOLC neighborhood GeoJSON file from the [Digital Scholarship Lab (DSL)](https://dsl.richmond.edu/panorama/redlining/#loc=4/41.218/-97.194&text=downloads)

- `02_organize_fb.R` - organizes and reads the % Foreign Born entry on the HOLC area description sheets.

- `03_organize_blk.R` - organizes and reads the % Black entry on the HOLC area description sheets.

- `04_organize_age.R` - organizes and reads the Building Age entry on the HOLC area description sheets.

- `05_output_table_stats.R` - generates table outputs and summary statistics that are located in the `DATA_DOWNLOAD` folder.

- `06_bar_graphs.R` - produces the bar graphs in the `DATA_DOWNLOAD` folder.

- `gis_files_map.R` - produces the Esri Shapefile and GeoJSON file in the `DATA_DOWNLOAD` folder. Also creates an interactive map that users can explore within R.


# Correspondence
For any issues with these scripts, please [create an issue](https://github.com/[removed]/HHUUD10/issues).

## License
The data collected and presented are licensed under the [Creative Commons Attribution 4.0 International license](https://creativecommons.org/licenses/by/4.0/), and the underlying code used to format, analyze, and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).
