The goal of this project is to analyze data from fume hoods in different buildings and departments on campus and characterize fume hood sash closure.

###Getting Started

Using this code to analyse and visualize fume hood data from csv files will require you to perform the following steps:

1. Install R. RStudio is not required, but is highly recommended.
2. Install several R packages, including:
* plyr
* dplyr
* ggplot2
* reshape2
3. Setup file stucture to match the directory structure described below. It will need to contain:
- this fume_hood_analysis repository containing .CSV and .R files
- a data folder into which you'll put csv data files with your fume hood data
- an output folder into which all PDF figures and csv output will be saved
4. Open up and run the R script 'process_data.R' in either RStudio (recommended) or R. This script is the master script to perform the main fume hood data analysis. 

###Directory structure

```
+-- Project_folder
|  +-- fume_hood_analysis   (directory with cloned github repository)
|  |  +-- README.md         
|  |  +-- hood_mapping.csv  (csv file with names of all fume hoods and their buildings/depts)
|  |  +-- process_data.R    (script to process data)
|  |  +-- src               (directory contains data processing, analysis functions)
|  +-- data                 (directory contains csv files with raw fume hood data)
L  +-- output               (directory to save any graphs, output from calculations)
```

###Functions

####check_format
`Description` Check data for indicators of formatting, returns format type

`Example` `file_format <- check_format(original_data) `

`Value` A character string describing data formatting.

####format_long_data
Description: Converts data into a standard wide format from a long format.

Example: `formatted_data <- format_long_data(original_data)`

Value: A data frame with a single column named `dttm` and an additional column with time series data for each fume hood included in the original data (wide format).

####format_wide_data
Description: Converts data into a standard wide format from any wide format. 

The primary differences between the two wide formats is variation in types of separators and inclusion of metadata in original wide formats.

Example: `formatted_data <- format_wide_data(original_data)`

Value: A data frame with a single column named `dttm` and an additional column with time series data for each fume hood included in the original data (wide format).

####plot_fume_hood
Description: Plots time series of fume hood data