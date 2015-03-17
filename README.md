This project does the following:

- reads csv files from a data folder
- converts data to a common format
- calculates metrics for each fume hood
- plots data for fume hoods

###Directory structure

```
+-- Project_folder
|  +-- fume_hood_analysis   (directory with cloned github repository)
|  |  +-- README.md         
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