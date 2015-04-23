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

###Running basic fume hood calcs

Open and run R script 'process_data.R'. It goes through the following main sections:

1. Reading and formatting all data. Files are read from the `data` directory and loaded into R.
2. Summarize files and compress (combine fume hood data or different dates from multiple data files)
3. Summarize hoods (using compressed data), save csv file with all metrics, save fume hood plots to pdf file
4. Visualizations for hood data, saved to pdf file
5. Analysis at a weekly level, summarize and save plots to pdf
6. Sampling of fume hoods and weeks for analysis in paper
7. Other analysis (testing, not used in paper)

Output from 'process_data.R' are saved to the `output` folder and include the following :

1. hood_summary.csv (created in step 3 above)
2. fume_hood_time_series.pdf (created in step 3 above)
3. explore_variation_within_depts.pdf (created in step 4 above)
4. weekly_hood_summary.csv (created in step 5 above)
5. sample_data.csv (created in step 6 above)


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

