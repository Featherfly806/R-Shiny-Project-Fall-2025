# BF591 - RShiny Project
R shiny application for this project


# Data source
Brown AV, Hudson KA (2015) Developmental profiling of gene expression in soybean trifoliate leaves and cotyledons. BMC Plant Biol 15:169

## Sample Information Exploration
- enables user to upload a CSV file containing sample info
- Tab with a summary of the table that includes a summary of the type and values in each column, e.g.: Number of rows: X Number of columns: Y
- Tab with a data table displaying the sample information, with sortable columns
- Tab with histograms, density plots, or violin plots of continuous variables.


## Counts Matrix Exploration
- Tab with text or a table summarizing the effect of the filtering
- Tab with diagnostic scatter plots, where genes passing filters are marked in a darker color, and genes filtered out are lighter
- Tab with a clustered heatmap of counts remaining after filtering
- Tab with a scatter plot of PCA projections.


## Differential Expression
- Tab with sortable table displaying differential expression results
- enable gene name search functionality to filter rows of table


## Testing_App
- run automated tests on server logic
