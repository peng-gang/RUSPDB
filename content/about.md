## About dbRUSP
Metabolic levels of newborns are influenced by different clinical conditions. dbRUSP is designed to show the changes of metabolic levels under different clinical conditions. 

## Instructions
There are 7 panels besides the **About** panel in this database: **GA and BW** (gestational age and birth weight), **Ethnicity**, **Sex**, **Age at blood collection** (AaBC), **TPN** (total parenteral nutrition), **Multiple Comparison** and **Metabolite to condition**. The first 5 panels investigate the influence from one or two clinical variables to the metabolic levels. **GA and BW** panel investigate both gestational age and birth weight together because GA and BW are high correlated. Exploring GA or BW alone may lead to some biased results. In the **Multiple Comparison** panel, we compare the metabolic level in a specific group (influences from multiple clinical variables) selected by the users to the metabolic level in the most common group (birth weight between 2500-4000g, gestational age between 37-41 weeks, age at blood collection between 24-48 hours, and without TPN). In the **Metabolite to condition** panel, we include a table to show disorders related to metabolites and their OMIM links. The detailed instructions of how to use this database can be found in the <a href="userguide.pdf" download="user_guide.pdf">User Guide</a>.

## Data
Data from 500,539 screen-negative singleton infants born between 2013 and 2017 were selected at random by the California NBS program that included metabolic analytes measured by MS/MS as well as birth weight (BW), gestational age (GA), sex, ethnicity, age at blood collection (AaBC), and total parenteral nutrition (TPN). The California Department of Public Health is not responsible for the results or conclusions drawn by the authors of this publication.

## Code
All the RUSPtools websites are built with <a href="http://www.r-project.org" target="_blank">R</a> and the <a href="http://shiny.rstudio.com" target="_blank">Shiny framework</a> and the source code are publicly available at GitHub(TBA). 

</br>

<img src="attention.png" width="30" height="30" /> 

<span style="color: red;"> The sample size in this database is relatively large. It may takes about 30s to 1min to process the data and show the results, especially in "GA and BW" and "Age at blood collection" panels. The processing time depends on the condition of server and internet connection. </span>

If you have any questions or suggestions about dbRUSP, please contact [Dr. Curt Scharfe](mailto:curt.scharfe@yale.edu). 
