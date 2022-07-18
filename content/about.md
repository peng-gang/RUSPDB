## About dbRUSP

dbRUSP is an interactive database for the analysis and interpretation of newborn screening (NBS) data and for studying the effects of covariates on blood metabolite levels. It currently contains data for blood metabolic analytes detected by tandem mass spectrometry (MS/MS) screening and important covariates for more than half a million babies reported by a public NBS program. Using dbRUSP, users can explore the breadth of metabolic differences in diverse populations, investigate the influence from single variables and from multiple variables on blood metabolite levels, search for novel candidate metabolites and marker ratios for detection of metabolic disorders, and utilize this resource for development and validation of data mining algorithms to increase the accuracy of genetic disease screening.

## Data

Data from 500,539 screen-negative singleton babies born between 2013 and 2017 were selected at random by the California NBS program. The data includes 41 metabolic analytes detected using MS/MS-based screening of dried blood spot samples and six covariates including the continuous variables of birth weight (BW), gestational age (GA), age at blood collection (AaBC), and the categorical variables of sex, parent-reported ethnicity, and parenteral nutrition (TPN) status. The California Department of Public Health is not responsible for the results or conclusions drawn by the authors of this publication.

## Instructions

dbRUSP provides two modules to study the influence on metabolite levels from single variables and joint effects from multiple variables. The first module contains 5 panels including the panel for BW and GA (analyzed together due to high correlation), AaBC, parent-reported ethnicity, sex, and TPN. In the "Multiple comparison" panel under the second module, users can select newborns based on specific covariate selections and compare differences in their metabolite levels to the "common" metabolite ranges (BW 2,500-4,000g, GA 37-41 weeks, Aabc 24-48 hours, no TPN) among newborns in the "reference" group. The "Metabolite to condition" table is available to look up metabolites and corresponding condition(s) with hyperlinks to the Online Mendelian Inheritance in Man (OMIM). Detailed instructions for using dbRUSP are provided in the <a href="userguide.pdf" download="user_guide.pdf">User Guide</a>.

## Code

dbRUSP was built using the R shiny package and the source code is publicly available at [GitHub](https://github.com/peng-gang/RUSPDB). You may also be interested to explore our related projects and web-based tools under https://rusptools.shinyapps.io/home/.

</br>

<img src="attention.png" width="30" height="30"/>

<span style="color: red;"> Response time for some queries may be delayed (30-60 sec) due to large sample size especially in the “GA and BW” and “Age at blood collection” panels. Processing time and encountered latencies may depend on the speed and quality of the servers and internet connection. </span>

Please contact [Curt Scharfe](mailto:curt.scharfe@yale.edu) or [Gang Peng](mailto:gang.peng@yale.edu) with questions or suggestions for dbRUSP.

