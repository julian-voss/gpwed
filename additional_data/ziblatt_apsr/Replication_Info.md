# Replication data & code for "Wealth of Tongues: Why Peripheral Regions Vote for the Radical Right in Germany"

Daniel Ziblatt / Hanno Hilbig / Daniel Bischof, June 2023

Below, we provide additional information regarding the replication code and data.

Note that we provide an overview of all tables and figures and the .R files that produce them in the replication_overview.xlsx file.

## SOEP results

- The SOEP data is used for the results on social status (figure 3 and table A3), as well as for the analyses on self-reported dialect usage (SI section A.9)
    - We note that we cannot provide the SOEP data files, since they are proprietary
    - We provide code that cleans the SOEP data and allows researchers to reproduce our results
    - For details on which figures and tables use the SOEP data, see the replication_overview.xlsx file
    - We also provide saved results as an .rds file, such that figure 3 and table A3 can be reproduced
    - For information on SOEP data access, see https://www.diw.de/en/diw_01.c.601584.en/data_access.html
    - We note that our analysis requires linking SOEP respondents to our main dialectal distance measures, which we have done through on-site access at the DIW in Berlin

## GLES results 

- A number of analysis draw on the "Repeatedly questioned respondents of the Short-term Campaign Panel 2013 and 2017 (GLES)" data (see replication_overview.xlsx). We can likewise not provide the GLES data files. We provide code to reproduce our results, as well as saved results as .rds files, which can be used to reproduce figure 3 and table A3.  
- In order to get access to the GLES data, you need to open an account with GESIS. You can then get access to the GLES campaign panel via the GESIS webpage: https://search.gesis.org/research_data/ZA6827 