# German Post-War Election Database (GPWED)
This repository contains all code used to digitize municipal-level data for German federal elections between 1949 and 1969. See the following paper for details: <https://osf.io/preprints/osf/ubjm4>

The script `/code/scripts/mytextractor.py` has been used to extract the statistical tables in `scans/`. For reasons of storage space the repository only includes a sample of the processed image scans.

## Data access
The harmonized municipal-level data is provided through the R package `gpwed`, which is available directly from this GitHub repository.

```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")

# Install the gpwed package directly from GitHub
devtools::install_github("julian-voss/gpwed")

# Load the package and the main data set
library(gpwed)
data("gpwed")
head(gpwed)
```
