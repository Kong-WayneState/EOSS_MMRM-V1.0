# EOSS_MMRM-V1.0

1. Save all 6 files in a single directory on your computer.

2. Launch R Studio on your computer.

3. In the R Studio console, use the setwd() function to set the working directory to the folder where you saved the files.
   For example: setwd("path/to/your/folder")

4. In the R Studio console, use the install.packages() function to install the necessary packages.
   For example: install.packages(c("shiny", "shinydashboard", "dplyr", "mmrm", "rsconnect"))

5. In the "app.R" file, set the number of cores for parallel calculation and run it. 
   For example: num.core = 6 # You can adjust this number based on your system capabilities
   or at most, you can use num.core = detectCores()-1
   
6. Then you can launch the Shiny app by clicking the "Run App" button in the script editor toolbar.
