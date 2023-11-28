# EOSS_MMRM-V1.0

1. Save the zip file including all 6 documents to your computer and unzip it.

2. Launch R Studio on your computer and open app.R.

3. In the R Studio console, install the necessary packages (only needed for the first time).
   
   For example: install.packages(c("shiny", "shinyjs", shinydashboard", "dplyr", "mmrm"))

4. Set the number of cores for parallel calculation.
   
   The default setup is with 6 cores: num.core = 6
   
   You can adjust this number based on your system capabilities.
   
   For example, if your computer has 32 cores, you may set it to 32 or a lower value.
   
5. Then you can launch the Shiny app by clicking the "Run App" button in the script editor toolbar.
