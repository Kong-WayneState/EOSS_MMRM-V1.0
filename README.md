# EOSS_MMRM-V1.0

1. Save the zip file including all 6 documents to your computer and unzip it.

2. Launch R Studio on your computer and open app.R.

3. In the R Studio console, use the install.packages() function to install the necessary packages.
   # For example: install.packages(c("shiny", "shinydashboard", "dplyr", "mmrm", "rsconnect"))

4. In the "app.R" file, set the number of cores for parallel calculation and run it. 
   # The default set up is num.core = 6
   # You can adjust this number based on your system capabilities.
   # For example, if your computer has 32 cores, you may set num.core = 32 or less
  
5. Then you can launch the Shiny app by clicking the "Run App" button in the script editor toolbar.
