# User Interface for Paper Reports QC
A fun project that I worked on to facilitate the data QC process at The American Institutes of Research, D.C. The application is built using Shiny, which is a web application framework for R. This application can be freely used by Data QC Assistants working at A.I.R. The application fully runs locally (on your machine/computer) with no required connection to any external server or the internet.



# Two Easy Steps To Launch The Application ( 5 minutes or Less)
### 1- Install (or Update to) the latest version of R
Go to this [page](https://cran.rstudio.com/) and download the latest version of R available for your operating system (Windows). Open the dowloaded file and follow the simple installation instructions.

### 2- Open R and Launch the App
Open your newly installed/updated R software. Copy the following lines of code:
```
# Install the Shiny Library
if(!require('shiny')) install.packages('shiny')
library(shiny)
# Run Application
runGitHub("QC-Application","ahmedtadde")

```
Paste into the R console and press "ENTER". You may be prompted to choose a "CRAN Repository" for the installation of the required libraries/functions (choose one of the US options). Once all required libraries/functions are automatically installed and loaded, the application will launch and be ready for use in your browser. That's it!

# For Issues, Questions, Comments and Suggestions
I can be reached via Lync (for people at A.I.R) as Tadde, Ahmed. I can also be reached by email: ahmedtadde@gmail.com
