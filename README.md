# User Interface for Paper Reports QC
A fun project that I worked on to faciliate the data QC process at The American Institues of Research, D.C. 
The application is built using Shiny, which is a web application framework for R. This application can be freely used by
Data QC Assistants working at A.I.R. The application fully runs locally (on your machine/computer) with no required connection to any external server or the internet.

# Steps to use the application
### 1- Install (or Update to) the latest version of R
Go to this [page](https://cran.rstudio.com/) and download the latest version of R available for your operating system (Windows).
Open the dowloaded file and follow the simple installation instructions.

### 2- Install (or Update to) the latest version of RStudio
Go to this [page](https://www.rstudio.com/products/rstudio/download3/) and navigate to the 'Installers for Supported Platforms' section at the bottom of the page. Choose and download the appropriate file for your operating system (Windows). 
Open the dowloaded file and follow the simple installation instructions. If there is no RStudio shortcut on your desktop after the installation process is done, do the following: search and go to the installation folder (named RStudio), open the 'bin' folder, find the rstudio file, left click on it and select "Create Shortcut". Move the created shortcut to your desktop or your location of choice; Double click on it to launch RStudio.

### 3- Download/Clone this Repository
Scroll up this current page to find and click the green button that reads "Clone or download". Select 'Download Zip'. Unzip the downloaded folder to your location of choice (Desktop for instance). The folder will be named "QC-Application-master" by default.

### 4- Run the Application
Launch RStudio. In the top right corner, click on the "Project(None)" button then select "Open Project...".
Navigate to the location of the "QC-Application-master" folder you just downloaded. Go into the 'code' folder and double-click on "QC App.Rproj". As a final step, locate the console panel/window (easy to find, usually it is the top or bottom right panel in RStudio) and type the following line of code:
```
source("../run.R")
```
Then, press "ENTER" on your keyboard. The application should launch automatically in your browser.

# For Issues, Questions, Comments and Suggestions
I can be reached via Lync (for people at A.I.R) as Tadde, Ahmed. I can also be reached by email: ahmedtadde@gmail.com
