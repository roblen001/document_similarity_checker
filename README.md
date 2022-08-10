Roberto Lentini, 2021JULY09

# Proposal Similarity Checker (Shiny) App 

A shiny app for checking similarities between proposals.

Additional information, documentation, and guides coming soon.

<hr>

* Install [R](https://cran.r-project.org/) first and then [RStudio](https://rstudio.com/products/rstudio/download/). Please choose the correct installer carefully as it will depend on your computer's operating system.

<br>

* Download and install the shiny app directly with the following lines of code:
```
  if (!require("devtools")){
    install.packages("devtools")
  }
  devtools::install_github(repo = "roblen001/document_similarity_checker")
```
If you get the following message in your RStudio console, please type 3.
<br><br>
<img src='etc/package-update.png'>

<br>

* Type ```library("ProposalSimilarityChecker")``` to activate the package.

<br>

* Type ```ProposalSimilarityChecker::installPackages()``` to install any missing packages and/or dependencies. Please type 3 again if you get the message above.

<br>

* When installation is complete, type ```ProposalSimilarityChecker::runApp()``` to open the app.

<br>
