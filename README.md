# fuzzymatching
A prototype of the fuzzy matching application I wrote at CDIC.

To run the application:
1. Install the [Google Chrome portable](https://portableapps.com/apps/internet/google_chrome_portable) into the App subfolder if necessary.
2. Double-click run.bat in the App subfolder.

The application shown here takes in a file with set columns and performs a number of checks between each pair of rows of data to see if they are similar. I go into more detail on the functionality in fuzzymatch_instructions_shiny.pdf.

I used [this blog post](https://www.r-bloggers.com/deploying-desktop-apps-with-r/) to make the Shiny application portable. It uses R-portable and Google Chrome portable, neither of which I wrote - the only part that is my creation is the Shiny application in the shiny subfolder.

The R code here is based on was originally to be used as part of the R tool in Alteryx at my position in CDIC. During my time there, I ended up extending the Alteryx R code greatly from what I'm showing here, with functionalities such as:
- creating templates for which columns should be checked on
- selecting which checks to use
- formatting the input data such as removing duplicate rows, ignoring addresses, etc.
- manipulating some rule-specific variables (e.g. number of columns that can be reversed/misordered)
The full application passed UAT at CDIC. I can get in touch with my former employer for these files if more is needed.