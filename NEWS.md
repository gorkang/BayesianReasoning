# README

### v0.3.2 Clean up, test, rinse and repeat

Clean up old comments, add more tests...

* 100% code coverage
* min_possible_prevalence() is now much more efficient
* Improvements to overlay = "line" en PPV_heatmap(), now using {ggforce} for labels
* Changed color palette for NPV
* Tweaked color palette for PPV


### v0.3.1 Testing, testing

Corrected issues raised in CRAN revision

* Reduced title to less than 65 characters
* Do not capitalize things in the Description text
* Added references to Description explaining main concepts
* Replaced cat() with message()
* Added folder parameter to PPV_diagnostic_vs_screening() and PPV_heatmap() functions

Added tests

* Added tests for all main functions
* Added codecov



### v0.3.0 It, works?

Global clean-up

* No more global environment vars
* PPV and NPV working
* Passes all CRAN checks

### v0.2.0 Somehow, sometimes works

Some improvements to stability

* Overlay works better
* Shiny app working

### v0.1.0 It barely works  

Initial release, includes 3 functions:  

* **PPV_heatmap.R**: Plot PPV heatmaps  
* **PPV_diagnostic_vs_screening.R**: Plot PPV for a diagnostic and a screening group  
* **min_possible_prevalence.R**: Show minimum possible prevalence given the test characteristics  
