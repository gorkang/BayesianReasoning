# BayesianReasoning 0.4.2

Major updates  

* New plot_cutoff()  
  + Shows healthy and sick distributions and shows FP, FN, TP and TN depending on a cutoff point  
* New remove_layers_cutoff_plot() functions  
  + Get's rid of layers of a cutoff_plot: FP, FN, TP or TN  

Minor updates

* Fix for CRAN change in docType
* Use linewidth instead of size
* Faster implementation of a step in min_possible_prevalence


# BayesianReasoning 0.4.1

Minor updates

* safely_show_image_Rmd() function to fail gracefully when URL not found in Rmd's
* Add and fix checks in process_variables() 
* Add tests to cover corner cases and new checks


# BayesianReasoning 0.4.0

Major updates

* Refactored the way we calculate PPV and NPV
* Added new parameter to control type of y scale (one_out_of) # 33
* Added new parameter to control x axis (limits_Sensitivity and limits_Specificity)
* Added multiple checks to catch corner conditions
* Renamed lots of parameters to make things more coherent

Minor updates  

* Avoid acronyms
* Add bold label to area overlay
* Get rid of extra character in legend
* Translate all elements in area overlay
* Changed palette for NPV plots
* Add tests for new parameters and checks
* Add extra info in area overlay


# BayesianReasoning 0.3.3

Minor update  

* x axis labels should always be equally spaced #37
* Update tests to work with upcoming ggplot2 #42
* Add default parameters to avoid errors #43
* Fix LazyData NOTE

# BayesianReasoning 0.3.2

Clean up old comments, add more tests...

* 100% code coverage
* min_possible_prevalence() is now much more efficient
* Improvements to overlay = "line" en PPV_heatmap(), now using {ggforce} for labels
* Changed color palette for NPV
* Tweaked color palette for PPV


# BayesianReasoning 0.3.1

Corrected issues raised in CRAN revision

* Reduced title to less than 65 characters
* Do not capitalize things in the Description text
* Added references to Description explaining main concepts
* Replaced cat() with message()
* Added folder parameter to PPV_diagnostic_vs_screening() and PPV_heatmap() functions

Added tests

* Added tests for all main functions
* Added codecov



# BayesianReasoning 0.3.0

Global clean-up

* No more global environment vars
* PPV and NPV working
* Passes all CRAN checks

# BayesianReasoning 0.2.0

Some improvements to stability

* Overlay works better
* Shiny app working

# BayesianReasoning 0.1.0

Initial release, includes 3 functions:  

* **PPV_heatmap.R**: Plot PPV heatmaps  
* **PPV_diagnostic_vs_screening.R**: Plot PPV for a diagnostic and a screening group  
* **min_possible_prevalence.R**: Show minimum possible prevalence given the test characteristics  
