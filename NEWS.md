# hubVis 0.0.0.9100

This release contains an update to apply standardized hub vocabulary and use 
"target data" rather than "truth data" in the hub (including in the functions 
parameter and associated documentation) (#21)

It also contains a patch for `plot_step_ahead_model_output()` to return a
warning instead of an error if the output plot is interactive and the `facet_nrow` 
parameter is higher than the number of maximum facet expected. (#19)

# hubVis 0.0.0.9001

This release contains a bug fix for `plot_step_ahead_model_output()` returns
warning instead of an error for `output_type_id` column in class character
instead of numeric (#18)

# hubVis 0.0.0.9000

* Release of first draft `hubVis` package
