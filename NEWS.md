
# hubVis 0.0.0.9002

This release contains a new feature, a new optional parameter `group` in the 
`plot_step_ahead_model_output()` function to allow to group or partition the 
input data in the plot according to a specific column. This feature is 
currently only available for "static" plot. Please refer to 
`ggplot2::aes_group_order` for more information.

# hubVis 0.0.0.9001

This release contains a bug fix for `plot_step_ahead_model_output()` returns
warning instead of an error for `output_type_id` column in class character
instead of numeric (#18)

# hubVis 0.0.0.9000

* Release of first draft `hubVis` package
