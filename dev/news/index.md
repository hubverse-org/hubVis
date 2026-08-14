# Changelog

## hubVis (development version)

- add `log_scale` parameter in the
  [`plot_step_ahead_model_output()`](https://hubverse-org.github.io/hubVis/dev/reference/plot_step_ahead_model_output.md)
  function to allow log scale on the y-axis
- fix shared axis on interactive plot version
- minor fix of the vignette to adapt to last version of `hubExamples`
  (\> 0.1)
- add functionality to plot `"sample"` output type as spaghetti plot in
  the
  [`plot_step_ahead_model_output()`](https://hubverse-org.github.io/hubVis/dev/reference/plot_step_ahead_model_output.md)
  function.
- update
  [`plot_step_ahead_model_output()`](https://hubverse-org.github.io/hubVis/dev/reference/plot_step_ahead_model_output.md)
  function to use `"sample"` output type to calculate missing expected
  quantiles if `intervals` and/or `use_median_as_point` are set to a
  numeric vector or `TRUE`, respectively.
- [`plot_step_ahead_model_output()`](https://hubverse-org.github.io/hubVis/dev/reference/plot_step_ahead_model_output.md)
  is now marked with a “questioning” lifecycle badge. The function API
  will change in a future version to improve usability and
  maintainability (see
  [\#71](https://github.com/hubverse-org/hubVis/issues/71)).

## hubVis 0.1.3

This release contains a fix to the issue
[\#64](https://github.com/hubverse-org/hubVis/issues/64): update to code
to be compatible with ggplot2 v4.0.0 and previous version.

## hubVis 0.1.2

This release contains a fix to the issue
[\#56](https://github.com/hubverse-org/hubVis/issues/56): fix palette
creation for `*_color` parameters

## hubVis 0.1.1

This release contains an update to fix issue
[\#46](https://github.com/hubverse-org/hubVis/issues/46):

- create legend after plot generated to add all expected trace in the
  legend
- transform the fill_by column as a factor (internally) to ensure the
  palette is assigned properly

## hubVis 0.1.0

Release stable 0.1.0 version

This release contains an update to the parameter `group` in the
[`plot_step_ahead_model_output()`](https://hubverse-org.github.io/hubVis/dev/reference/plot_step_ahead_model_output.md)
function, now available for “static” and “interactive” plot.
([\#16](https://github.com/hubverse-org/hubVis/issues/16)). Snapshots
tests of graphical output
([\#13](https://github.com/hubverse-org/hubVis/issues/13)) has been
added too.

## hubVis 0.0.0.9201

This release contains an update the standardized argument name for model
output tables in all hubverse packages to `model_out_tbl`
([\#35](https://github.com/hubverse-org/hubVis/issues/35)).

Warning output message has also been updated for more clarification
([\#36](https://github.com/hubverse-org/hubVis/issues/36)).

## hubVis 0.0.0.9100

This release contains an update to apply standardized hub vocabulary and
use “target data” rather than “truth data” in the hub (including in the
functions parameter and associated documentation)
([\#21](https://github.com/hubverse-org/hubVis/issues/21)), use examples
from the [hubExamples](https://github.com/hubverse-org/hubExamples)
package ([\#27](https://github.com/hubverse-org/hubVis/issues/27)). The
associated parameters in
[`plot_step_ahead_model_output()`](https://hubverse-org.github.io/hubVis/dev/reference/plot_step_ahead_model_output.md)
for the target data have been updated accordingly.

It also contains a patch for
[`plot_step_ahead_model_output()`](https://hubverse-org.github.io/hubVis/dev/reference/plot_step_ahead_model_output.md)
to return a warning instead of an error if the output plot is
interactive and the `facet_nrow` parameter is higher than the number of
maximum facet expected
([\#19](https://github.com/hubverse-org/hubVis/issues/19)). A warning
will also be generated if the input model output table contains empty
columns.

The package is also now upgraded to match the hubverse style and best
practise ([\#25](https://github.com/hubverse-org/hubVis/issues/25))

## hubVis 0.0.0.9002

This release contains a new feature, a new optional parameter `group` in
the
[`plot_step_ahead_model_output()`](https://hubverse-org.github.io/hubVis/dev/reference/plot_step_ahead_model_output.md)
function to allow to group or partition the input data in the plot
according to a specific column. This feature is currently only available
for “static” plot. Please refer to
[`ggplot2::aes_group_order`](https://ggplot2.tidyverse.org/reference/aes_group_order.html)
for more information.
([\#16](https://github.com/hubverse-org/hubVis/issues/16))

## hubVis 0.0.0.9001

This release contains a bug fix for
[`plot_step_ahead_model_output()`](https://hubverse-org.github.io/hubVis/dev/reference/plot_step_ahead_model_output.md)
returns warning instead of an error for `output_type_id` column in class
character instead of numeric
([\#18](https://github.com/hubverse-org/hubVis/issues/18))

## hubVis 0.0.0.9000

- Release of first draft `hubVis` package
