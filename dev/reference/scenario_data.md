# Example scenario hub data

The hubExamples package provides two data sets that contain example
model output and target data for an example scenario hub:
`scenario_outputs` and `scenario_target_ts`.

Data from `hubExamples` package:
https://github.com/hubverse-org/hubExamples

`scenario_target_ts` contains time series target data associated with
the scenario projection data.

## Usage

``` r
scenario_outputs

scenario_target_ts
```

## Format

### `scenario_outputs`

A data frame with 7,176 rows and 9 columns:

- model_id:

  the name of the model

- origin_date:

  the starting point of the projection in yyyy-mm-dd format

- scenario_id:

  a unique identifier for the scenario

- location:

  FIPS code identifying a location

- target:

  a unique identifier for the target

- horizon:

  number of time units ahead being projected relative to the
  `origin_date`, in units of weeks

- output_type:

  the type of representation of the prediction; in this example, all
  values for the `output_type` are "quantile".

- output_type_id:

  more identifying information specific to the output type; here, the
  `output_type_id` specifies the probability level for the quantile
  prediction

- value:

  the model’s prediction

### `scenario_target_ts`

A data frame with 127 rows and 4 columns:

- location:

  FIPS code identifying a location

- date:

  the date of the target observation in yyyy-mm-dd format

- observation:

  a count of incident cases in the given `location` in the week ending
  on the given `date`.

- target:

  a unique identifier for the target

## Source

<https://github.com/hubverse-org/example-complex-scenario-hub/>

## Details

`scenario_outputs` contains example scenario projection data that
represents model outputs and an ensemble from a scenario hub with
predictions for one target (`inc hosp`) in one location (`"US"`), one
round ("2021-03-07") and four scenarios. This dataset reflects scenario
projection outputs as they look when retrieved from a hub via the
`hubData` package (which is slightly different than they look when
originally submitted by modelers), and with a "mean" ensemble calculated
with the `hubEnsemble` package `simple_ensemble()` function with default
parameters. The date of occurrence of the outcome of interest in
yyyy-mm-dd format can be calculated directly from the `origin_date` and
`horizon` as follows:
`target_end_date = origin_date + (7 * horizon) - 1`

Data from `hubExamples` package:
https://github.com/hubverse-org/hubExamples
