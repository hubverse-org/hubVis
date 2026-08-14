# Example forecast hub data

The hubExamples package provides three data sets that contain example
model output and target data for an example forecast hub:
`forecast_outputs`, `forecast_target_ts`, and `forecast_oracle_output`.

`forecast_outputs` contains example forecast data that represents model
outputs from a forecast hub with predictions for three influenza-related
targets (wk inc flu hosp, wk flu hops rate category, and wk flu hosp
rate) for two reference dates in 2022. This dataset reflects forecast
outputs as they look when retrieved from a hub via the `hubData` package
(which is slightly different than they look when originally submitted by
modelers).

`forecast_target_ts` contains time series target data from a hub that
predicts influenza-related targets.

## Usage

``` r
forecast_outputs

forecast_target_ts
```

## Format

### `forecast_outputs`

A data frame with 10,224 rows and 9 columns:

- model_id:

  the name of the model

- reference_date:

  the starting point of the forecast in yyyy-mm-dd format

- target:

  a unique identifier for the target

- horizon:

  number of time units ahead being forecasted relative to the
  `reference_date`, in units of weeks

- location:

  FIPS code identifying a location

- target_end_date:

  the date of occurrence of the outcome of interest in yyyy-mm-dd
  format; this can be calculated directly from the `reference_date` and
  `horizon` as follows: `target_end_date = reference_date + 7*horizon`

- output_type:

  the type of representation of the prediction

- output_type_id:

  more identifying information specific to the output type;
  `output_type_id` is not relevant for every kind of `output_type` (for
  example, hubs will not expect `output_type_id` values when the
  `output_type` is mean or median

- value:

  the model’s prediction

### `forecast_target_ts`

A data frame with 20,510 rows and 4 columns:

- target_end_date:

  the date of the target observation in yyyy-mm-dd format

- target:

  a unique identifier for the target

- location:

  FIPS code identifying a location

- observation:

  a count of hospital admissions in the given `location` in the week
  ending on the given `date`.

## Source

<https://github.com/hubverse-org/example-complex-forecast-hub/>

## Details

Data from `hubExamples` package:
https://github.com/hubverse-org/hubExamples

Data from `hubExamples` package:
https://github.com/hubverse-org/hubExamples
