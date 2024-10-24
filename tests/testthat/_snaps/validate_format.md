# Input parameters

    Code
      plot_step_ahead_model_output(as.list(projection_data_a_us), target_data_us)
    Condition
      Error in `mdl_out_validation()`:
      x `model_output_data` must be a `data.frame`.

---

    Code
      plot_step_ahead_model_output(df_test, target_data_us, show_plot = FALSE)
    Condition
      Warning:
      ! `model_output_data` must be a `model_out_tbl`. Class applied by default

---

    Code
      plot_step_ahead_model_output(df_test, target_data_us, show_plot = FALSE)
    Condition
      Warning:
      ! `output_type_id` column must be a numeric. Converting to numeric.

---

    Code
      plot_step_ahead_model_output(df_test, target_data_us, show_plot = FALSE)
    Condition
      Warning:
      ! `model_output_data` contains some empty columns: age_group.

---

    Code
      plot_step_ahead_model_output(projection_data_a_us, target_data_us, show_plot = FALSE,
        x_col_name = "date")
    Condition
      Error in `mdl_out_validation()`:
      x `model_output_data` did not have all required columns "model_id", "output_type_id", "date", and "value"

---

    Code
      plot_step_ahead_model_output(projection_data_a_us, target_data_us, show_plot = FALSE,
        fill_by = "model_name")
    Condition
      Error in `mdl_out_validation()`:
      x `model_output_data` did not have all required columns "model_id", "output_type_id", "target_date", "value", and "model_name"

---

    Code
      plot_step_ahead_model_output(projection_data_a_us, target_data_us, show_plot = FALSE,
        group = "forecast_date")
    Condition
      Error in `mdl_out_validation()`:
      x `model_output_data` did not have all required columns "model_id", "output_type_id", "target_date", "value", and "forecast_date"

---

    Code
      plot_step_ahead_model_output(df_test, target_data_us, show_plot = FALSE)
    Condition
      Error in `mdl_out_validation()`:
      x `model_output_data` did not have all required columns "model_id", "output_type_id", "target_date", and "value"

---

    Code
      plot_step_ahead_model_output(df_test, target_data_us, show_plot = FALSE)
    Condition
      Error in `output_type_validation()`:
      x `model_output_type_val` did not have the expected output_type_id value 0.975, 0.025, 0.9, 0.1, 0.75, and 0.25

---

    Code
      plot_step_ahead_model_output(df_test, target_data_us, show_plot = FALSE)
    Condition
      Error in `round()`:
      ! non-numeric argument to mathematical function

---

    Code
      plot_step_ahead_model_output(df_test, target_data_us, show_plot = FALSE)
    Condition
      Error in `mdl_out_validation()`:
      x `model_output_data` should contain at least one supported output type.
      i Supported output types: "median" and "quantile".

---

    Code
      plot_step_ahead_model_output(projection_data_a_us, as.list(target_data_us),
      show_plot = FALSE)
    Condition
      Error in `target_validation()`:
      x `target_data` must be a `data.frame`.

---

    Code
      plot_step_ahead_model_output(projection_data_a_us, target_data_us, show_plot = FALSE,
        x_target_col_name = "time_idx")
    Condition
      Error in `target_validation()`:
      x `target_data` did not have all required columns "time_idx" and "observation"

---

    Code
      plot_step_ahead_model_output(projection_data_a_us, target_data_us, show_plot = FALSE,
        intervals = c(0.6, 0.75))
    Condition
      Warning:
      ! `intervals` should correspond to one or multiple of these possible values "0.95", "0.9", "0.8", and "0.5". Only the matching value(s) will be used (if no matching value, the default will be used).

---

    Code
      plot_step_ahead_model_output(d_proj, target_data_us, show_plot = FALSE)
    Condition
      Warning:
      ! `model_output_data` contains more than 5 models, the plot will be reduced to show only one interval (the maximum interval value): "0.95"

---

    Code
      plot_step_ahead_model_output(df_test, target_data_us, show_plot = FALSE,
        use_median_as_point = TRUE)
    Condition
      Error in `output_type_validation()`:
      x `model_output_type_val` did not have the expected output_type_id value 0.5, 0.975, 0.025, 0.9, 0.1, 0.75, and 0.25

---

    Code
      plot_step_ahead_model_output(projection_data_a_us, target_data_us, show_plot = FALSE,
        ens_color = "black")
    Condition
      Error in `ensemble_validation()`:
      x Both `ens_color` and `ens_name` should be set to a non NULL value

---

    Code
      plot_step_ahead_model_output(projection_data_a_us, target_data_us, show_plot = FALSE,
        facet = "value")
    Condition
      Error in `facet_validation()`:
      x if `facet` is not NULL, the argument should be of length 1 and should match one of the task_id columns of `model_output_data`

---

    Code
      plot_step_ahead_model_output(df_test, target_data_us, facet = "scenario_id",
        show_plot = FALSE, facet_nrow = 5)
    Condition
      Warning:
      ! `facet_nrow` should be less or equal to the number of unique `facet` values. By default, the parameter will be set to 4

---

    Code
      plot_step_ahead_model_output(projection_data_a_us, target_data_us, show_plot = FALSE,
        facet = c("target", "scenario_id"))
    Condition
      Error in `facet_validation()`:
      x if `facet` is not NULL, the argument should be of length 1 and should match one of the task_id columns of `model_output_data`

---

    Code
      plot_step_ahead_model_output(projection_data_a_us, target_data_us, show_plot = FALSE,
        facet_title = "center")
    Condition
      Error in `facet_validation()`:
      x `facet_title` should correspond to one of these possible values: "top right", "top left", "bottom right", and "bottom left"

---

    Code
      plot_step_ahead_model_output(projection_data_a_us, target_data_us, show_plot = FALSE,
        top_layer = "model")
    Condition
      Error in `layer_validation()`:
      x `top_layer` should correspond to one of these possible values: "model_output", "target"

