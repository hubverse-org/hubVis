# Temporary code to store hubExample file to solve Ubuntu GitHub Actions
# issue

forecast_outputs <- hubExamples::forecast_outputs
usethis::use_data(forecast_outputs, overwrite = TRUE)

forecast_target_ts <- hubExamples::forecast_target_ts
usethis::use_data(forecast_target_ts, overwrite = TRUE)

scenario_outputs <- hubExamples::scenario_outputs
usethis::use_data(scenario_outputs, overwrite = TRUE)

scenario_target_ts <- hubExamples::scenario_target_ts
usethis::use_data(scenario_target_ts, overwrite = TRUE)
