# lexr NEWS

## v0.0.4

- Passes reward tag values through to analysis data.
- Passes weights through to analysis data.
- Added `recovery_days` argument to `make_detect_data` to filter out handling mortalities.
- Added `as.data.frame` function to convert analysis_data object to a data frame.
- Renamed logical matrix `Spawning` in analysis data as `Spawned`.

## v0.0.3
 
- Added spawning function to `make_analysis_data` to identify spawning periods.
- Added growth function to `make_analysis_data` to calculate subsequent lengths.
- `analysis_data` object now includes a capture by period monitored matrix indicating 
whether it has an active acoustic tag.

## v0.0.2

- In analysis data removed matrix unknowns (not subsequently removed) or 
inapplicables (already removed) are now NA instead of FALSE.
- Fixed bug where tags that were missing when recaught now subsequently coded as missing.

## v0.0.1

- Initial release.
