# lexr NEWS

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
