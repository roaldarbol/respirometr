# respirometr 0.2.1

# respirometr 0.2.0

## New features

* Added `normalise_drift_lm()` to correct measurement drift using linear regression
* Added `normalise_drift_means()` to correct measurement drift using linear interpolation between pre/post means

## Improvements

* Enhanced documentation for `read_licor_li850()` and `read_licor_li7000()`
* Added comprehensive test suite for all main functions
* Added input validation and informative error messages using the cli package
* Both UK (normalise) and US (normalize) spelling variants are now supported for drift correction functions

# respirometr 0.1.1

* Added proper pre-post normalisation
* Fixes a bug that caused some files not to be read.

# respirometr 0.1.0

* Initial version.
