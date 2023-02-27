## Test environments
* local R installation, R 4.2.1
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Notes
* This is a re-submission addressing comments to add quotes to package names in the DESCRIPTION.
* Additional requests to remove cat() statements were discussed and agreed that their usage is appropriate for interactive inspection functions, e.g. in R/inspect.R
