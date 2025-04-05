
# triact 0.2.0

* First version on CRAN

# triact 0.3.0

* Major revision of the package (inc. help & vignette, example data) in the course of the revision of the associated peer-reviewed paper. Backwards compatibility is not guaranteed. 

* Important bugfix: Fixed left/right classification in Triact\$add_side() which also involved changing the default value for parameter crit_left

# triact 0.3.1

* Fix: now allows accelerator data with any sampling frequency (given consistent) 
* Fix: $load_files() can now deal with truncated timestamps (e.g. dropped 00:00:00 for midnight) 
* Updated dependencies to new R and package versions
* Updated documentation and a new doc of global options (?triact_options)

