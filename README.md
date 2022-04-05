
# Demo analysis pipeline and report using dummy data

View report `here`(<report.html>)


## How to run

1.  Open the R console and call `renv::restore()` to install the
    required R packages.
2.  Call
    `tar_make()`(<https://wlandau.github.io/targets/reference/tar_make.html>)
    to run the pipeline.
3.  Call `tar_read(target)` to retrieve a specified target.
4.  Call `tar_visnetwork()` and `tar_glimpse()` to visualise the
    pipeline.

## File structure

``` r
├── _targets.R
├── R/
├──── functions.R
├── ...../
├──── ..........
└── report.Rmd
```

| File             | Purpose                                                                                               |
| ---------------- | ----------------------------------------------------------------------------------------------------- |
| \[`_targets.R`\] | Declares the [`targets`](https://docs.ropensci.org/targets) pipeline. See `tar_script()` for details. |
| \[`R/`\]         | Contains R scripts with user-defined functions.                                                       |
| \[`report.Rmd`\] | an R Markdown report                                                                                  |
