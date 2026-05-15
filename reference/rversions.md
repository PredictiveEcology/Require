# R versions

Reference table of R versions and their release dates (2018 and later).

## Usage

``` r
rversions
```

## Details

Update this as needed using `rversions::r_versions()`:

` # install.packages("rversions") v = rversions::r_versions() keep = which(as.Date(v$date, format = " as.Date("2018-01-01", format = " dput(v[keep, c("version", "date")]) `
