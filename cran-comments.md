## Updated release

This is a new package. This is primarily a function (and its helpers) carved off
from another pacakge, `reproducible` that became too large. See `NEWS.md`.

## Test environments

### Previous R versions
* macOS 9.4                 (travis-ci), R 3.6.3
* Ubuntu 16.04              (travis-ci), R 3.6.3
* Windows                    (appveyor), R 3.6.3
* Windows                 (win-builder), R 3.6.3

### Current R versions
* macOS 10.13.3 High Sierra (travis-ci), R 4.0.0
* Ubuntu 16.04              (travis-ci), R 4.0.0
* Ubuntu 18.04                  (local), R 4.0.0
* Windows                    (appveyor), R 4.0.0
* Windows                 (win-builder), R 4.0.0

### Development R version
* Ubuntu 16.04              (travis-ci), R 4.1.0 (2020-05-18 r78486)
* Windows                    (appveyor), R 4.1.0 (2020-05-17 r78478)
* Windows                 (win-builder), R 4.1.0 (2020-05-15 r78473)

## R CMD check results

There were no ERRORs nor WARNINGs.

There is one NOTE and it is about possible mis-spelled words. These are all correct spelling:
```
Possibly mis-spelled words in DESCRIPTION:
  github (5:23)
  workflow (7:18)
  workflows (3:59, 6:34)
```


## Downstream dependencies

Currently, there are none.
