# This converts master or main to HEAD for a git repo

This will also convert a git repo with nothing after the @ to @HEAD

## Usage

``` r
masterMainToHead(gitRepo)
```

## Arguments

- gitRepo:

  A git repository of the form account/repo with optional @branch or
  @sha or @tag

## Value

The git repository with @HEAD if it had @master, @main or no @.
