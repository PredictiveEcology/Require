# Use message to print a clean square data structure

Sends to `message`, but in a structured way so that a data.frame-like
can be cleanly sent to messaging.

This will only show a message if the value of `verbose` is greater than
the `verboseLevel`. This is mostly useful for developers of code who
want to give users of their code easy access to how verbose their code
will be. A developer of a function will place this `messageVerbose`
internally, setting the `verboseLevel` according to how advanced they
may want the message to be. `1` is a reasonable default for standard
use, `0` would be for "a very important message for all users", `2` or
above would be increasing levels of details for e.g., advanced use. If a
user sets to `-1` with this numeric approach, they can avoid all
messaging.

## Usage

``` r
messageDF(df, round, verbose = getOption("Require.verbose"), verboseLevel = 1)

messageVerbose(..., verbose = getOption("Require.verbose"), verboseLevel = 1)

messageVerboseCounter(
  pre = "",
  post = "",
  verbose = getOption("Require.verbose"),
  verboseLevel = 1,
  counter = 1,
  total = 1,
  minCounter = 1
)
```

## Arguments

- df:

  A data.frame, data.table, matrix

- round:

  An optional numeric to pass to `round`

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

- verboseLevel:

  A numeric indicating what verbose threshold (level) above which this
  message will show.

- ...:

  Passed to `install.packages`. Good candidates are e.g., `type` or
  `dependencies`. This can be used with `install_githubArgs` or
  `install.packageArgs` which give individual options for those 2
  internal function calls.

- pre:

  A single text string to paste before the counter

- post:

  A single text string to paste after the counter

- counter:

  An integer indicating which iteration is being done

- total:

  An integer indicating the total number to be done.

- minCounter:

  An integer indicating the minimum (i.e,. starting value)

## Value

Used for side effects, namely messaging that can be turned on or off
with different numeric values of `verboseLevel`. A user sets the
`verboseLevel` for a particular message.
