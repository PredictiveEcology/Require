## Release information

Require 2.1.1 fixes the check failures CRAN reported for 2.1.0 on four macOS
flavours and on the BLIS additional-issues machine. Both were test-suite
problems rather than package behaviour: the suite assumed the packages it needs
are reachable from the first and last entries of `.libPaths()`, which is not
true under `R CMD check` where contributed packages sit in a middle entry; and
tests that ran `Install()` left a `pak` symlink in the `.Rcheck` directory.

It follows 2.1.0 by five days because those checks are currently failing.

Also included: a `splitGitRepo()` fix (the branch is now taken from whatever
follows `@`, not from a fixed position), and two `browser()` calls on
unreachable-in-practice error paths replaced with `stop()`.

## Test environments

* Ubuntu 24.04, R 4.6.1 (local, `R CMD check --as-cran` on the built tarball)
* win-builder: R-devel, R 4.6.1 (release), R 4.5.3 (oldrelease)
* macOS builder (mac.r-project.org): R 4.6.1, Apple M1
* R-hub: Windows R-devel/release/oldrel; macOS release on arm64 and x86_64;
  Fedora R-devel with ATLAS
* GitHub Actions: Ubuntu and Windows from R-devel to R 4.2, macOS release, plus
  a run with dependencies installed into a library of their own, reproducing the
  split `.libPaths()` layout above

## R CMD check results

0 errors | 0 warnings | 1 note

    Days since last update: 5

That is the interval since 2.1.0, explained above.

The win-builder R-oldrelease (R 4.5.3) run additionally reported an HTTP 403 on
`https://stackoverflow.com/a/36873741/3890027` in `man/setLibPaths.Rd`. Stack
Overflow returns 403 to non-browser clients; the link is valid. No other
platform reported it.

## Downstream dependencies

One reverse dependency on CRAN: SpaDES.core 3.2.1. (The 2.1.0 submission
reported none, which was correct only because SpaDES.core was archived at the
time; it has since returned.)

revdepcheck: 0 new problems, 0 packages failed to check. SpaDES.core 3.2.1
checks clean against this version.
