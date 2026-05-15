## Release information

Note on the previous (cancelled) submission:

- The earlier submission of this version was correctly cancelled because, during the package's tests, the installation engine
  attempted to use sudo. We investigated this immediately.

- The package itself contains no calls to sudo or any other privilege-escalation command. The cause was the pak package, which this
  release adopts as its default installation engine. On Linux,
  pak includes an optional feature that automatically installs missing
  system libraries, and to do so it checks for and uses sudo. This `Require` package did not disable that feature before relying on pak, and
  one of the integration tests caused pak to run during the check, which triggered the sudo attempt on the check machine.

- This has been corrected in two parallel ways:

  1. The package now disables pak's automatic system-library installation when it loads, before any call to pak is made. It will
  never attempt to install system software or use elevated privileges. Installing system libraries remains the responsibility of the
  user or system administrator. This setting is applied only when the user has not explicitly chosen otherwise, so an informed user
  who deliberately enables the feature retains that choice.
  2. The integration tests that install packages over the network (and therefore exercise pak) are now skipped on CRAN, so they no
  longer run during the check. These tests continue to run in the project's continuous-integration environment. Tests that do not
  require network access are unaffected and still run on CRAN.

- We have verified that loading the package disables the feature, and that the affected tests are skipped under the CRAN check
  environment. We apologise for the earlier submission and have taken care to ensure the package no longer attempts any system-level
  installation or privilege escalation.

- This is a major release (2.0.0). The major change is that the package
dependency and installation engine now defaults to `pak`. The legacy
non-pak code path is retained for users who set
`options(Require.usePak = FALSE)`, but `pak` is the only actively
maintained installer going forward -- the version bump signals the
backbone switch. See `NEWS.md` for the full list of changes. 

We updated the vignette that explains in detail why 
this package plays a different role than `pak`, `renv` and `base` functions like
`install.packages` and `require` for package management. 


## Test environments

  Require is a pure-R package (no compiled code), so the GitHub Actions +
  win-builder matrix covers the OS / R-version surface; rhub flavours
  that target compiled-code or numeric-precision issues (ASAN/UBSAN,
  valgrind, ATLAS, noLD) were skipped as they add no value here.

  ### GitHub Actions (run on the submitted commit)
  * Ubuntu 24.04: R-devel, R 4.6.0 (release), R 4.5.3, R 4.4.3, R 4.3.3
  * Windows:      R-devel, R 4.6.0 (release), R 4.5.3, R 4.4.3, R 4.3.3
  * macOS:        R 4.6.0 (release)

  ### win-builder
  * Windows, R-devel
  * Windows, R 4.6.x (release)
  * Windows, R 4.5.x (oldrelease)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

We checked the 1 reverse dependency (`SpaDES.core`) from CRAN, comparing
R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

SpaDES.core has 1 pre-existing vignette error (an `ii-modules.Rmd`
sentinel about an interrupted `spades()` call) that occurs identically
on both the CRAN and dev versions of Require. It is unrelated to this
package and was present before the 2.0.0 changes.
