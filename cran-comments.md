## Release information

This update fixes a set of installation bugs, mostly in the `pak` path and in
GitHub references: branch names containing "/" (`Acct/Repo@fix/x`), branches
whose names merely start with "main" or "master", a GitHub package whose
repository name differs from the package name, `Remotes` entries that name
`Require` itself, and installing from a package snapshot
(`packageVersionFile`) at exactly the pinned commits. `Require(character(0))`
now returns `logical()`. See `NEWS.md`. There are no new exported functions.

`pak` is never allowed to escalate privileges (`PKG_SYSREQS=false`, as in 2.1.x).

## Test environments

* Ubuntu 24.04, R 4.6.1 (local, `R CMD check --as-cran` on the built tarball;
  tests: 0 failures, 20 skips for network/pak-install tests)
* GitHub Actions (org reusable workflows): see the badge on the repository
* win-builder (devel, release, oldrelease), macOS builder and R-hub: TO DO
  before submission

## R CMD check results

0 errors | 0 warnings | 0 notes locally (the only NOTE on the development
branch is the development version number and an old `Date`, both set at
release).

## Reverse dependencies

`SpaDES.core` (Imports). `revdepcheck` TO DO before submission.
