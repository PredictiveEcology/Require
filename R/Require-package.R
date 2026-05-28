#' @import methods
#' @importFrom callr r
#' @importFrom processx run
#' @rdname Require
#'
#' @details
#' Require declares hard `Imports` on `callr` and `processx` to ensure they are
#' installed alongside Require. They are not called directly from Require's R
#' code; they are runtime dependencies of `pak`'s background `r_session`
#' subprocess. pak's CRAN binary ships embedded copies of these helpers but
#' does not declare them as standalone `Imports`, and on some Windows
#' configurations pak's subprocess wedges unless standalone `processx` /
#' `callr` are also visible in `.libPaths()` (symptom: every per-ref
#' `pak::pak()` call fails with an empty reason string and no pak progress
#' output). Declaring them in Require's Imports guarantees they're present
#' wherever Require is installed.
"_PACKAGE"

## R CMD check satisfies the "Imports field not imported from" check by
## locating at least one `pkg::symbol` or NAMESPACE-imported symbol per
## Imports package. The roxygen @importFrom directives above generate the
## NAMESPACE entries; the no-op reference below ensures the symbols stay
## resolvable even if R CMD check tightens its heuristic. The list() is
## never evaluated past construction at package load time.
.pakRuntimeHelpers <- function() invisible(list(callr::r, processx::run))
