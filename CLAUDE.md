# CLAUDE.md

Guidance for Claude Code when working in this repository.

## Overview

`NetLogoR` is an R package that provides S4 classes (`worldMatrix`, `worldArray`,
`agentMatrix`) and functions for building spatially explicit agent-based models,
following the framework of the NetLogo software. It is on CRAN, so changes must
keep `R CMD check` clean.

## Workflow

Use the `/r-lib:r-package-development` skill for package work (`devtools`,
`testthat`, `roxygen2` commands and conventions).

**Exception: do not run `air format .` on this package.** The skill recommends it,
but reformatting the whole codebase would bury real changes in noise. Match the
surrounding formatting by hand instead.

Common commands:

```sh
Rscript -e "devtools::load_all(); <code>"          # run code against the package
Rscript -e "devtools::test()"                      # all tests
Rscript -e "devtools::test(filter = '^world')"     # tests/testthat/test-world*.R
Rscript -e "devtools::document()"                  # regenerate man/ and NAMESPACE
Rscript -e "devtools::check()"                     # R CMD check
```

## Code style

- **Comments:** use `##` for prose/explanatory comments; reserve a single `#` for
  code that has been commented out. This keeps dead code visually distinct from
  explanation. Roxygen (`#'`) is unaffected. Apply this to code you touch — don't
  reformat untouched files.
- `.lintr` enforces `camelCase` object names and a 100-character line limit.
- Use the base pipe (`|>`), not `%>%`.
- Existing code is largely base R + `data.table` + `terra`; keep new dependencies
  out unless there's a clear win.

## Testing

- Tests for `R/{name}.R` live in `tests/testthat/test-{name}.R` (note the existing
  files group by topic, e.g. `test-world-functions.R`).
- New code needs accompanying tests; match the style of neighbouring tests.

## Documentation

- roxygen2 8.1.0 with markdown enabled; `man/` and `NAMESPACE` are generated —
  never edit them by hand.
- Roxygen version bumps churn every `.Rd` file. Keep documentation regeneration in
  a **separate commit** from code changes so diffs stay reviewable.
- Update `NEWS.md` for user-visible changes.

## Git

- `development` is the integration branch; it is merged into `main` before a CRAN
  release. New work goes on a branch off `development` (e.g. `issue-49-worldarray-levels`),
  and PRs target `development`.
- **Multiple Claude sessions may be working in this repo at once.** Stage explicit
  paths; never `git add -A` or `git commit -a`. Check `git status` before staging.
- Do not commit or push unless asked. Commit messages: succinct, lowercase,
  imperative mood (see `git log`), with the `Co-Authored-By:` trailer.

## Deprecation

Don't remove user-facing functions or arguments outright. Deprecate them first
with `.Deprecated()`/a clear message telling users what to use instead and that
removal is coming in a future release (see the `sp` → `sf` transition in
`R/helpers.R`), then remove in a later release.

## CI

- GitHub Actions in `.github/workflows/`: `R-CMD-check`, `pkgdown`, `test-coverage`.
- Pin `actions/checkout` at v6.
- CRAN macOS binaries lag Windows by several days after a source update, so macOS
  jobs can fail on freshly-updated dependencies; source-install workarounds in the
  workflow are temporary and should be removed once binaries catch up.
