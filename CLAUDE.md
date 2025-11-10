# Project-Specific Instructions for EpiNow2

## Git and Commits

### Commit Messages
- Keep commit messages short and to the point
- Don't reference issue numbers in commit messages - these belong in PR descriptions, not commits
- Focus on what the change does, not which issue it addresses
- Break work into small atomic commits when possible

### Example

"Add prior choice guide vignette"

## Pull Requests

- Follow the pull request template in `.github/PULL_REQUEST_TEMPLATE.md`
- Complete the checklist items in the template
- Link to the related issue in the PR description (not in commit messages)

## Testing

### test_that() Descriptions
- Make test descriptions descriptive but brief
- Common patterns:
  - "works as expected with default arguments"
  - "errors for bad 'X' specifications"
  - "produces expected output"
  - "correctly handles X"

### Example
```r
test_that("calc_CrI works as expected with default arguments", {
  # test code
})
```

## Code Comments

### Iterative Work
- Don't add comments reflecting insights from previous iterations
- Comments should explain *why* code does something, not the history of how we got there
- Avoid comments like "Changed this because previous approach had X problem"

## NEWS.md

### Format
- Add a news item for each piece of work
- Don't reference issue numbers - those are for PRs, not NEWS
- Don't mention code generators/reviewers or @ mentions
- Keep entries relatively short (default: 1 sentence, but can be more if needed)
- Organize by section: "Package changes", "Model changes", "Bug fixes", "Documentation"
- Start with action verbs: "Added", "Fixed", "Updated", "Changed"
- Bug fixes often start: "A bug was fixed where..."

### Example
```markdown
## Package changes

- Development-only dependencies have been moved from `Suggests` to `Config/Needs/dev`.
- The package now has a hex logo.
```

## Deprecation

### Use lifecycle Package
- Deprecate functionality using the `{lifecycle}` package
- Follow the deprecation lifecycle:
  1. Warning - `lifecycle::deprecate_warn()`
  2. Stopping - `lifecycle::deprecate_stop()`
  3. Removal - Delete the function

### Example
```r
# Soft deprecation (warning)
lifecycle::deprecate_warn(
  "1.8.0",
  "old_function(param = 'old')",
  "new_function(param = 'new')"
)

# Hard deprecation (error)
lifecycle::deprecate_stop(
  "1.6.0",
  "old_function()"
)
```

Add lifecycle badge to documentation:
```r
#' @description `r lifecycle::badge("deprecated")`
```

## Code Style

- Follow the tidyverse [style guide](https://style.tidyverse.org)
- Limit lines to 80 characters (code and comments)
- Use [styler](https://CRAN.R-project.org/package=styler) package to apply styles
- Don't restyle code unrelated to your changes
- Use [roxygen2](https://cran.r-project.org/package=roxygen2) with Markdown syntax for documentation
- Use [testthat](https://cran.r-project.org/package=testthat) for unit tests

## Documentation

### roxygen2 Function Titles
- Use sentence case for function titles, not title case
  - Good: `#' Calculate confidence intervals`
  - Bad: `#' Calculate Confidence Intervals`

## Additional Context

See `.github/CONTRIBUTING.md` for full contribution guidelines.
