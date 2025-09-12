
# Scaffold ----------------------------------------------------------------

.scaffold_package_recipe <- function() {
  lines <- c(
    "usethis::create_package(path)",
    "usethis::use_description()",
    sprintf("Title: %s", 'chatgtp::ask_chatgpt()'),
    sprintf("Description: %s", 'chatgtp::ask_chatgpt()'),
    "person(given = \"Seokhoon\", family = \"Joo\", email = \"seokhoonj@gmail.com\",\n       role = c(\"aut\", \"cre\"))",
    sprintf("Depends: R (>= %s)", getRversion()),
    "Remotes:\n  github::seokhoonj/ggshort\n  github::seokhoonj/jaid",
    "Imports:\n  data.table (>= 1.14.2),\n  ggplot2 (>= 3.3.3),\n  ggshort,\n  jaid",
    "usethis::use_namespace()",
    "usethis::use_mit_license() # usethis::use_gpl3_license()",
    "usethis::use_package_doc()",
    "#' @description\n#' blah blah blah\n#' @keywords internal,\n#' @useDynLib jaid, .registration = TRUE\n#' @import ggplot2\n#' @importFrom Rcpp sourceCpp",
    "usethis::use_readme_md()",
    "usethis::use_cran_badge()",
    "usethis::use_cran_comments()",
    "devtools::check()",
    "# Change the environment to a terminal",
    "# git add .",
    "# git commit -m 'first commit'",
    "usethis::use_github_action_check_release()"
  )
  cat("path = \"\"", paste0("\n", lines))
}

.scaffold_deprecated <- function(when, what, with) {
  lines <- c(
    sprintf("#' Deprecated: %s()", what),
    "#'",
    "#' @description",
    "#' `r lifecycle::badge(\"deprecated\")`",
    "#'",
    sprintf("#' Use [%s()] instead.", with),
    "#'",
    sprintf("#' @param ... Additional arguments passed to [%s()].", with),
    "#'",
    sprintf("#' @return Same return value as [%s()].", with),
    "#'",
    sprintf("#' @seealso [%s()]", with),
    "#'",
    "#' @export",
    sprintf("%s <- function(...) {", what),
    sprintf("  lifecycle::deprecate_warn(\"%s\", \"%s()\", \"%s()\")", when, what, with),
    sprintf("  %s(...)", with),
    "}"
  )
  cat(lines, sep = "\n")
}

# Internal helper functions -----------------------------------------------

.package_license <- function(packages) {
  license <- sapply(packages, function(x) utils::packageDescription(x)["License"])
  names(license) <- packages
  license
}

.print_description_license <- function(packages = c("data.table", "ggplot2")) {
  lines <- sort(unique(sapply(packages, function(x)
    sprintf("%s (%s)", x, package_license(x)))))
  cat("X-PackageLicense:", paste0("\n  ", lines))

}

.print_description_depends <- function() {
  cat(sprintf("Depends: R (>= %s)\n", getRversion()))
}

.print_description_imports <- function(packages = c("data.table", "ggplot2")) {
  lines <- sort(unique(sapply(packages, function(x)
    sprintf("%s (>= %s)", x,utils::packageVersion(x)))))
  cat("Imports:", paste0("\n  ", lines))
}

