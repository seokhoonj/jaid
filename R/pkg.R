#' @export
print_pkg_recipe <- function() {
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

#' @export
print_descr_depends <- function() {
  cat(sprintf("Depends: R (>= %s)\n", getRversion()))
}

#' @export
print_descr_imports <- function(pkgs = c("data.table", "ggplot2")) {
  lines <- sort(unique(sapply(pkgs, function(x) sprintf("%s (>= %s)", x, packageVersion(x)))))
  cat("Imports:", paste0("\n  ", lines))
}
