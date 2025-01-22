#' Setup a Python or Miniconda Environment
#'
#' This function creates a Python or Miniconda environment, installs necessary Python packages,
#' and configures the environment for use with the `reticulate` package.
#'
#' @param env_name A character string specifying the name of the environment. For Python-only
#'   environments, this is the path where the virtual environment will be created
#'   (default: `~/.virtualenvs/<env_name>`). For Miniconda, this is the name of the Conda environment
#'   (default: `~/.conda/envs/<env_name>`).
#' @param packages A character vector of Python packages to install. This can include both
#'   regular Python packages (e.g., `"numpy"`) and Git-based packages (e.g.,
#'   `"git+https://github.com/seokhoonj/underwriter"`).
#' @param use_miniconda A logical value indicating whether to use Miniconda for environment management.
#'   If `TRUE`, Miniconda will be used to create and manage the environment. If `FALSE`, a standalone
#'   Python virtual environment (`virtualenv`) will be used. Default is `FALSE`.
#'
#' @details
#' The function dynamically handles the creation and activation of Python or Miniconda environments
#' based on the value of `use_miniconda`. It ensures that Python or Miniconda is installed if not
#' already available and installs the specified packages using `pip` or Conda.
#'
#' - For Python environments, the virtual environment will be created at
#'   `~/.virtualenvs/<env_name>`.
#' - For Miniconda environments, the Conda environment will be created at
#'   `~/.conda/envs/<env_name>`.
#'
#' @return No return value. The function creates and configures the Python or Miniconda environment.
#'
#' @export
setup_python_env <- function(
    env_name = "r-reticulate",
    packages = c("numpy", "pandas", "git+https://github.com/seokhoonj/underwriter"),
    use_miniconda = FALSE
) {
  # 1. Check and install the reticulate package
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    install.packages("reticulate")
  }
  library(reticulate)

  # 2. Determine the environment path based on `use_miniconda`
  if (use_miniconda) {
    conda_env_path <- file.path("~/.conda/envs", env_name)

    # Check if Miniconda is installed
    miniconda_dir <- reticulate::miniconda_path()
    if (!dir.exists(miniconda_dir)) {
      message("Miniconda is not installed. Installing Miniconda...")
      reticulate::install_miniconda()
    }
    message("Using Miniconda...")

    # Ensure the Conda environment exists
    if (!env_name %in% reticulate::conda_list()$name) {
      message("Creating Conda environment: ", conda_env_path)
      reticulate::conda_create(env_name, python_version = "3.10")
    }
    # Activate the Conda environment
    reticulate::use_condaenv(env_name, required = TRUE)
  } else {
    # For virtualenv, set the virtual environment path
    venv_path <- file.path("~/.virtualenvs", env_name)
    python_path <- tryCatch({
      reticulate::py_config()$python
    }, error = function(e) {
      NULL
    })

    if (is.null(python_path)) {
      message("Python is not installed. Installing Python...")
      reticulate::install_python()
      python_path <- reticulate::py_config()$python
    }
    message("Using Python at: ", python_path)

    # Create the virtual environment
    if (!dir.exists(venv_path)) {
      message("Creating virtual environment at ", venv_path)
      reticulate::virtualenv_create(envname = venv_path, python = python_path)
    } else {
      message("Virtual environment already exists at ", venv_path)
    }
    # Activate the virtual environment
    reticulate::use_virtualenv(venv_path, required = TRUE)
  }

  # 3. Install packages with update check
  for (package in packages) {
    if (grepl("^git\\+https://", package)) {
      # Extract the Git repository URL and package name
      repo_url <- sub("^git\\+https://", "https://", package)
      package_name <- sub(".*/", "", gsub("\\.git$", "", repo_url))

      # Check installed version (if any)
      installed_version <- tryCatch({
        py_result <- reticulate::py_run_string(paste0("
import subprocess
result = subprocess.run(['pip', 'show', '", package_name, "'], capture_output=True, text=True)
result.stdout
        "), local = TRUE)
        py_result$result
      }, error = function(e) {
        NULL
      })

      # Fetch the latest Git commit SHA
      latest_sha <- tryCatch({
        py_result <- reticulate::py_run_string(paste0("
import subprocess
result = subprocess.run(['git', 'ls-remote', '", repo_url, "'], capture_output=True, text=True)
result.stdout.splitlines()[0].split()[0]
        "), local = TRUE)
        py_result$result
      }, error = function(e) {
        stop("Failed to fetch the latest commit SHA for: ", repo_url, ". Error: ", e$message)
      })

      # Reinstall if the SHA has changed or not installed
      if (is.null(installed_version) || !grepl(latest_sha, installed_version, fixed = TRUE)) {
        message("Installing or updating Git package: ", package)
        tryCatch({
          reticulate::py_install(package, method = "pip")
        }, error = function(e) {
          stop("Failed to install or update Git package: ", package, ". Error: ", e$message)
        })
      } else {
        message("Git package is already up-to-date: ", package_name)
      }
    } else {
      # Install regular packages
      if (use_miniconda) {
        message("Installing Conda package: ", package)
        tryCatch({
          reticulate::conda_install(env_name, package)
        }, error = function(e) {
          stop("Failed to install Conda package: ", package, ". Error: ", e$message)
        })
      } else {
        message("Installing package with pip: ", package)
        tryCatch({
          reticulate::py_install(package, method = "pip")
        }, error = function(e) {
          stop("Failed to install pip package: ", package, ". Error: ", e$message)
        })
      }
    }
  }

  message("Python environment setup complete. You can now use the reticulate environment.")
}
