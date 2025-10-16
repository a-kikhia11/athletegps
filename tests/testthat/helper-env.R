# If running locally use .Renviron
#readRenviron(file.path(rprojroot::find_rstudio_root_file(), ".Renviron"))

# Load libraries
library(jsonlite)
library(keyring)

# DELETE EXISTING CREDS ----
# Simulates a fresh install
# Tests involving a reinstall should expect pre-existing config and tokens

clear_api_key <- function() {
  # Deletes athletegps_credentials from keyring
  try(key_delete("statsports_credentials", username = "api_key"), silent=TRUE)
}

# Delete config .json file if it exists
clear_config_file <- function() {
  delete_if_exists <- function(path) {
    if (file.exists(path)) {
      file.remove(path)
    }
  }

  file_path_config <- file.path(Sys.getenv("HOME"), ".statsports_config.json")
  delete_if_exists(file_path_config)
}

clear_all_credentials <- function(){
  clear_api_key()
  clear_config_file()
}

# Clears credentials in the environment if they exist
clear_env_creds <- function() {
  Sys.unsetenv("api_key")
}

# Reload credentials and token
reset_credentials <- function() {
  set_credentials(
    api_key = Sys.getenv("api_key")
  )
}
