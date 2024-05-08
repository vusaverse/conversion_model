

Sys.setenv("read_and_write_config" = "99_utils/read_and_write_settings.csv")

Sys.setenv("CONFIG_READ" = "99_utils/read_settings.csv")
Sys.setenv("CONFIG_WRITE" = "99_utils/write_settings.csv")


read_config_proj <- function(env_var = "CONFIG_READ", find_loc_in_env = TRUE) {
  read_config_path <- Sys.getenv(env_var)

  # Update this if the read functions (see below) are updated to support to different file types
  supported_extensions <- c("rds", "csv", "fst")


  documentation_reference <- "See documentation at:\n https://r.mtdv.me/load_save_config."

  if (read_config_path == "") {
    rlang::warn("You have not set read settings",
                .frequency = "once", .frequency_id =
                  "read_config_no_env_var")
    return(NULL)
  }

  tryCatch(
    error = function(e) {
      rlang::warn(paste0("Sys.getenv(env_var) doesn't lead to a correct semicolon separated csv.\n",
                         documentation_reference,
      ),
      .frequency = "once",
      .frequency_id = "read_config_file_path")
      return(NULL)
    }, {
      read_config_df <- read_delim(read_config_path, delim = ";", show_col_types = FALSE)

      if (find_loc_in_env == TRUE) {
        read_config_df <- read_config_df %>%
          mutate(across(ends_with("data_dir"), ~if_else(.x %in% names(Sys.getenv()),
                                                        Sys.getenv(.x),
                                                        .x)))
      }
    }
  )

  # TODO This is been required in the config file
  colnames_config <-
    c(
      "script_dir",
      "type",
      "data_dir",
      "base_data_dir",
      "add_branch",
      "extension",
      "message",
      "notes"
    )

  ## Two potential issues with colnames
  colnames_config_warning <- FALSE
  if (length(colnames_config) != length(names(read_config_df))) {
    colnames_config_warning = TRUE

  } else if (all(names(read_config_df) != colnames_config)) {
    colnames_config_warning = TRUE
  }

  if (colnames_config_warning == TRUE) {
    rlang::warn(paste0("Your redconfig file has other column names than expected.\n",
                       "See the config file at:\n",
                       read_config_path,
                       "\n",
                       documentation_reference),
                .frequency = "once",
                .frequency_id = "read_and_write_config_colnames")
    return(NULL)
  }

  ## Test duplicates
  duplications_df <- read_config_df %>%
    dplyr::group_by(script_dir, type) %>%
    dplyr::tally() %>%
    dplyr::filter(n > 1)

  if (nrow(duplications_df) > 0) {
    duplications_str <- duplications_df %>%
      dplyr::mutate(non_unique = if_else(n == 1, NA_character_, paste(script_dir, type, sep = " - "))) %>%
      dplyr::pull(non_unique)

    duplication_info_single_str <- paste(duplications_str, collapse = " | ")

    rlang::warn(paste0("The following directory-types appear more than once:\n",
                       duplication_info_single_str,
                       "\n",
                       documentation_reference),
                .frequency = "once",
                .frequency_id = "read_and_write_config_duplicated")
    return(NULL)
  }

  script_dirs_without_extension <- read_config_df %>%
    dplyr::mutate(supported_extension = extension %in% supported_extensions)  %>%
    dplyr::filter(supported_extension == FALSE) %>%
    dplyr::pull(script_dir)

  if (length(script_dirs_without_extension > 0)) {
    rlang::warn(paste0("The following directories don't have a supported read extension:",
                       paste(script_dirs_without_extension),
                       "\n",
                       "Supported read extension are:",
                       paste(supported_extensions),
                       "\n",
                       documentation_reference),
                .frequency = "once",
                .frequency_id = "read_and_write_config_extensions")
    return(NULL)

  }

  return(read_config_df)

}



write_config_proj <- function(env_var = "CONFIG_WRITE", find_loc_in_env = TRUE) {
  write_config_path <- Sys.getenv(env_var)

  # Update this if the write functions (see below) are updated to support to different file types
  supported_extensions <- c("rds", "csv", "fst")


  documentation_reference <- "See documentation at:\n https://r.mtdv.me/load_save_config."

  if (write_config_path == "") {
    rlang::warn("You have not set read settings",
                .frequency = "once", .frequency_id =
                  "write_config_no_env_var")
    return(NULL)
  }

  tryCatch(
    error = function(e) {
      rlang::warn(paste0("Your write_config_path doesn't lead to a correct semicolon separated csv.\n",
                         documentation_reference,
      ),
      .frequency = "once",
      .frequency_id = "write_config_file_path")
      return(NULL)
    }, {
      write_config_df <- read_delim(write_config_path, delim = ";", show_col_types = FALSE) %>%
        dplyr::mutate(extensions = strsplit(extensions, ",\\s*"))

      if (find_loc_in_env == TRUE) {
        write_config_df <- write_config_df %>%
          mutate(across(ends_with("data_dir"), ~if_else(.x %in% names(Sys.getenv()),
                                                        Sys.getenv(.x),
                                                        .x)))
      }
    }
  )

  # TODO This is been required in the config file
  colnames_config <-
    c(
      "script_dir",
      "type",
      "data_dir",
      "base_data_dir",
      "add_branch",
      "extensions",
      "message",
      "notes"
    )

  ## Two potential issues with colnames
  colnames_config_warning <- FALSE
  if (length(colnames_config) != length(names(write_config_df))) {
    colnames_config_warning = TRUE

  } else if (all(names(write_config_df) != colnames_config)) {
    colnames_config_warning = TRUE
  }

  if (colnames_config_warning == TRUE) {
    rlang::warn(paste0("Your redconfig file has other column names than expected.\n",
                       "See the config file at:\n",
                       write_config_path,
                       "\n",
                       documentation_reference),
                .frequency = "once",
                .frequency_id = "write_config_colnames")
    return(NULL)
  }

  ## Test duplicates
  duplications_df <- write_config_df %>%
    dplyr::group_by(script_dir, type) %>%
    dplyr::tally() %>%
    dplyr::filter(n > 1)

  if (nrow(duplications_df) > 0) {
    duplications_str <- duplications_df %>%
      dplyr::mutate(non_unique = if_else(n == 1, NA_character_, paste(script_dir, type, sep = " - "))) %>%
      dplyr::pull(non_unique)

    duplication_info_single_str <- paste(duplications_str, collapse = " | ")

    rlang::warn(paste0("The following directory-types appear more than once:\n",
                       duplication_info_single_str,
                       "\n",
                       documentation_reference),
                .frequency = "once",
                .frequency_id = "write_config_duplicated")
    return(NULL)
  }

  ## Check of all the given extensions appear in the supported extensions
  script_dirs_without_extension <- write_config_df %>%
    dplyr::mutate(supported_extension = map_lgl(
      extensions,
      ~ if_else(
        length(.x) == 1,
        (max(.x %in% supported_extensions)),
        sum(map_lgl(.x, ~
                      .x %in% supported_extensions)) == length(.x)
      )
    )) %>%
    dplyr::filter(supported_extension == FALSE) %>%
    dplyr::pull(script_dir)

  if (length(script_dirs_without_extension > 0)) {
    rlang::warn(paste0("The following directories don't have a supported write extension:",
                       paste(script_dirs_without_extension),
                       "\n",
                       "Supported extension are:",
                       paste(supported_extensions),
                       "\n",
                       documentation_reference),
                .frequency = "once",
                .frequency_id = "write_config_extensions")
    return(NULL)

  }

  return(write_config_df)

}


filter_settings_on_type <- function (settings_script_dir, settings_type) {

  if (is.null(settings_type)) {
    # No settings
    if (nrow(settings_script_dir) > 1) {

      rlang::inform(paste0("The first type is selected from the following possibilties:\n",
                           as.character(paste(dplyr::pull(settings_script_dir, var = "type"), collapse = " "))),
                    .frequency = "once",
                    .frequency_id = "write_file_proj_type")

      settings_script_dir <- settings_script_dir %>%
        slice_head(n = 1)
    }
  } else if (!is.null(settings_type)) {

    settings_script_dir_type <- settings_script_dir %>%
      dplyr::filter(type == settings_type)

    ## Gaurd clause
    if (nrow(settings_script_dir_type) == 0) {
      if (nrow(settings_script_dir == 1)) {
        rlang::abort("The supplied type is not available. Please remove the type parameter.\n",
                     "This will select the only values for this script directory.")
      }
      if (nrow(settings_script_dir >= 1)) {
        rlang::abort(paste0("The supplied type is not available. The available types are:\n",
                            dplyr::pull(settings_script_dir$type),
                            "\n",
                            "Remove the type parameter for the first choice or provide one of the given values."))
      }
    }

    # The good case
    settings_script_dir <- settings_script_dir_type

  }
  return(settings_script_dir)
}




overwrite_dot_arguments <- function(function_args, ...) {
  dots <- list(...)
  return(c(function_args[setdiff(names(function_args), names(dots))], dots))
}





read_file_proj <- function(
    name,
    settings_df = read_config_proj(),
    settings_type = NULL,
    sub_dir = NA_character_,
    dir = NULL,
    base_dir = NULL,
    add_branch = NULL,
    full_dir = NULL,
    extension = NULL,
    fix_encoding = FALSE,
    csv_encoding = "latin1",
    csv_names_unique = TRUE,
    ...
) {

  ## Get the script path relative to the working directory
  script_path <- stringr::str_remove(this.path::sys.path(), paste0(getwd(), "/"))

  # Check the manual and config setting for completeness

  # Both either base_dir or dir AND extension must be set to be manual
  if (sum(max(!is.null(dir), !is.null(base_dir)), !is.null(extension)) < 2) {
    manual_settings <- FALSE
  } else {
    manual_settings <- TRUE
  }

  if (is.null(settings_df)) {
    config_settings <- FALSE
  } else {
    config_settings <- TRUE
    settings_script_dir <- settings_df %>%
      dplyr::filter(str_detect(script_path, script_dir))

    if (nrow(settings_script_dir) == 0) {
      config_settings <- FALSE
    } else {
      settings_script_dir <- filter_settings_on_type(settings_script_dir, settings_type)

      if (!is.na(settings_script_dir$message)) {
        rlang::inform(settings_script_dir$message,
                      .frequency = "once",
                      .frequency_id = "read_file_proj_message")

      }
    }
  }

  if (sum(manual_settings, config_settings) == 0) {
    rlang::abort(paste0("You have not given enough variables to save the file properly.\n",
                        "Either add a correct config with current script_path in it or set at least",
                        "the dir and one of the write_* arguments"))
  }


  ## Set the dir variables based on manual or config settings
  if (is.null(base_dir) && config_settings == TRUE) {
    base_dir <- settings_script_dir[["base_data_dir"]]
  }

  if (is.null(add_branch) && config_settings == TRUE) {
    add_branch <- settings_script_dir[["add_branch"]]
  }

  ##' *INFO* System call to get the branch
  if (add_branch == TRUE) {
    branch <- system("git branch --show-current", intern = TRUE)
  } else {
    branch <- NA_character_
  }

  if (is.null(extension) && config_settings == TRUE) {
    extension <- settings_script_dir[["extension"]]
  }

  if (is.null(dir) && config_settings == TRUE) {
    dir <- settings_script_dir[["data_dir"]]
  }

  # Create full dir and file path
  if (!is.null(full_dir)) {
    dir_complete <- full_dir
  } else {
    dir_elements <- c(base_dir, branch, dir, sub_dir)
    dir_elements <- dir_elements[!is.na(dir_elements)]
    dir_complete <- paste(dir_elements, collapse = "/")
    dir_complete <- stringr::str_replace_all(dir_complete, stringr::fixed("//"), "/")
  }


  if (dir.exists(dir_complete) == FALSE) {
    rlang::abort(paste0("The constructed directory: ",
                        dir_complete,
                        " doesn't exist.\n",
                        "Change the input or config file.\n",
                        "For instance, use (base_)dir = '', to overwrite the (base_)dir in the config file."
    )
    )
  }

  file_name <- paste(name, extension, sep = ".")
  file_path_complete <- paste(dir_complete, file_name, sep = "/")
  file_path_complete <- stringr::str_replace_all(file_path_complete, stringr::fixed("//"), "/")



  # read with function based on extension
  if (extension == "rds") {
    if (fix_encoding == TRUE) {
      df <- fixencoding(readRDS(file_path_complete, ...), encoding)
    }
    else {
      df <- readRDS(file_path_complete, ...)
    }
  }


  if (extension == "csv") {

    ## Two names arguments to make names correct in base style
    ## readr default of ... gives issues
    function_args <- list(file = file_path_complete,
                          delim = ";",
                          name_repair = make.names,
                          escape_double = FALSE,
                          locale = readr::locale(decimal_mark = ",",
                                                 grouping_mark = "."),
                          trim_ws = TRUE,
                          col_types = readr::cols(.default = readr::col_guess()))

    ## Overwrite the arguments from the function_args with those from the dots (...)
    function_args <- overwrite_dot_arguments(function_args, ...)

    df <- do.call(readr::read_delim, function_args)

    if (csv_names_unique == TRUE) {
      ## Ensure name are unique
      names(df) <- make.unique(names(df))
    }

    if (fix_encoding) {
      df <- fixencoding(df, csv_encoding)
    }
  }

  if (extension == "fst") {
    if (fix_encoding) {
      df <- fixencoding(fst::read_fst(file_path_complete, ...) %>%
                          dplyr::mutate_if(is.character, .funs = function(x) {
                            return(`Encoding<-`(x, "UTF-8"))
                          }), encoding)
    }
    else {
      df <- fst::read_fst(file_path_complete, ...) %>% dplyr::mutate_if(is.character,
                                                                        .funs = function(x) {
                                                                          return(`Encoding<-`(x, "UTF-8"))
                                                                        })
    }
  }

  return(df)


}



write_file_proj <- function(
    object,
    name = NULL,
    settings_df = write_config_proj(),
    settings_type = NULL,
    sub_dir = NA_character_,
    base_dir = NULL,
    dir = NULL,
    add_branch = NULL,
    full_dir = NULL,
    extensions = NULL,
    extra_ext = NULL,
    rds_version = 3,
    csv_na = "",
    csv_sep = ";",
    csv_dec = ",",
    fst_compress = 100,
    ...
) {

  ## Set the object name to the file name if not given
  if (is.null(name)) {
    name <- deparse(substitute(object))
  }

  ## Get the script path relative to the working directory
  script_path <- stringr::str_remove(this.path::sys.path(), paste0(getwd(), "/"))

  # Set and then check the manual and config setting for completeness

  # Both either base_dir or dir AND one write must be set to be manual
  if (sum(max(!is.null(dir), !is.null(base_dir)), !is.null(extensions)) < 2) {
    manual_settings <- FALSE
  } else {
    manual_settings <- TRUE
  }

  if (is.null(settings_df)) {
    config_settings <- FALSE
  } else {
    config_settings <- TRUE
    settings_script_dir <- settings_df %>%
      dplyr::filter(str_detect(script_path, script_dir))

    # When the script_path is not in the config file set to FALSE
    if (nrow(settings_script_dir) == 0) {
      config_settings <- FALSE
    } else {
      # Some directories can have additional types
      # Deal with this type, this generates an error if type is incorrectly supplied
      settings_script_dir <- filter_settings_on_type(settings_script_dir, settings_type)

      if(!is.na(settings_script_dir$message)) {
        rlang::inform(settings_script_dir$message,
                      .frequency = "once",
                      .frequency_id = "write_file_proj_message")

      }
    }
  }

  # Abort if no settings
  if (sum(manual_settings, config_settings) == 0) {
    rlang::abort(paste0("You have not given enough variables to save the file properly.\n,
                          Either add a correct config with current directory in it or set at least
                          the dir and one of the write_* arguments"))
  }

  ## Set the dir variables based on manual or config settings
  if (is.null(base_dir) && config_settings == TRUE) {
    base_dir <- settings_script_dir[["base_data_dir"]]
  }

  if (is.null(add_branch) && config_settings == TRUE) {
    add_branch <- settings_script_dir[["add_branch"]]
  }

  ##' *INFO* System call to get the branch
  if (add_branch == TRUE) {
    branch <- system("git branch --show-current", intern = TRUE)
  } else {
    branch <- NA_character_
  }

  if (is.null(dir) && config_settings == TRUE) {
    dir <- settings_script_dir[["data_dir"]]
  }

  if (is.null(extensions) && config_settings == TRUE) {
    extensions <- settings_script_dir[["extensions"]]
  }

  if (!is.null(extra_ext)) {
    extensions <- c(extensions, extra_ext)
  }

  ## Create complete dir and file path, remove any empty values and double slashes
  if (!is.null(full_dir)) {
    dir_complete <- full_dir
  } else {
    dir_elements <- c(base_dir, branch, dir, sub_dir)
    dir_elements <- dir_elements[!is.na(dir_elements)]
    dir_complete <- paste(dir_elements, collapse = "/")
    dir_complete <- stringr::str_replace_all(dir_complete, stringr::fixed("//"), "/")
  }

  if (dir.exists(dir_complete) == FALSE) {
    rlang::abort(paste0("The constructed directory doesn't exist.\n",
                        'Run dir.create("',dir_complete, '", recursive = TRUE) or change the input or config file.\n',
                        "For instance, use (base_)dir = '', to overwrite the (base_)dir in the config file.")
    )
  }

  file_path_complete <- paste(dir_complete, name, sep = "/")
  file_path_complete <- stringr::str_replace_all(file_path_complete, stringr::fixed("//"), "/")
  message(file_path_complete)

  print(file_path_complete)

  ## write
  if ("csv" %in% extensions) {
    data.table::fwrite(object,
                       paste0(file_path_complete, ".csv"),
                       na = csv_na,
                       sep = csv_sep,
                       dec = csv_dec
    )
  }

  if ("fst" %in% extensions) {
    fst::write_fst(object,
                   paste0(file_path_complete, ".fst"),
                   compress = fst_compress,
                   ...)
  }

  if ("rds" %in% extensions) {
    saveRDS(object,
            paste0(file_path_complete,".rds"),
            version = rds_version,
            ...)
  }
}

