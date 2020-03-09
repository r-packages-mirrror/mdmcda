#' Options for the dmcda package
#'
#' The `dmcda::opts` object contains three functions to set, get, and reset
#' options used by the dmcda package. Use `dmcda::opts$set` to set options,
#' `dmcda::opts$get` to get options, or `dmcda::opts$reset` to reset specific or
#' all options to their default values.
#'
#' It is normally not necessary to get or set `dmcda` options.
#'
#' The following arguments can be passed:
#'
#' \describe{
#'   \item{...}{For `dmcda::opts$set`, the dots can be used to specify the options
#'   to set, in the format `option = value`, for example,
#'   `varViewCols = c("values", "level")`. For
#'   `dmcda::opts$reset`, a list of options to be reset can be passed.}
#'   \item{option}{For `dmcda::opts$set`, the name of the option to set.}
#'   \item{default}{For `dmcda::opts$get`, the default value to return if the
#'   option has not been manually specified.}
#' }
#'
#' The following options can be set:
#'
#' \describe{
#'
#'   \item{varViewCols}{The order and names of the columns to include in the
#'   variable view.}
#'
#'   \item{showLabellerWarning}{Whether to show a warning if labeller labels
#'   are encountered.}
#'
#' }
#'
#' @aliases opts set get reset
#'
#' @usage opts
#'
#' @examples ### Get the default columns in the variable view
#' dmcda::opts$get(varViewCols);
#'
#' ### Set it to a custom version
#' dmcda::opts$set(varViewCols = c("values", "level"));
#'
#' ### Check that it worked
#' dmcda::opts$get(varViewCols);
#'
#' ### Reset this option to its default value
#' dmcda::opts$reset(varViewCols);
#'
#' ### Check that the reset worked, too
#' dmcda::opts$get(varViewCols);
#'
#' @export
opts <- list();

opts$set <- function(...) {
  dots <- list(...);
  dotNames <- names(dots);
  names(dots) <-
    paste0("dmcda.", dotNames);
  if (all(dotNames %in% names(opts$defaults))) {
    do.call(options,
            dots);
  } else {
    stop("Option ", vecTxtQ(dotNames), " is/are not a valid (i.e. existing) option for dmcda!");
  }
}

opts$get <- function(option, default=FALSE) {
  option <- as.character(substitute(option));
  if (!(option %in% names(opts$defaults))) {
    stop("Option '", option, "' is not a valid (i.e. existing) option for dmcda!");
  } else {
    return(getOption(paste0("dmcda.", option),
                     opts$defaults[[option]]));
  }
}

opts$reset <- function(...) {
  optionNames <-
    unlist(lapply(as.list(substitute(...())),
                  as.character));
  if (length(optionNames) == 0) {
    do.call(opts$set,
            opts$defaults);
  } else {
    prefixedOptionNames <-
      paste0("dmcda.", optionNames);
    if (all(optionNames %in% names(opts$defaults))) {
      do.call(opts$set,
              opts$defaults[optionNames]);
    } else {
      invalidOptions <-
        !(optionNames %in% names(opts$defaults));
      stop("Option(s) ", vecTxtQ(optionNames[invalidOptions]),
           "' is/are not a valid (i.e. existing) option for dmcda!");
    }
  }
}

opts$ez <- list();
opts$ez$figSize <-
  function(size = c("A4", "slide", "A4slide"),
           margin = 1,
           marginUnit = "cm",
           portrait = FALSE,
           setOption = TRUE,
           setFontSize = TRUE,
           fontSizeMultiplier = 1.5) {

    if ("a4slide" %in% tolower(size[1])) {
      width <- 29.7/2.54;
      height <- (29.7 / (16/9))/2.54;
    } else if ("slide" %in% tolower(size[1])) {
      width <- 40/3;
      height <- 7.5;
    } else if ("a4" %in% tolower(size[1])) {
      width <- 29.7/2.54;
      height <- 21/2.54;
    }

    if (tolower(marginUnit) == "mm")
      margin <- margin / 100;
    if (tolower(marginUnit) == "cm")
      margin <- margin / 2.54;

    if (portrait) {
      width <- height - 2*margin;
      height <- width - 2*margin;
    } else {
      width <- width - 2*margin;
      height <- height - 2*margin;
    }

    if (setOption) {
      dmcda::opts$set(ggSaveFigWidth = width,
                      ggSaveFigHeight = height,
                      ggSaveUnits = "in");
    }

    if (setFontSize) {
      dmcda::opts$set(ggBaseSize = round(fontSizeMultiplier * max(c(height, width))));
    }

    return(invisible(c(width = width,
                       height = height)));

  }

opts$defaults <-
  list(

    ### ggSave defaults
    ggSaveFigWidth = 11,
    ggSaveFigHeight = 7.5,
    ggSaveUnits = "in",
    ggSaveDPI = 300,
    ggBaseSize = 16,

    ### Silence
    silent = TRUE,

    ### Whether you want extra information, as for debugging
    debug = FALSE

  )

