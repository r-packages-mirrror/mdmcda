#' Options for the mdmcda package
#'
#' The `mdmcda::opts` object contains three functions to set, get, and reset
#' options used by the dmcda package. Use `mdmcda::opts$set` to set options,
#' `mdmcda::opts$get` to get options, or `mdmcda::opts$reset` to reset specific or
#' all options to their default values.
#'
#' It is normally not necessary to get or set `mdmcda` options.
#'
#' The following arguments can be passed:
#'
#' \describe{
#'   \item{...}{For `mdmcda::opts$set`, the dots can be used to specify the options
#'   to set, in the format `option = value`, for example,
#'   `varViewCols = c("values", "level")`. For
#'   `mdmcda::opts$reset`, a list of options to be reset can be passed.}
#'   \item{option}{For `mdmcda::opts$set`, the name of the option to set.}
#'   \item{default}{For `mdmcda::opts$get`, the default value to return if the
#'   option has not been manually specified.}
#' }
#'
#' The following options can be set:
#'
#' \describe{
#'
#'   \item{decisionId_col}{The default column name for the decision identifier}
#'   \item{decisionLabel_col}{The default column name for the decision label}
#'   \item{criterionId_col}{The default column name for the criterion identifier}
#'   \item{criterionLabel_col}{The default column name for the criterion label}
#'   \item{criterionDescription_col}{The default column name for the criterion description}
#'   \item{parentCriterionId_col}{The default column name for the parent criterion identifier}
#'   \item{alternativeValue_col}{The default column name for the alternaive value}
#'   \item{alternativeLabel_col}{The default column name for the alternative label}
#'   \item{scenarioId_col}{The default column name for the scenario identifier}
#'   \item{scenarioLabel_col}{The default column name for the scenario label}
#'   \item{decisionDescription_col}{The default column name for the decision description}
#'   \item{decisionAlternatives_col}{The default column name for the label combining a decision and the selected alternative}
#'   \item{weightProfileId_col}{The default column name for the decision identifier}
#'   \item{score_col}{The column with the scores returned by `scenario_scores`}
#'   \item{leafCriterion_col}{The name of the column name indicating whether a criterion is a leaf (without children) or not}
#'   \item{rootCriterionId}{The name of the root criterion in the criaria tree}
#'
#' }
#'
#' @aliases opts set get reset
#'
#' @usage opts
#'
#' @examples ### Get the default columns in the variable view
#' mdmcda::opts$get(varViewCols);
#'
#' ### Set it to a custom version
#' mdmcda::opts$set(varViewCols = c("values", "level"));
#'
#' ### Check that it worked
#' mdmcda::opts$get(varViewCols);
#'
#' ### Reset this option to its default value
#' mdmcda::opts$reset(varViewCols);
#'
#' ### Check that the reset worked, too
#' mdmcda::opts$get(varViewCols);
#'
#' @export
opts <- list();

opts$set <- function(...) {
  dots <- list(...);
  dotNames <- names(dots);
  names(dots) <-
    paste0("mdmcda.", dotNames);
  if (all(dotNames %in% names(opts$defaults))) {
    do.call(options,
            dots);
  } else {
    stop("Option ", vecTxtQ(dotNames),
         " is/are not a valid (i.e. existing) option for the `mdmcda` package!!");
  }
}

opts$get <- function(option, default=FALSE) {
  option <- as.character(substitute(option));
  if (!(option %in% names(opts$defaults))) {
    stop("Option '", option,
         "' is not a valid (i.e. existing) option for the `mdmcda` package!!");
  } else {
    return(getOption(paste0("mdmcda.", option),
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
      paste0("mdmcda.", optionNames);
    if (all(optionNames %in% names(opts$defaults))) {
      do.call(opts$set,
              opts$defaults[optionNames]);
    } else {
      invalidOptions <-
        !(optionNames %in% names(opts$defaults));
      stop("Option(s) ", vecTxtQ(optionNames[invalidOptions]),
           "' is/are not a valid (i.e. existing) option for the `mdmcda` package!");
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
      mdmcda::opts$set(ggSaveFigWidth = width,
                       ggSaveFigHeight = height,
                       ggSaveUnits = "in");
    }

    if (setFontSize) {
      mdmcda::opts$set(ggBaseSize = round(fontSizeMultiplier * max(c(height, width))));
    }

    return(invisible(c(width = width,
                       height = height)));

  }

opts$defaults <-
  list(

    decisionId_col           = "decision_id",
    decisionLabel_col        = "decision_label",
    criterionId_col          = "criterion_id",
    criterionLabel_col       = "criterion_label",
    criterionDescription_col = "criterion_description",
    parentCriterionId_col    = "parentCriterion_id",
    parentCriterionLabel_col    = "parentCriterionLabel_col",
    alternativeValue_col     = "alternative_value",
    alternativeLabel_col     = "alternative_label",
    scenarioId_col           = "scenario_id",
    scenarioLabel_col        = "scenario_label",
    decisionDescription_col  = "decision_description",
    decisionAlternatives_col = "decision_alternatives",
    weightProfileId_col      = "weight_profile_id",
    estimate_col             = "estimate_col",
    score_col                = "score",
    leafCriterion_col        = "leafCriterion",

    rootCriterionId          = "outcomes",

    performanceTable_decisionRegex = c("performance_subtable_for_(.*)_on_.*\\.xlsx$",
                                       "\\1"),
    performanceTable_criterionRegex = c("performance_subtable_for_.*_on_(.*)\\.xlsx$",
                                        "\\1"),

    ### ggSave defaults
    ggSaveFigWidth = 11,
    ggSaveFigHeight = 7.5,
    ggSaveUnits = "in",
    ggSaveDPI = 300,
    ggBaseSize = 16,

    ### Silence
    quiet = TRUE,
    silent = TRUE,

    ### Whether to prevent overwriting
    preventOverwriting = TRUE,

    ### Whether you want extra information, as for debugging
    debug = FALSE

  )

