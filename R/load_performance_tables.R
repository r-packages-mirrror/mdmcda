#' @export
load_performance_tables <- function(input,
                                    extension="xlsx",
                                    regex=NULL,
                                    ignore="empty-performance-table",
                                    estimatesSheet="Estimates",
                                    confidencesSheet = "Confidences",
                                    sep=",",
                                    recursive=TRUE,
                                    silent=TRUE,
                                    ...) {

  if (!is.character(input) || !length(input)==1) {
    stop("Only specify a single string as 'input'!");
  }

  if (!dir.exists(input)) {
    stop("Directory provided to read from ('",
         input,
         "') does not exist!");
  }

  if (is.null(regex)) {
    regex <- paste0("^(.*)\\.", extension, "$");
  }

  if (grepl('\\.xls', regex)) {
    if (!requireNamespace("XLConnect", quietly = TRUE)) {
    #if (!requireNamespace("xlsx", quietly = TRUE)) {
      stop("To import from excel format, the \"XLConnect\" package is required. ",
           "It needs to be installed and it needs to be able to load ",
           "its dependency the \"rJava\" package. That package can only ",
           "load if it can find where you installed Java. So, you either need ",
           "to install the XLConnect package using `install.packages('XLConnect');`,",
           "or you need to install Java (make sure to install the version ",
           "matching your R version; so either 32-bit or 64-bit!).");
    }
  }

  ### Get list of files
  performanceTableFiles <-
    list.files(input,
               pattern=regex,
               full.names=TRUE,
               recursive = recursive);

  ### Remove files to ignore
  performanceTableFiles <-
    performanceTableFiles[!grepl(ignore,
                                 performanceTableFiles)];

  if (!silent) {
    cat("\nStarting to process the following list of files:\n\n",
        paste0("- ", performanceTableFiles, "\n"));
  }

  ### Load all performance tables
  res <- list(performance_subtables = list());
  for (filename in performanceTableFiles) {
    res$performance_subtables[[basename(filename)]] <-
      load_performance_table(filename,
                             estimatesSheet=estimatesSheet,
                             confidencesSheet=confidencesSheet,
                             sep=sep,
                             ...);
  }

  ### Extract each performance tables estimatorCode
  res$estimatorCodeVector <-
    unlist(lapply(res$performance_subtables,
                  function(x) {
                    return(attr(x, 'estimatorCode'));
                  }));

  res$estimatorCodeVector[is.na(res$estimatorCodeVector)] <-
    "all";

  res$estimatorCodes <- unique(na.omit(res$estimatorCodeVector));

  ### Store estimate dataframes
  res$estimates <-
    lapply(res$performance_subtables,
           estimates_from_performance_table);

  ### Set identifier legend to combine estimates in one dataframe
  res$multiEstimateLegend <-
    res$estimatorCodeVector;
  res$multiEstimateInverseLegend <-
    names(res$multiEstimateLegend);
  names(res$multiEstimateInverseLegend) <-
    res$multiEstimateLegend;

  ### Combine estimates in one dataframe
  res$multiEstimateDf <- data.frame(decision_id = character(),
                                    decision_label = character(),
                                    decision_alternative_value = character(),
                                    decision_alternative_label = character(),
                                    criterion_id = character(),
                                    criterion_label = character(),
                                    stringsAsFactors = FALSE);
  for (i in names(res$multiEstimateLegend)) {
    for (j in 1:nrow(res$estimates[[i]]$estimatesDf)) {
      rowNr <-
        which(res$multiEstimateDf$decision_id %in% res$estimates[[i]]$estimatesDf[j, 'decision_id'] &
              res$multiEstimateDf$decision_alternative_value %in% res$estimates[[i]]$estimatesDf[j, 'decision_alternative_value'] &
              res$multiEstimateDf$criterion_id %in% res$estimates[[i]]$estimatesDf[j, 'criterion_id']);
      if (is.null(rowNr) || !length(rowNr)) {
        rowNr <-
          nrow(res$multiEstimateDf) + 1;
      } else if (length(rowNr) > 1) {
        stop("Error: duplicate rows!");
      }
      res$multiEstimateDf[rowNr, 'decision_id'] <-
        res$estimates[[i]]$estimatesDf[j, 'decision_id'];
      res$multiEstimateDf[rowNr, 'decision_label'] <-
        res$estimates[[i]]$estimatesDf[j, 'decision_label'];
      res$multiEstimateDf[rowNr, 'decision_alternative_value'] <-
        res$estimates[[i]]$estimatesDf[j, 'decision_alternative_value'];
      res$multiEstimateDf[rowNr, 'decision_alternative_label'] <-
        res$estimates[[i]]$estimatesDf[j, 'decision_alternative_label'];
      res$multiEstimateDf[rowNr, 'criterion_id'] <-
        res$estimates[[i]]$estimatesDf[j, 'criterion_id'];
      res$multiEstimateDf[rowNr, 'criterion_label'] <-
        res$estimates[[i]]$estimatesDf[j, 'criterion_label'];
      res$multiEstimateDf[rowNr, res$multiEstimateLegend[i]] <-
        res$estimates[[i]]$estimatesDf[j, 'value'];
    }
  }

  ### Converting estimates to numeric
  res$multiEstimateDf[, res$estimatorCodes] <-
    lapply(res$estimatorCodes,
           function(i, x = res$multiEstimateDf[, i]) {
             suppressWarnings(y <- as.numeric(x));
             z <- as.character(y);
             ### z & z should now be equal. If not, z will have
             ### deviating values or missing values
             if (identical(x, z)) {
               return(y);
             } else {
               warning("One of the estimates loaded from '",
                       i, "' in file '",
                       res$multiEstimateInverseLegend[i],
                       "' cannot be converted to a numeric value.");
               return(x);
             }
           });

  ### Generate complete performance_tables
  res$performance_tables <- list();
  for (i in res$estimatorCodes) {
    res$performance_tables[[i]] <-
      estimateDf_to_performance_table(res$multiEstimateDf,
                                      valueCol = i);
    res$performance_tables[[i]][1,1] <- i;
  }

  numericValues <-
    res$estimatorCodes[
      unlist(lapply(res$multiEstimateDf[, res$estimatorCodes], is.numeric))
    ];

  if (length(numericValues) > 1) {

    res$multiEstimateDf$variance <-
      apply(res$multiEstimateDf[, numericValues],
            1,
            stats::var,
            na.rm=TRUE);

    res$multiEstimateDf$value_mean <-
      apply(res$multiEstimateDf[, numericValues],
            1,
            mean,
            na.rm=TRUE);

    res$performance_table_var <-
      estimateDf_to_performance_table(res$multiEstimateDf,
                                      valueCol = "variance");

    res$performance_table_mean <-
      estimateDf_to_performance_table(res$multiEstimateDf,
                                      valueCol = "value_mean");


    ### Get consensus matrix
    res$consensusDf <-
      res$multiEstimateDf[, c('criterion_label', 'variance')];
    res$consensusDf$selectedAlternative <-
      paste(res$multiEstimateDf$decision_label,
            res$multiEstimateDf$decision_alternative_label,
            sep=":\n");

    precision <- max(nchar(as.character(res$multiEstimateDf$value_mean)));

    res$consensusDf$formattedMean <-
      round(res$multiEstimateDf$value_mean,
            min(precision, 2));

    res$consensusDf$invertedVariance <-
      max(res$multiEstimateDf$variance) -
      res$multiEstimateDf$variance;

    res$consensusMap <-
      ggplot2::ggplot(res$consensusDf,
                      mapping=ggplot2::aes_string(x='criterion_label',
                                                  y='selectedAlternative',
                                                  fill='variance',
                                                  label = 'formattedMean')) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(mapping=ggplot2::aes_string(color='invertedVariance')) +
      ggplot2::scale_fill_viridis_c(name = "Variance\n(disagreement)") +
      ggplot2::scale_color_viridis_c(name = "Variance\n(disagreement)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, hjust=1)) +
      ggplot2::labs(title = "Consensus Map",
                    subtitle = paste0("Based on ", length(numericValues),
                                      " estimates from a DMCDA"),
                    x = "Criteria",
                    y = "Decisions");

  } else {
    consensusDf <- NA;
    consensusMap <- NA;
  }

  return(invisible(res));
}
