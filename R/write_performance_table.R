#' @export
write_performance_table <- function(performance_table,
                                    criteria,
                                    path = NULL,
                                    split = TRUE,
                                    fullTableFilenamePattern = "performance_table_by_%s",
                                    subTableFilenamePattern = "performance_subtable_for_%s_on_%s_by_%s",
                                    estimatorCodes = "all",
                                    ext = "csv",
                                    overwrite = FALSE,
                                    sep = ",",
                                    silent=TRUE,
                                    ...) {

  if (!dir.exists(path)) {
    stop("The directory specified where to write the file ('",
         path, "') does not exist!");
  }

  wantsXls <- grepl('xls', ext);

  if (!('performance_table' %in% class(performance_table))) {
    stop("As argument 'performance_table', you have to provide a ",
         "performance table (sorry if that was not clear :-)). You ",
         "provided an object with class ", vecTxtQ(performance_table),
         ".");
  }

  if (wantsXls) {
    if (!requireNamespace("xlsx", quietly = TRUE)) {
      stop("To export to excel format, the \"xlsx\" package is required. ",
           "It needs to be installed and it needs to be able to load ",
           "its dependency the \"rJava\" package. That package can only ",
           "load if it can find where you installed Java. So, you either need ",
           "to install the xlsx package using `install.packages('xlsx');`,",
           "or you need to install Java (make sure to install the version ",
           "matching your R version; so either 32-bit or 64-bit!).");
    } else {
      writeFun <- function(performance_table,
                           file) {
        xlsx::write.xlsx(performance_table,
                         file = file,
                         col.names = FALSE,
                         row.names = FALSE,
                         append = FALSE,
                         ...);
      }
    }
  } else {
    writeFun <- function(performance_table,
                         file) {
      utils::write.table(performance_table,
                         file = file,
                         col.names = FALSE,
                         row.names = FALSE,
                         append = FALSE,
                         sep=sep,
                         ...);
    }
  }

  if (split) {

    ### Get a list of the decisions
    decisionsDf <-
      data.frame(performance_table[3:nrow(performance_table), 1:4],
                 stringsAsFactors = FALSE);
    names(decisionsDf) <-
      c("decision_id", "alternative_id", "decision_label", "alternative_label");

    ### Get a list of criteria and their parents
    criteriaDf <-
      criteria$criteriaDf[criteria$criteriaDf$isLeaf, c("id", "parentCriterion", "label")];

    ### Add row and column numbers from performance table

    decisionsDf$row <-
      unlist(lapply(1:nrow(decisionsDf),
             function(i) {
               rowsWithDecision <-
                 which(performance_table[, 1] == decisionsDf[i, 'decision_id']);
               rowsWithAlternative <-
                 which(performance_table[, 2] == decisionsDf[i, 'alternative_id']);
               rowNr <- intersect(rowsWithDecision, rowsWithAlternative);
               if (!silent) {
                 cat0("\nRows with decision '", decisionsDf[i, 'decision_id'],
                      "' in performance table: ",
                      vecTxt(rowsWithDecision), ".");
                 cat0("\nRows with alternative '", decisionsDf[i, 'alternative_id'],
                      "' in performance table: ",
                      vecTxt(rowsWithAlternative), ".");
                 cat0("\nIntersection: ",
                      vecTxt(rowNr), ".");
               }
               if (length(rowNr) == 0) {
                 stop("There are no rows in the performance table for decision '",
                      decisionsDf[i, 'decision_id'],
                      "' and alternative ", decisionsDf[i, 'alternative_id'], ".");
               } else if (length(rowNr) > 1) {
                 stop("There are multiple rows in the performance table for decision '",
                      decisionsDf[i, 'decision_id'],
                      "' and alternative ", decisionsDf[i, 'alternative_id'],
                      ", specifically rows ", vecTxt(rowNr), ".");
               }
               return(intersect(rowsWithDecision, rowsWithAlternative));
             }));

    criteriaDf$col <-
      unlist(lapply(1:nrow(criteriaDf),
                    function(i) {
                      return(which(performance_table[1, ] == criteriaDf[i, 'id']));
                    }));

    performance_subtables <- list();
    ### Process the criteria
    for (i in unique(criteriaDf$parentCriterion)) {
      performance_subtables[[i]] <- list();
      ### Process the decisions
      for (j in unique(decisionsDf$decision_id)) {
        rows <- c(1:2, decisionsDf[decisionsDf$decision_id %in% j, 'row']);
        cols <- c(1:4, criteriaDf[criteriaDf$parentCriterion %in% i, 'col']);
        performance_subtables[[i]][[j]] <-
          performance_table[rows, cols];
        ### Write this performance subtable to a file
        for (estimatorCode in estimatorCodes) {
          ### Add estimator code
          performance_subtables[[i]][[j]][1,1] <-
            estimatorCode;
          writeFun(performance_subtables[[i]][[j]],
                   file=file.path(path,
                                  paste0(sprintf(subTableFilenamePattern,
                                                 j, i, estimatorCode),
                                         ".", ext)));
        }
      }
    }
    return(invisible(performance_subtables));

  } else {
    writeFun(performance_table,
             file=file.path(path,
                            paste0(sprintf(fullTableFilenamePattern,
                                           estimatorCodes[1]),
                                   ".", ext)));
    return(invisible(performance_table));
  }

}
