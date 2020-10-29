#' @export
scoreBarchart_criteria <- function(estimatesByCriterion,
                                   estimateCol,
                                   fill = "black",
                                   strokeColor = "black",
                                   strokeSize = .1,
                                   title = "MDMCDA criteria bar chart",
                                   xLab = "Criteria",
                                   yLab = estimateCol,
                                   criteriaOrder = NULL,
                                   criteriaLabels = NULL,
                                   criteriaLabelCol = NULL,
                                   parentCriterionOrder = NULL,
                                   parentCriterionIds = NULL,
                                   parentCriterionLabels = NULL,
                                   xLabelRotationAngle = 45,
                                   theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize")),
                                   guides = ggplot2::guide_legend(nrow = 2),
                                   legend.position = "top",
                                   legend.box.margin = ggplot2::margin(.5, .5, .5, .5, "cm")) {



  if (is.null(criteriaLabelCol)) {
    criteriaLabelCol <- "criteriaLabelCol";

    if (is.null(criteriaLabels)) {
      estimatesByCriterion[, criteriaLabelCol] <-
        estimatesByCriterion$criterion_id;
    } else {
      estimatesByCriterion[, criteriaLabelCol] <-
        criteriaLabels;
    }

  }

  if (is.null(criteriaOrder)) {
    criteriaOrder <-
      estimatesByCriterion$criterion_id;
    row.names(estimatesByCriterion) <-
      estimatesByCriterion$criterion_id;
  } else {
    if (is.character(criteriaOrder) &&
        (length(criteriaOrder) == 1)) {
      if ((tolower(criteriaOrder) == "decreasing") ||
          (tolower(criteriaOrder) == "increasing")) {
        criteriaOrder <-
          estimatesByCriterion[order(estimatesByCriterion[, estimateCol],
                                     decreasing = (tolower(criteriaOrder) == "decreasing")),
                               "criterion_id"];
        row.names(estimatesByCriterion) <-
          criteriaOrder;
      } else {
        stop("If `criteriaOrder` is not 'increasing' or 'decreasing', it ",
             "must be a character vector with the criterion_id values ",
             "in the desired order!");
      }
    }
    estimatesByCriterion <- estimatesByCriterion[criteriaOrder, ];
  }

  estimatesByCriterion$criterion_id <-
    factor(estimatesByCriterion$criterion_id,
           levels = criteriaOrder,
           labels = estimatesByCriterion[criteriaOrder, criteriaLabelCol],
           ordered = TRUE);

  if (!is.null(parentCriterionIds)) {
    estimatesByCriterion$cluster <-
      parentCriterionIds[
        estimatesByCriterion$criterion_id
      ];

    if (is.null(parentCriterionLabels)) {
      parentCriterionLabels <- unique(parentCriterionIds);
      names(parentCriterionLabels) <- parentCriterionLabels;
    }

    estimatesByCriterion$clusterLabel <-
      factor(
        estimatesByCriterion$cluster,
        levels = unique(estimatesByCriterion$cluster),
        labels = parentCriterionLabels[unique(estimatesByCriterion$cluster)],
        ordered = TRUE
      );

    # estimatesByCriterion$clusterLabel <-
    #   parentCriterionLabels[
    #     estimatesByCriterion$cluster
    #   ];
    #
    # estimatesByCriterion$clusterLabel <-
    #   factor(
    #     estimatesByCriterion$clusterLabel,
    #     levels = unique(estimatesByCriterion$clusterLabel),
    #     labels = parentCriterionLabels[unique(estimatesByCriterion$clusterLabel)],
    #     ordered = TRUE
    #   );

    res <-
      ggplot2::ggplot(data = estimatesByCriterion,
                      mapping = ggplot2::aes_string(x = "criterion_id",
                                                    y = estimateCol,
                                                    fill = "clusterLabel")) +
      ggplot2::geom_col(color = strokeColor,
                        size = strokeSize);

  } else {
    res <-
      ggplot2::ggplot(data = estimatesByCriterion,
                      mapping = ggplot2::aes_string(x = "criterion_id",
                                                    y = estimateCol)) +
      ggplot2::geom_col(fill = fill,
                        color = strokeColor,
                        size = strokeSize);
  }

  res <-
    res +
    theme +
    ggplot2::scale_fill_viridis_d(end=.9, name="Criteria Clusters") +
    ggplot2::theme_minimal() +
    ggplot2::guides(fill = guides,
                    color = guides) +
    ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = xLabelRotationAngle,
                                                              hjust = 1,
                                                              vjust = 0.6),
                   plot.title.position = "plot",
                   legend.position = legend.position,
                   legend.box.margin = legend.box.margin) +
    ggplot2::labs(title = title,
                  x = xLab,
                  y = yLab) +
    NULL;

  return(res);

}
