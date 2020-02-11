#' @export
scoreBarchart_decisions_criteria <- function (weighedEstimates,
                                              scenario_id,
                                              estimateCol,
                                              strokeSize = .1,
                                              strokeColor = "black",
                                              title = "DMCDA comparison bar chart") {

  res <-
    ggplot2::ggplot(data = weighedEstimates[weighedEstimates$scenario_id == scenario_id,
                                            c("decision_id",
                                              "criterion_id",
                                              estimateCol)],
                    mapping = ggplot2::aes_string(x='decision_id',
                                                  y=estimateCol,
                                                  fill='criterion_id')) +
    ggplot2::geom_col(color =strokeColor,
                      size=strokeSize) +
    ggplot2::scale_fill_viridis_d(name = "Criterion") +
    ggplot2::scale_x_discrete(position="bottom") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90,
                                                              hjust = 1,
                                                              vjust = 0.5)) +
    ggplot2::labs(title=title,
                  subtitle = paste0("Colours represent criteria, ",
                                    "separate rectangles per decision"),
                  x="Scenario",
                  y="Weighed estimated effect") +
    NULL;
  return(res);
}
