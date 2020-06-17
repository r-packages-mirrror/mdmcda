#' @export
scoreBarchart_decisions_criteria <- function (weighedEstimates,
                                              scenario_id,
                                              estimateCol,
                                              strokeSize = .1,
                                              strokeColor = "black",
                                              title = paste0("DMCDA bar chart to compare decisions for ", scenario_id),
                                              xLab = "Decisions",
                                              yLab = "Weighed estimated effect",
                                              theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize")),
                                              guides = ggplot2::guide_legend(ncol = 2),
                                              legend.position = "bottom",
                                              legend.box.margin = ggplot2::margin(.5, .5, .5, .5, "cm")) {

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
    theme +
    ggplot2::guides(fill = guides,
                    color = guides) +
    ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90,
                                                              hjust = 1,
                                                              vjust = 0.5),
                   plot.title.position = "plot",
                   legend.position = legend.position,
                   legend.box.margin = legend.box.margin) +
    ggplot2::labs(title=title,
                  subtitle = paste0("Colours represent criteria, ",
                                    "separate rectangles per decision"),
                  x=xLab,
                  y=yLab) +
    NULL;
  return(res);
}
