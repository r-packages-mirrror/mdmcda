#' @export
weight_based_sensitivity_analysis <- function(multiEstimateDf,
                                              weightsMeansAndSDs,
                                              weighedEstimates,
                                              criteria,
                                              scenarioDefinitions,
                                              scenarioOrder = names(scenarioDefinitions),
                                              scenarioLabels = stats::setNames(names(scenarioDefinitions),
                                                                               names(scenarioDefinitions)),
                                              scorer = "all",
                                              weightCols = c(raw = 'weight_mean_proportion',
                                                             rescaled = 'weight_mean_rescaled_proportion'),
                                              steps = 10,
                                              silent = mdmcda::opts$get("silent"),
                                              lineSize = 1,
                                              theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize"))) {

  criteriaClusters <-
    names(criteria$criteriaTree$children);

  ### We process all weight profiles, and for each weight
  ### profile, we process all criteria clusters.

  res <-
    lapply(
      weightCols,
      function(weightCol) {

        ### Get the weights of each cluster in this column
        criteriaClusterWeights <-
          stats::setNames(weightsMeansAndSDs[criteriaClusters,
                                             weightCol],
                          criteriaClusters);

        ### Set the steps
        oneStep <- criteriaClusterWeights / steps;

        ### Compute fractions
        weightFractionsPerStep <-
          sapply(0:steps,
                 function(i) return(1 - (i*(1/steps))));
        weightFractionsPerStepChar <- as.character(weightFractionsPerStep);

        ### Multiply each fraction with each cluster weight
        weightsPerStep <-
          sapply(weightFractionsPerStep, `*`, criteriaClusterWeights)
        colnames(weightsPerStep) <- weightFractionsPerStepChar;

        weightsPerCluster <-
          lapply(criteriaClusters,
                 function(cC) {
                   res <- weightsPerStep;
                   res[cC, ] <- criteriaClusterWeights[cC];
                   return(res);
                 });
        names(weightsPerCluster) <- criteriaClusters;

        ### Now process all criteria clusters
        res <-
          lapply(
            criteriaClusters,
            function(criteriaClusterName) {

              res <- list();

              ### Loop through the steps (the weight fractions)
              res$resultsPerWeightFraction <-
                lapply(
                  weightFractionsPerStepChar,
                  function(weightFraction) {

                    res <- list();

                    ### Now we create a new temporary weightsMeansAndSDs, where
                    ### we replaced the criteria cluster's weights in `weightCol`
                    ### with those in `weightsPerCluster[, weightFraction]`. We
                    ### then send it to `combine_weights_and_criteria()`, generate
                    ### a weight profile, and recompute the scores for each scenario
                    ### in scenarioDefinitions. We then compute each scenario's
                    ### rank, and finally, create two plots.

                    res$weightsMeansAndSDs <- weightsMeansAndSDs;

                    if (!silent) {
                      cat0(
                        "\nReplacing scores in column '", weightCol, "': ",
                        vecTxt(
                          paste0(
                            round(as.numeric(
                              res$weightsMeansAndSDs[criteriaClusters, weightCol]),
                              3),
                            " with ",
                            round(as.numeric(
                              weightsPerCluster[[criteriaClusterName]][criteriaClusters, weightFraction]),
                              3))),
                        "\n");
                    }

                    res$weightsMeansAndSDs[criteriaClusters, weightCol] <-
                      weightsPerCluster[[criteriaClusterName]][criteriaClusters, weightFraction];

                    res$combinedWeightsAndCriteria <-
                      combine_weights_and_criteria(weightsMeansAndSDs = res$weightsMeansAndSDs,
                                                   criteria = criteria,
                                                   weightCols = stats::setNames(weightCol,
                                                                                weightCol));

                    res$weightProfiles <-
                      mdmcda::create_weight_profile(weightsMeansAndSDs = res$combinedWeightsAndCriteria$weightsMeansAndSDs,
                                                   criteria = res$combinedWeightsAndCriteria$criteria,
                                                   profileName = "sensitivityAnalysis",
                                                   weightCol = "weight_mean_rescaled_proportion_total_percentage",
                                                   clusterWeightCol = "weight_mean_rescaled_proportion_product");

                    res$weighedEstimates <-
                      mdmcda::add_weights(weighedEstimates = weighedEstimates,
                                         weightProfiles = res$weightProfiles,
                                         weightProfileNames = names(res$weightProfiles));

                    res$scoresPerScenario <-
                      mdmcda::scores_by_scenario(weighedEstimates = res$weighedEstimates,
                                                estimateCols = paste0(names(res$weightProfiles),
                                                                      '_weighed_estimate'));

                    res$scoresPerScenario$score <-
                      res$scoresPerScenario$sensitivityAnalysis_weighed_estimate;
                    res$scoresPerScenario$rank <-
                      rank(res$scoresPerScenario$sensitivityAnalysis_weighed_estimate);
                    res$scoresPerScenario$scenario_id <-
                      factor(res$scoresPerScenario$scenario_id,
                             levels = scenarioOrder,
                             labels = scenarioLabels[scenarioOrder],
                             ordered=TRUE);
                    res$scoresPerScenario$weightFraction <-
                      factor(weightFraction,
                             levels = weightFractionsPerStepChar,
                             labels = weightFractionsPerStepChar,
                             ordered = TRUE);

                    res$scoresPerScenario <-
                      res$scoresPerScenario[, c("scenario_id",
                                                "weightFraction",
                                                "score",
                                                "rank")];

                    return(res);

                  });
              names(res$resultsPerWeightFraction) <-
                weightFractionsPerStep;

              ### In `res`, we now have a list with the scores per scenario for all
              ### weight fractions. We can now use those to create the plots.

              res$scoresPerScenarioDf <-
                do.call(rbind,
                        lapply(res$resultsPerWeightFraction,
                               function(x) {
                                 return(x$scoresPerScenario);
                               }));

              res$scorePlot <-
                ggplot2::ggplot(data = res$scoresPerScenarioDf,
                                mapping = ggplot2::aes_string(x = "weightFraction",
                                                              y = "score",
                                                              group = "scenario_id",
                                                              color = "scenario_id")) +
                ggplot2::geom_line(size=lineSize) +
                ggplot2::scale_color_viridis_d(end=.9) +
                ggplot2::labs(x = "Multiplier of weights of all other criteria clusters",
                              y = "Scores") +
                theme;

              rankBreaks <-
                sort(unique(res$scoresPerScenarioDf$rank));
              rankLabels <-
                c("Worst", rep("", length(rankBreaks) - 2), "Best");

              res$rankPlot <-
                ggplot2::ggplot(data = res$scoresPerScenarioDf,
                                mapping = ggplot2::aes_string(x = "weightFraction",
                                                              y = "rank",
                                                              group = "scenario_id",
                                                              color = "scenario_id")) +
                ggplot2::geom_line(size=lineSize) +
                ggplot2::scale_color_viridis_d(end=.9) +
                ggplot2::scale_y_continuous(breaks=rankBreaks,
                                            labels=rankLabels) +
                ggplot2::labs(x = "Multiplier of weights of all other criteria clusters",
                              y = "Ranks") +
                theme;

              return(res);

            });

        names(res) <-
          criteriaClusters;

        return(res);

      });

  names(res) <-
    weightCols;

  return(res);

}


