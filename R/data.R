#' Dataset from exemplary simulational study
#'
#' A dataset containing the results of DSGC (Decentral Smart Grid Control) simulations for
#' symmetric star 5-node system with inflrxible producer
#' in the center. Three inputs for each of four consumers are varied: tau -- the reaction time, from [0.5, 5];
#' T -- averaging time, from [1, 4]; g -- 'price elasticity of demand', from [0.05, 1]. The points
#' are sampled from design space according to Halton sequence.
#'
#' @format dsgc_sym is a matrix with 13 columns and 10000 rows. each row corresponds to a separate simulation.
#' The first 12 rows are values of inputs and the last column is a binary linear stability indicator,
#' where 1 = stable and 0 = unstable.
#'
#' @usage data(dsgc_sym)
#'
#' @keywords datasets
#'
#' @references Schaefer, B. et al. 2015. Decentral smart grid control. New Journal of Physics. 17, 1 (2015), 15002.
#'
#' Schaefer, B. et al. 2016. Taming instabilities in power grid networks by decentralized control.
#' European Physical Journal: Special Topics. 225, 3 (2016), 569-582.
#'
#' Arzamasov, V. et al. 2018. Towards Concise Models of Grid Stability. 2018 IEEE International
#' Conference on Communications, Control, and Computing Technologies for Smart Grids, SmartGridComm 2018. 2 (2018).
"dsgc_sym"


#' Dataset for testing BestInterval algorithm
#'
#' @format bi_test is a matrix with 5 columns and 1000 rows. First 4 columns are attributes; the last column - label
#'
#' @usage data(bi_test)
"bi_test"
