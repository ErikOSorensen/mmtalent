#' Data from mmtalent, an online experiment
#' A dataset from the online experiment used in the paper
#' "Talent and luck - An experimental study on inequality
#' acceptance" by Björn Bartling, Alexander W. Cappelen,
#' Ingvild L. Skarpeid, Erik Ø. Sørensen and Bertil Tungodden.
#'
#' @format A data frame with XXX observations and YY variables:
#' \describe{
#' \item{startdate}{Starting data for individual}
#' \item{durationinseconds} How long did participants take to complete study? (s)
#' \item{gender}
#' }
"mmtalent_df"


#' Census data on population in Agegroup x region x gender cells
#'
#'
#' @format A data frame with 40 observations and 4 variables:
#' \describe{
#' \item{region}{The four census regions}
#' \item{gender}{1: male, 2: female}
#' \item{age_category}{}
#' \item{n2016}{Population in 2016 (numeric)}
#' }
#' @source Based on the sc-est2016-alldata6.csv file from https://www2.census.gov/programs-surveys/popest/datasets/2010-2016/state/asrh/ downloaded November 28, 2017.
"census_df"
