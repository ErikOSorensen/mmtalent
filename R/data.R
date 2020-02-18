#' Data from mmtalent, an online experiment
#' A dataset from the online experiment used in the paper
#' "Talent and luck - An experimental study on inequality
#' acceptance" by Björn Bartling, Alexander W. Cappelen,
#' Ingvild L. Skarpeid, Erik Ø. Sørensen and Bertil Tungodden.
#'
#' @format A data frame with 2001 observations and 23 variables:
#' \describe{
#' \item{treatment}{Treatment (factor) \itemize{
#'  \item Ex Ante Impersonal
#'  \item Ex Ante Personal
#'  \item Ex Post Impersonal
#'  \item Ex Post Personal
#' }}
#' \item{timing}{ Ex Ante or Ex Post (factor)}
#' \item{personal}{ Impersonal or Personal (factor)}
#' \item{startdate}{Starting data for individual}
#' \item{durationinseconds}{How long did participants take to complete study? (s)}
#' \item{gender}{ "male" or "female" (characters)}
#' \item{age}{Age in years}
#' \item{age_category}{ (characters) \itemize{
#'  \item [18,35)
#'  \item [35,45),
#'  \item [45,55),
#'  \item [55,65),
#'  \item [65, 120)
#'  }
#' \item{state_residence}{State in which participant resides (characters)}
#' \item{region}{Census region in which participant resides ("Midwest", "Northeast","South", "West"; characters)}
#' \item{edu_category}{ Education categories (characters) }
#' \item{income_category}{ Income categories (characters) }
#' \item{luck_fair}{Are income differences due to luck unfair? Not incentivized. Numeric 0-10 scale, with 10 most unfair.}
#' \item{talent_fair}{Are income differences due to talent unfair? Not incentivized. Numeric 0-10 scale, with 10 most unfair.}
#' \item{effort_fair}{Are income differences due to effort unfair? Not incentivized. Numeric 0-10 scale, with 10 most unfair.}
#' \item{luck_control}{Is Luck within control? Not incentivized. Numeric 0-10 scale, with 10 completely under control.}
#' \item{talent_control}{Is Luck within control? Not incentivized. Numeric 0-10 scale, with 10 completely under control.}
#' \item{effort_control}{Is Luck within control? Not incentivized. Numeric 0-10 scale, with 10 completely under control.}
#' \item{redist_pref}{}
#' \item{polpref}{Political preference on a 1-5 numeric scale, with 1: most left, 5: most right.}
#' \item{redistribute}{Redistribution choice: How many dollars (0-6) to redistribute from winner to loser.}
#' \item{payment_low_worker}{Payment to worker (low), including baseline of 2, (numeric in [2-8])}
#' \item{payment_high_worker}{Payment to worker (high), including baseline of 2, (numeric in [2-8])}
#' }
#' @source Based on experiment by the authors.
"mmtalent_df"

#' Census data on population in Agegroup x region x gender cells
#'
#'
#' @format A data frame with 40 observations and 4 variables:
#' \describe{
#' \item{region}{The four census regions}
#' \item{gender}{"male" or "female", characters}
#' \item{age_category}{ (characters) \itemize{
#'  \item [18,35)
#'  \item [35,45),
#'  \item [45,55),
#'  \item [55,65),
#'  \item [65, 120)
#'  }}
#' \item{n2016}{Population in 2016 (numeric)}
#' }
#' @source Based on the sc-est2016-alldata6.csv file from https://www2.census.gov/programs-surveys/popest/datasets/2010-2016/state/asrh/ downloaded November 28, 2017.
"populationweights2016"
