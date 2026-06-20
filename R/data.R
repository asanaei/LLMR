# data.R --------------------------------------------------------------------
# Documentation for the bundled `anes_2024_personas` example dataset. The data
# itself is in data/anes_2024_personas.rda; the contract helpers are in
# personas.R. The reproducibility scripts live under inst/anes-data-prep/.

#' Example participant profiles derived from ANES 2024
#'
#' A ready-to-use set of 100 participant profiles, used across the LLMR family to
#' build example synthetic panels and discussions without downloading anything.
#' Each row is one respondent from the American National Election Studies (ANES)
#' 2024 Time Series Study, with demographics and a broad battery of political and
#' social attitudes decoded to their value labels. It is an ordinary data frame:
#' select columns and filter rows with [dplyr][dplyr::dplyr-package] or base R,
#' then hand the result to a consumer (for example `FocusGroup::create_agents_from_data()`
#' or `LLMRpanel::panel_from_personas()`). The helpers [llm_persona_split()],
#' [llm_persona_overview()], and [llm_persona_dictionary()] read the frame.
#'
#' @format A data frame with 100 rows and 125 columns. Column names are short,
#'   tidy-select-friendly handles. Demographics use a `demo_` prefix
#'   (`demo_age`, `demo_race_ethnicity`, ...). Attitudes use an `att_<block>_`
#'   prefix grouping them into a few blocks, so
#'   `dplyr::select(starts_with("att_iss_"))` grabs a whole block:
#'   \itemize{
#'     \item `att_id_` -- political identity (party id, ideology, vote)
#'     \item `att_aff_` -- feeling thermometers (candidates, parties, groups)
#'     \item `att_eval_` -- approval, the economy, personal finances, national mood
#'     \item `att_iss_` -- issue positions (taxes, immigration, guns, climate,
#'       health care, abortion, crime, race, foreign policy, trade, ...)
#'     \item `att_val_` -- trust, social trust, values, democracy
#'   }
#'   One numeric column, `ideology_score`, is a single conservative--liberal
#'   dimension (see Details); the rows are sorted by it. Attitude values are
#'   character labels (`NA` when missing or not applicable). The data frame
#'   carries a `"dictionary"` attribute (a data frame mapping each handle to its
#'   question wording, ANES variable code, and block) and a `"demographic_fields"`
#'   attribute marking the demographic columns.
#'
#' @details
#' The 100 respondents were chosen by diversity sampling (a greedy maximin pass
#' over a Gower distance, after dropping respondents missing more than a quarter
#' of the fields), so the set spans the range of demographic and attitudinal
#' profiles rather than reproducing population proportions. It is example
#' material; it is NOT a representative sample of the United States, carries no
#' survey weights, and should not be used for population inference. Each
#' respondent's own bundle of answers is kept intact (answers are not shuffled
#' across people), which is what makes a profile read as a coherent person.
#' Demographics are coarsened (age in bands, broad income and race categories,
#' census region rather than state); no respondent identifiers, granular
#' geography, open-ended text, or restricted-use variables are included. Items
#' concerning sexual orientation and gender identity are excluded by design.
#'
#' `ideology_score` is a unidimensional ideal point estimated from a graded-
#' response item-response model over the ordinal attitude items, standardized to
#' roughly mean 0 and unit scale, and oriented so that low is liberal and high is
#' conservative (it agrees with the first principal component at r ~= 0.99).
#'
#' The reproducibility scripts are under `inst/anes-data-prep/` in the installed
#' package; the raw ANES file is not bundled (it is freely available from ANES).
#'
#' @source Derived from the American National Election Studies. 2025. *ANES 2024
#'   Time Series Study Full Release* \[dataset and documentation\]. August 8,
#'   2025. \url{https://electionstudies.org/data-center/2024-time-series-study/}.
#'   A derived product of the ANES public release, distributed for example use;
#'   it contains no ANES respondent identifiers and no restricted-use data. Work
#'   that uses these personas should cite ANES as above. The ANES bears no
#'   responsibility for the analyses or interpretations presented here.
#' @examples
#' data(anes_2024_personas, package = "LLMR")
#' nrow(anes_2024_personas)
#' # the data frame is the interface: filter rows, select columns
#' \donttest{
#'   if (requireNamespace("dplyr", quietly = TRUE)) {
#'     library(dplyr)
#'     conservative <- dplyr::filter(anes_2024_personas, ideology_score > 0.5)
#'     nrow(conservative)
#'   }
#' }
"anes_2024_personas"
