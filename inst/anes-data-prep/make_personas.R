# make_personas.R -----------------------------------------------------------
# Reproducibility script for the bundled dataset `anes_2024_personas`. It is
# shipped (under inst/) for transparency, not run at build or install time.
#
# It builds the dataset from the public ANES 2024 Time Series Study Stata file,
# which is NOT bundled (it is freely available from ANES; see below). Download
# the Full Release `.dta`, then run this script from the package root with the
# path to that file, e.g.
#
#   ANES_DTA=~/Downloads/anes_timeseries_2024_stata.dta \
#     Rscript inst/anes-data-prep/make_personas.R
#
# or set the path inline below. Output: ~100 real ANES respondents, selected by
# diversity sampling for example coverage (NOT a representative sample),
# demographics coarsened, each respondent's attitude bundle kept intact, decoded
# to value labels, no ANES case IDs retained.
#
# Data source (please cite):
#   American National Election Studies. 2025. ANES 2024 Time Series Study Full
#   Release [dataset and documentation]. August 8, 2025.
#   https://electionstudies.org/data-center/2024-time-series-study/
#
# Variable selection: inst/anes-data-prep/persona_spec.R. Diversity sampling:
# block -> Gower distance -> seed required coverage -> greedy maximin (k-center).

suppressPackageStartupMessages({
  library(haven)
})

# Path to the ANES 2024 Time Series Stata file: from $ANES_DTA, else edit here.
DTA  <- Sys.getenv("ANES_DTA", "")
SPEC <- file.path("inst", "anes-data-prep", "persona_spec.R")
N_SHIP <- 100L
SEED <- 110L   # tie-breaks / stochastic steps only; selection is deterministic given data

if (!nzchar(DTA) || !file.exists(DTA)) {
  stop("Set ANES_DTA to the ANES 2024 Time Series .dta path ",
       "(download the Full Release from electionstudies.org). Looked for: ",
       if (nzchar(DTA)) DTA else "<unset>")
}
stopifnot(file.exists(SPEC))
source(SPEC)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- 1. read + decode the selected columns ---------------------------------
all_codes <- c(vapply(anes_demographics, `[[`, "", "code"),
               vapply(anes_attitudes,    `[[`, "", "code"))
raw <- haven::read_dta(DTA, col_select = dplyr::all_of(all_codes))

# variable label (question wording) per code, before decoding
var_label <- function(code) attr(raw[[code]], "label") %||% code

decode <- function(col) {
  if (inherits(col, c("haven_labelled", "labelled"))) {
    as.character(haven::as_factor(col, levels = "labels"))
  } else as.character(col)
}
num_of <- function(col) suppressWarnings(as.numeric(zap_labels(col)))

# --- 2. missing-value recoding ---------------------------------------------
is_missing_label <- function(x) {
  pat <- paste(anes_na_value_patterns, collapse = "|")
  grepl(pat, x, ignore.case = TRUE)
}
clean_label <- function(x) {
  x <- trimws(as.character(x))
  x[is_missing_label(x)] <- NA
  # strip leading "1. "/"-9. " code prefixes (allow stray leading whitespace)
  x <- gsub("^\\s*[-0-9]+\\.\\s*", "", x)
  # strip a trailing "(...)" note, and also a trailing UNCLOSED "(..." clause
  # (some ANES value labels run an open paren of coding caveats off the end)
  x <- gsub("\\s*\\([^)]*\\)\\s*$", "", x)
  x <- gsub("\\s*\\([^)]*$", "", x)
  x <- trimws(x)
  x[!nzchar(x %||% "")] <- NA
  x
}

# thermometers and numeric scales: drop the admin/DK numeric codes, keep 0-100
clean_numeric <- function(col) {
  v <- num_of(col)
  v[v %in% anes_na_numeric] <- NA
  v
}

# a variable is "thermometer-like" if its label says so or its range is 0-100
is_thermo <- function(code) grepl("thermometer|feeling toward", var_label(code), ignore.case = TRUE) ||
  grepl("^V241156$|^V241157$|^V241166$|^V241167$|^V241168$|^V242143$|^V242145$|^V242139$|^V242142$|^V242140$|^V242141$|^V242150$|^V242152$|^V242153$|^V242146$|^V242147$|^V242149$|^V242156$", code)

# --- 3. assemble a decoded, labeled character frame -------------------------
field_key   <- list()  # code -> shown key/question
field_group <- list()  # code -> block for distance (demo handled separately)
dat <- list()

for (d in anes_demographics) {
  code <- d$code; key <- d$key
  field_key[[code]] <- key; field_group[[code]] <- "demographics"
  if (identical(d$coarsen, "age_band")) {
    age <- clean_numeric(raw[[code]])
    band <- cut(age, breaks = c(-Inf, 24, 34, 44, 54, 64, Inf),
                labels = c("18-24","25-34","35-44","45-54","55-64","65+"))
    dat[[key]] <- as.character(band)
  } else if (identical(d$coarsen, "any_children")) {
    n <- clean_numeric(raw[[code]])
    dat[[key]] <- ifelse(is.na(n), NA, ifelse(n > 0, "Yes", "No"))
  } else {
    dat[[key]] <- clean_label(decode(raw[[code]]))
  }
}

for (a in anes_attitudes) {
  code <- a$code; key <- a$q
  field_key[[code]] <- key; field_group[[code]] <- a$domain
  if (is_thermo(code)) {
    v <- clean_numeric(raw[[code]])
    # render thermometers as a short verbal band for prose readability
    dat[[key]] <- cut(v, breaks = c(-Inf, 15, 40, 60, 85, Inf),
                      labels = c("very cold (0-15)","cool (16-40)","neutral (41-60)",
                                 "warm (61-85)","very warm (86-100)")) |> as.character()
  } else {
    dat[[key]] <- clean_label(decode(raw[[code]]))
  }
}

decoded <- as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE)

# --- 4. completeness filter (>= 75% of persona fields non-missing) ----------
field_complete <- rowMeans(!is.na(decoded))
elig <- which(field_complete >= 0.75)
message(sprintf("Eligible respondents (>=75%% complete): %d of %d", length(elig), nrow(decoded)))
pool <- decoded[elig, , drop = FALSE]

# --- 5. diversity sampling: blocked Gower distance + greedy maximin ----------
# Vectorized k-center. Encode each column ONCE into a numeric matrix:
#   - ordinal-looking scale -> rank in [0,1] (squared-diff contribution)
#   - nominal -> integer codes (mismatch contribution)
# Fold the per-block weight into each column. Distance between two rows is the
# weighted average over columns where BOTH are observed. We keep a running
# min-distance vector and update it against only the LAST added point each step,
# so the fill is O(N_ship * |pool| * cols) instead of cubic.
block_of <- vapply(names(pool), function(k) {
  code <- names(field_key)[match(k, unlist(field_key))]
  field_group[[code]] %||% "issue"
}, "")
block_w <- c(demographics = 0.25, identity = 0.25, affect = 0.20,
             evaluation = 0.15, issue = 0.15, values = 0.15)
bw <- block_w[unique(block_of)]; bw <- bw / sum(bw)
col_w <- vapply(block_of, function(b) bw[[b]] %||% 0, 0)

col_levels <- lapply(pool, function(x) sort(unique(x[!is.na(x)])))
ord_hint <- function(lv) {
  any(grepl("[0-9]", lv)) ||
    any(grepl("strong|very|extremely|always|never|much|favor|oppose|increase|decrease|more|less|better|worse|approve|disapprove",
              lv, ignore.case = TRUE))
}
n <- nrow(pool); P <- ncol(pool)
COORD <- matrix(NA_real_, n, P)   # ordinal coordinate in [0,1] (NA if nominal/const/missing)
CODE  <- matrix(NA_integer_, n, P) # nominal integer code (NA if ordinal/const/missing)
is_ord <- logical(P)
for (j in seq_len(P)) {
  x <- pool[[j]]; lv <- col_levels[[j]]
  if (length(lv) <= 1 || col_w[j] == 0) next
  if (ord_hint(lv) && length(lv) <= 12) {
    is_ord[j] <- TRUE
    COORD[, j] <- (match(x, lv) - 1) / (length(lv) - 1)
  } else {
    CODE[, j] <- match(x, lv)
  }
}
active <- which(col_w > 0 & (is_ord | apply(!is.na(CODE), 2, any)))

# distance from every row to a single reference row `r` (vectorized over rows)
dist_to <- function(r) {
  num <- numeric(n); den <- numeric(n)
  for (j in active) {
    w <- col_w[j]
    if (is_ord[j]) {
      a <- COORD[, j]; b <- COORD[r, j]
      ok <- !is.na(a) & !is.na(b)
      num[ok] <- num[ok] + w * abs(a[ok] - b)
      den[ok] <- den[ok] + w
    } else {
      a <- CODE[, j]; b <- CODE[r, j]
      ok <- !is.na(a) & !is.na(b)
      num[ok] <- num[ok] + w * (a[ok] != b)
      den[ok] <- den[ok] + w
    }
  }
  ifelse(den == 0, 0, num / den)
}

# seed coverage: one respondent per major party-id / race / ideology band
seed_idx <- integer(0)
seed_on <- function(col_key, want) {
  if (!col_key %in% names(pool)) return(invisible())
  x <- pool[[col_key]]
  for (w in want) {
    hit <- setdiff(which(grepl(w, x, ignore.case = TRUE)), seed_idx)
    if (length(hit)) seed_idx <<- c(seed_idx, hit[[1]])
  }
}
seed_on("Party identification", c("Strong Democrat","Not very strong Democrat","Independent","Not very strong Republican","Strong Republican"))
seed_on("race/ethnicity", c("White","Black","Hispanic","Asian","American Ind","Multiple"))
seed_on("Liberal-conservative self-placement", c("liberal","Moderate","conservative"))
seed_idx <- unique(seed_idx)

set.seed(SEED)
selected <- if (length(seed_idx)) seed_idx else sample.int(n, 1)
# running min-distance to the selected set, initialized from the seeds
min_dist <- rep(Inf, n)
for (s in selected) min_dist <- pmin(min_dist, dist_to(s))
min_dist[selected] <- -Inf
while (length(selected) < min(N_SHIP, n)) {
  nxt <- which.max(min_dist)
  selected <- c(selected, nxt)
  min_dist <- pmin(min_dist, dist_to(nxt))
  min_dist[selected] <- -Inf
}

# --- 6. a single conservative-liberal dimension (IRT ideal point) -----------
# Fit a unidimensional graded-response IRT model over the ordinal attitude items,
# on the FULL eligible pool (more stable than on 100), then score the selected
# respondents. Items are auto-oriented so the latent trait runs liberal (low) to
# conservative (high); the score is standardized ~N(0,1). This is one-time
# data-raw work and may use mirt; it never enters the shipped package.
att_keys_all <- vapply(anes_attitudes, `[[`, "", "q")
att_keys_all <- intersect(att_keys_all, names(pool))

# integer-code each attitude item by the rank order of its observed labels,
# keeping only items with a usable ordinal structure (2-12 ordered categories).
ordinalize <- function(x) {
  lv <- col_levels[[match_name <- NA]]  # placeholder; recompute locally
  lv <- sort(unique(x[!is.na(x)]))
  if (length(lv) < 2 || length(lv) > 12) return(NULL)
  # only treat as ordinal if labels look like a scale
  ord_words <- "strong|very|extremely|always|never|much|favor|oppose|increase|decrease|more|less|better|worse|approve|disapprove|cold|cool|warm|neutral|agree|likely|important|satisfied|worried|hopeful|angry|threat"
  if (!(any(grepl("[0-9]", lv)) || any(grepl(ord_words, lv, ignore.case = TRUE)))) return(NULL)
  match(x, lv)
}
item_mat <- lapply(att_keys_all, function(k) ordinalize(pool[[k]]))
names(item_mat) <- att_keys_all
keep_items <- names(item_mat)[!vapply(item_mat, is.null, logical(1))]
M <- do.call(cbind, item_mat[keep_items])
colnames(M) <- keep_items

# Orient every item to a conservative anchor, so a high coded value means the
# more-conservative response, before fitting. The anchor must itself be correctly
# oriented (high = conservative); `ordinalize`'s alphabetical coding is NOT
# reliable for that, so score the two anchor items by an explicit conservative
# direction matched on their value labels.
score_directed <- function(x, conservative_first) {
  # conservative_first: regex matched against labels, most-conservative first;
  # returns a numeric where higher = more conservative.
  x <- as.character(x); out <- rep(NA_real_, length(x))
  k <- length(conservative_first)
  for (i in seq_along(conservative_first)) {
    hit <- grepl(conservative_first[i], x, ignore.case = TRUE) & is.na(out)
    out[hit] <- (k - i + 1)   # first pattern (most conservative) -> highest
  }
  out
}
party_dir <- c("strong republican", "not very strong republican",
               "independent.?republican", "independent$|^independent",
               "independent.?democrat", "not very strong democrat",
               "strong democrat")
ideo_dir <- c("extremely conservative", "conservative", "slightly conservative",
              "moderate", "slightly liberal", "^liberal|[^y] liberal",
              "extremely liberal")
a1 <- if ("Party identification" %in% names(pool))
  scale(score_directed(pool[["Party identification"]], party_dir))[, 1] else NA
a2 <- if ("Liberal-conservative self-placement" %in% names(pool))
  scale(score_directed(pool[["Liberal-conservative self-placement"]], ideo_dir))[, 1] else NA
anchor <- rowMeans(cbind(a1, a2), na.rm = TRUE)
for (j in colnames(M)) {
  r <- suppressWarnings(stats::cor(M[, j], anchor, use = "pairwise.complete.obs"))
  if (!is.na(r) && r < 0) M[, j] <- (max(M[, j], na.rm = TRUE) + 1) - M[, j]
}

ideo_pool <- rep(NA_real_, nrow(pool))
fitted_ok <- FALSE
if (requireNamespace("mirt", quietly = TRUE)) {
  fit <- try(mirt::mirt(as.data.frame(M), 1, itemtype = "graded",
                        verbose = FALSE, TOL = 1e-3), silent = TRUE)
  if (!inherits(fit, "try-error")) {
    th <- try(mirt::fscores(fit, full.scores = TRUE, method = "EAP"), silent = TRUE)
    if (!inherits(th, "try-error")) { ideo_pool <- as.numeric(th[, 1]); fitted_ok <- TRUE }
  }
}
if (!fitted_ok) {
  # fallback: polarity-aligned standardized mean of the items
  z <- scale(M); ideo_pool <- rowMeans(z, na.rm = TRUE)
  message("mirt unavailable or failed; used polarity-aligned mean for ideology_score.")
}
# orient so high = conservative (anchor must be positively correlated), standardize
if (suppressWarnings(stats::cor(ideo_pool, anchor, use = "pairwise.complete.obs")) < 0)
  ideo_pool <- -ideo_pool
ideo_pool <- as.numeric(scale(ideo_pool))

# sanity: agreement with PCA's first component
pc1 <- tryCatch({
  zc <- scale(M); zc[is.na(zc)] <- 0
  s <- stats::prcomp(zc, center = FALSE)$x[, 1]
  if (stats::cor(s, ideo_pool) < 0) s <- -s
  stats::cor(s, ideo_pool)
}, error = function(e) NA_real_)
message(sprintf("ideology_score: %s on %d items; cor with PCA PC1 = %.3f",
                if (fitted_ok) "IRT graded-response" else "aligned mean",
                length(keep_items), pc1))

# --- 7. assemble the shipped frame: select, score, order, prefix-rename ------
sel <- pool[selected, , drop = FALSE]
sel$ideology_score <- round(ideo_pool[selected], 3)
sel <- sel[order(sel$ideology_score), , drop = FALSE]   # liberal -> conservative
rownames(sel) <- NULL

# Build tidy column handles: demo_<slug> and att_<domain>_<slug>. The human
# question wording moves to the dictionary so prose stays readable.
slug <- function(s) {
  s <- tolower(s); s <- gsub("[^a-z0-9]+", "_", s)
  s <- gsub("^_|_$", "", s); substr(s, 1, 28)
}
dom_short <- c(identity = "id", affect = "aff", evaluation = "eval",
               issue = "iss", values = "val")
demo_keys <- vapply(anes_demographics, `[[`, "", "key")

handle_of <- character(0); dict_rows <- list()
for (nm in names(sel)) {
  if (nm == "ideology_score") { handle_of[nm] <- "ideology_score"; next }
  if (nm %in% demo_keys) {
    h <- paste0("demo_", slug(nm)); dom <- "demographic"
  } else {
    code <- names(field_key)[match(nm, unlist(field_key))]
    dom_full <- field_group[[code]] %||% "issue"
    h <- paste0("att_", dom_short[[dom_full]] %||% "iss", "_", slug(nm))
    dom <- dom_full
  }
  # de-duplicate handles if a slug collides
  base_h <- h; k <- 1L
  while (h %in% handle_of) { k <- k + 1L; h <- paste0(base_h, "_", k) }
  handle_of[nm] <- h
  dict_rows[[nm]] <- data.frame(
    handle = h, question = nm,
    anes_variable = names(field_key)[match(nm, unlist(field_key))] %||% NA_character_,
    domain = dom, stringsAsFactors = FALSE)
}
dict <- do.call(rbind, dict_rows[names(sel)[names(sel) != "ideology_score"]])
rownames(dict) <- NULL

anes_2024_personas <- sel
names(anes_2024_personas) <- unname(handle_of[names(sel)])

attr(anes_2024_personas, "dictionary") <- dict
attr(anes_2024_personas, "demographic_fields") <- dict$handle[dict$domain == "demographic"]
attr(anes_2024_personas, "source") <- paste(
  "Derived from the American National Election Studies (ANES) 2024 Time Series",
  "Study, public release. Diversity sample for example coverage; NOT representative.",
  "Rows ordered by ideology_score (low = liberal, high = conservative).")
attr(anes_2024_personas, "n_demographics") <- length(anes_demographics)
attr(anes_2024_personas, "n_attitudes") <- length(anes_attitudes)

message(sprintf("Shipping %d respondents x %d fields (incl. ideology_score).",
                nrow(anes_2024_personas), ncol(anes_2024_personas)))

if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(anes_2024_personas, overwrite = TRUE, compress = "xz")
} else {
  if (!dir.exists("data")) dir.create("data")
  save(anes_2024_personas, file = "data/anes_2024_personas.rda", compress = "xz")
}
message("Wrote data/anes_2024_personas.rda")
