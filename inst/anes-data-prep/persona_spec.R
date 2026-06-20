# persona_spec.R ------------------------------------------------------------
# Variable specification for the bundled example dataset
# (data/anes_2024_personas.rda), sourced by make_personas.R. The auditable list
# of which ANES 2024 Time Series variables become persona fields, how they are
# grouped (for diversity sampling), and how demographics are coarsened. Shipped
# under inst/ for transparency; not run at build or install time.
#
# Source: ANES 2024 Time Series Study, public release. Items chosen from the
# public SUMMARY (`x`) recodes where available (clean ordinal scales). No
# restricted-coded variables, no case IDs, no granular geography, no open-ends.
# Orientation / gender-identity items are excluded by owner rule (see EXCLUDED).

# --- DEMOGRAPHICS (rendered as background; coarsened where noted) ------------
# Each entry: code = ANES var, key = the human field name shown in the persona.
anes_demographics <- list(
  list(code = "V241458x", key = "age",                coarsen = "age_band"),
  list(code = "V241550",  key = "sex"),
  list(code = "V241465x", key = "education"),
  list(code = "V241501x", key = "race/ethnicity"),
  list(code = "V241461x", key = "marital status"),
  list(code = "V241567x", key = "household income"),
  list(code = "V241445x", key = "religion"),
  list(code = "V241420",  key = "religion importance"),
  list(code = "V241440",  key = "religious attendance"),
  list(code = "V243007",  key = "census region"),
  list(code = "V242340",  key = "community type"),
  list(code = "V241488x", key = "employment status"),
  list(code = "V241531",  key = "home ownership"),
  list(code = "V241521",  key = "children in household", coarsen = "any_children"),
  list(code = "V241497",  key = "union household"),
  list(code = "V241470",  key = "military service"),
  list(code = "V241004",  key = "attention to politics")
)

# --- ATTITUDES (rendered as a Question/Answer block; bundle kept intact) -----
# domain is used only to BLOCK the diversity-sampling distance, not shown.
anes_attitudes <- list(
  # Party / ideology / vote
  list(code = "V241227x", domain = "identity",  q = "Party identification"),
  list(code = "V241228",  domain = "identity",  q = "Importance of party identity"),
  list(code = "V241177",  domain = "identity",  q = "Liberal-conservative self-placement"),
  list(code = "V242096x", domain = "identity",  q = "2024 presidential vote"),
  list(code = "V241043",  domain = "identity",  q = "Presidential vote intent"),
  list(code = "V241049",  domain = "identity",  q = "Harris vs Trump preference"),
  list(code = "V241100",  domain = "identity",  q = "Likelihood of voting"),
  list(code = "V241075x", domain = "identity",  q = "Party of presidential vote/intent"),
  list(code = "V241076x", domain = "identity",  q = "Party of US House vote/intent"),
  # Thermometers (candidates, parties, institutions, groups)
  list(code = "V241156", domain = "affect", q = "Feeling toward Kamala Harris"),
  list(code = "V241157", domain = "affect", q = "Feeling toward Donald Trump"),
  list(code = "V241166", domain = "affect", q = "Feeling toward the Democratic Party"),
  list(code = "V241167", domain = "affect", q = "Feeling toward the Republican Party"),
  list(code = "V241168", domain = "affect", q = "Feeling toward the MAGA movement"),
  list(code = "V242143", domain = "affect", q = "Feeling toward the Supreme Court"),
  list(code = "V242145", domain = "affect", q = "Feeling toward Congress"),
  list(code = "V242139", domain = "affect", q = "Feeling toward liberals"),
  list(code = "V242142", domain = "affect", q = "Feeling toward conservatives"),
  list(code = "V242140", domain = "affect", q = "Feeling toward labor unions"),
  list(code = "V242141", domain = "affect", q = "Feeling toward big business"),
  list(code = "V242150", domain = "affect", q = "Feeling toward the police"),
  list(code = "V242152", domain = "affect", q = "Feeling toward Black Lives Matter"),
  list(code = "V242153", domain = "affect", q = "Feeling toward the NRA"),
  list(code = "V242146", domain = "affect", q = "Feeling toward Muslims"),
  list(code = "V242147", domain = "affect", q = "Feeling toward Christians"),
  list(code = "V242149", domain = "affect", q = "Feeling toward Jews"),
  list(code = "V242156", domain = "affect", q = "Feeling toward Planned Parenthood"),
  # Approval
  list(code = "V241137x", domain = "evaluation", q = "Approval of the President"),
  list(code = "V241129x", domain = "evaluation", q = "Approval of Congress"),
  list(code = "V241133x", domain = "evaluation", q = "Approval of the Supreme Court"),
  list(code = "V241143x", domain = "evaluation", q = "Approval of the President on the economy"),
  list(code = "V241152x", domain = "evaluation", q = "Approval of the President on immigration"),
  list(code = "V241149x", domain = "evaluation", q = "Approval of the President on abortion"),
  # Economy / personal finance
  list(code = "V241294x", domain = "evaluation", q = "National economy over the past year"),
  list(code = "V241297x", domain = "evaluation", q = "National economy over the next year"),
  list(code = "V241451",  domain = "evaluation", q = "Personal finances vs a year ago"),
  list(code = "V241539",  domain = "evaluation", q = "Worry about personal finances"),
  # Taxes / spending / redistribution
  list(code = "V241239",  domain = "issue", q = "Government spending and services"),
  list(code = "V241263x", domain = "issue", q = "Federal spending on Social Security"),
  list(code = "V241281x", domain = "issue", q = "Federal spending on aid to the poor"),
  list(code = "V241275x", domain = "issue", q = "Federal spending on welfare"),
  list(code = "V242316a", domain = "issue", q = "Tax on millionaires"),
  list(code = "V242253x", domain = "issue", q = "Government reducing income inequality"),
  # Immigration
  list(code = "V241386",  domain = "issue", q = "Policy toward unauthorized immigrants"),
  list(code = "V241395x", domain = "issue", q = "Border wall"),
  list(code = "V241389x", domain = "issue", q = "Ending birthright citizenship"),
  list(code = "V242234x", domain = "issue", q = "Path to citizenship"),
  list(code = "V242235",  domain = "issue", q = "Immigrants and the economy"),
  # Guns
  list(code = "V242328x", domain = "issue", q = "Gun background checks"),
  list(code = "V242325",  domain = "issue", q = "Difficulty of buying a gun"),
  # Climate / environment
  list(code = "V242324x", domain = "issue", q = "Regulating greenhouse emissions"),
  list(code = "V242321",  domain = "issue", q = "Climate change and severe weather"),
  list(code = "V241258",  domain = "issue", q = "Environment vs business tradeoff"),
  # Healthcare
  list(code = "V242353x", domain = "issue", q = "Government help paying for health care"),
  list(code = "V241245",  domain = "issue", q = "Government vs private medical insurance"),
  # Abortion
  list(code = "V241248",  domain = "issue", q = "Abortion position"),
  list(code = "V241303",  domain = "issue", q = "Importance of the abortion issue"),
  # Crime / policing
  list(code = "V241308x", domain = "issue", q = "Death penalty"),
  list(code = "V242336",  domain = "issue", q = "Police use of force"),
  list(code = "V242525x", domain = "issue", q = "Whether police treat blacks or whites better"),
  # Racial policy and attitudes
  list(code = "V242241x", domain = "issue", q = "Preferential hiring of blacks"),
  list(code = "V242245x", domain = "issue", q = "Affirmative action in universities"),
  list(code = "V241255",  domain = "issue", q = "Government assistance to blacks"),
  list(code = "V242300",  domain = "issue", q = "Blacks should work their way up without favors"),
  list(code = "V242301",  domain = "issue", q = "Slavery makes it harder for blacks"),
  list(code = "V242302",  domain = "issue", q = "Blacks have gotten less than they deserve"),
  list(code = "V242303",  domain = "issue", q = "If blacks tried harder they would be as well off"),
  list(code = "V242549",  domain = "issue", q = "Discrimination against blacks"),
  list(code = "V242552",  domain = "issue", q = "Discrimination against whites"),
  list(code = "V242554",  domain = "issue", q = "Discrimination against women"),
  # Foreign policy / defense / trade
  list(code = "V241400x", domain = "issue", q = "US weapons to Ukraine"),
  list(code = "V241403x", domain = "issue", q = "US military aid to Israel"),
  list(code = "V241409x", domain = "issue", q = "Side with Israelis or Palestinians"),
  list(code = "V242346x", domain = "issue", q = "Free trade agreements"),
  list(code = "V242365",  domain = "issue", q = "China as a threat"),
  list(code = "V242366",  domain = "issue", q = "Russia as a threat"),
  list(code = "V241242",  domain = "issue", q = "Defense spending"),
  # Trust / populism / authoritarianism
  list(code = "V241229",  domain = "values", q = "Trust in government in Washington"),
  list(code = "V241231",  domain = "values", q = "Government run for a few big interests"),
  list(code = "V242304",  domain = "values", q = "Whether the political system works for insiders"),
  list(code = "V241338x", domain = "values", q = "Compromise vs sticking to principles"),
  list(code = "V242411",  domain = "values", q = "A strong leader is good for government"),
  list(code = "V242414",  domain = "values", q = "Leaving decisions to independent experts"),
  list(code = "V242260",  domain = "values", q = "Child trait: independence vs respect"),
  list(code = "V242262",  domain = "values", q = "Child trait: obedience vs self-reliance"),
  # Social trust / wellbeing / institutional confidence
  list(code = "V241234",  domain = "values", q = "Whether people can be trusted"),
  list(code = "V241621",  domain = "values", q = "Satisfaction with life"),
  list(code = "V242614",  domain = "values", q = "General happiness"),
  list(code = "V242617",  domain = "values", q = "Confidence in the Supreme Court"),
  list(code = "V242620",  domain = "values", q = "Confidence in the scientific community"),
  list(code = "V242619",  domain = "values", q = "Confidence in the press"),
  # Moral traditionalism / gender roles (roles, NOT identity)
  list(code = "V242258",  domain = "values", q = "Adjusting morals to a changing world"),
  list(code = "V242259",  domain = "values", q = "Traditional family values"),
  list(code = "V242279x", domain = "values", q = "Whether men should work and women keep home"),
  # Democracy / mobility / mood / competence / education
  list(code = "V241711",  domain = "values", q = "Satisfaction with democracy"),
  list(code = "V241731",  domain = "values", q = "Importance of remaining a democracy"),
  list(code = "V242314x", domain = "issue",  q = "Economic mobility vs 20 years ago"),
  list(code = "V241360",  domain = "issue",  q = "Income gap vs 20 years ago"),
  list(code = "V242269x", domain = "issue",  q = "Whether rural areas get their fair share"),
  list(code = "V241118",  domain = "evaluation", q = "Hopefulness about the country"),
  list(code = "V241121",  domain = "evaluation", q = "Anger about the country"),
  list(code = "V241236",  domain = "evaluation", q = "Which party better handles the economy"),
  list(code = "V241237",  domain = "evaluation", q = "Which party better handles immigration"),
  list(code = "V242179",  domain = "issue", q = "Importance of what public schools teach"),
  list(code = "V241290x", domain = "issue", q = "Approval of diversity, equity and inclusion"),
  # Sexual-harassment-as-social-issue (owner added back; NOT orientation/identity)
  list(code = "V241341",  domain = "issue", q = "Whether sexual harassment would affect the vote"),
  list(code = "V242361x", domain = "issue", q = "Whether attention to sexual harassment has gone too far")
)

# --- EXCLUDED by owner rule (orientation / gender identity) ------------------
# Recorded for provenance; never read into the dataset.
anes_excluded_identity <- c(
  "V241370","V241371","V241372x","V241373","V241374","V241375x",
  "V241376","V241377","V241378x","V241379","V241380","V241381x",
  "V241382","V241383","V241384","V241385x","V241552","V241553","V241553z",
  "V241719","V241742","V242144","V242151","V242211","V242362","V242363",
  "V242364x","V242457","V242458x","V242508","V242510","V242553","V242558",
  # the LGBT-elected importance + same-sex social-contact + orientation items
  "V242359","V242360"  # harassment strength components folded into V242361x; orientation-adjacent rows dropped
)

# --- value recodes: codes to treat as missing before rendering --------------
# Thermometer don't-know/don't-recognize, "haven't thought", DOB-missing,
# and the ANES negative administrative codes (-1 inapplicable, -4..-9, post
# breakoff -5/-6/-7). Applied in the data-raw build, not at runtime.
anes_na_value_patterns <- c(
  "refused", "don't know", "don.t know", "\\bdk\\b", "\\brf\\b", "dk/rf",
  "inapplicable", "haven't thought", "haven.t thought", "don't recognize",
  "don.t recognize", "missing", "no post", "break ?off", "partial",
  "interview deleted", "not asked", "no pre", "technical error",
  "legitimate skip", "^-[0-9]"
)
anes_na_numeric <- c(-1, -2, -3, -4, -5, -6, -7, -8, -9, 998, 999)
