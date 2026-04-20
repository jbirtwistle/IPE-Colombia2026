# =============================================================================
# CONTENT ANALYSIS CODEBOOK
# Instrumento de Análisis de Contenido Discursivo — Populismo Fiscal
# Juan Ambrose Birtwistle De Figueredo (2026)
#
# Based on: Hawkins, Riding & Mudde (2012) adapted for fiscal populism context
# Inter-coder reliability: κ = 0.78 (global), p < 0.001
# PABAK κ = 0.76
# Corpus: n = 47 texts, stratified sample Aug 2025 – Apr 2026
# =============================================================================

# INSTRUMENT OVERVIEW
# -------------------
# Five indicators (I1-I5) measuring populist fiscal discourse.
# Each text is coded 0 (absent) or 1 (present) per indicator.
# Coding unit: paragraph-level for I1-I3; document-level for I4-I5.
# Minimum text length for inclusion: 200 words.

# INDICATOR DEFINITIONS
# ---------------------

# I1 — "El pueblo" como actor homogéneo y virtuoso
# Definition: Text frames "the people" as a unified, morally pure collective
#             distinct from corrupt elites. Includes references to:
#             - "el pueblo colombiano", "la gente", "los de abajo"
#             - Homogeneous attribution of virtues to popular masses
#             - Contrast between pure people vs. corrupt elite
# Coding rule: Code 1 if ANY paragraph contains this framing
# Connection to model: Amplifies (1-α̅) effective; increases ρ
# κ = 0.79

# I2 — Élite corrupta como causante de males del pueblo
# Definition: Text identifies a corrupt elite as the direct cause of
#             economic/social problems, AND implies that removing this elite
#             would free up fiscal resources. Includes:
#             - Named or unnamed "corrupt politicians/businesspeople"
#             - Causal link: corruption → fiscal waste → available savings
#             - "Si acabamos con la corrupción, alcanza el dinero"
# Coding rule: Code 1 if BOTH elements present (elite identified + causal link)
# Connection to model: Defines σ source (corrupt = latent σ)
# κ = 0.82

# I3 — Candidato como encarnación de la voluntad popular
# Definition: Text presents the candidate as the sole authentic representative
#             of the people, whose personal qualities guarantee program success.
#             Includes:
#             - First-person singular as subject of collective transformation
#             - Personal moral qualities as substitute for institutional mechanisms
#             - "Yo soy la única opción", messianic framing
# Coding rule: Code 1 if candidate-as-embodiment framing is explicit
# Connection to model: Reduces effective α of electorate
# κ = 0.74

# I4 — Descalificación de instituciones y mediadores
# Definition: Text explicitly delegitimizes fiscal/democratic institutions,
#             technocratic bodies, or media as biased against the people. Includes:
#             - Attacks on CARF, Contraloría, Banco de la República
#             - "Los medios mienten", "los técnicos del establecimiento"
#             - Framing institutional constraints as obstacles, not safeguards
# Coding rule: Code 1 if ANY institution is delegitimized on behalf of "the people"
# Connection to model: Erodes τ_F; reduces CARF credibility
# κ = 0.77

# I5 — Transformación inmediata y total sin descripción de mecanismos
# Definition: Text promises rapid, comprehensive transformation WITHOUT
#             specifying the implementation mechanism. Includes:
#             - "El primer día", "en 90 días", "de inmediato"
#             - Transformation framed as act of will, not policy process
#             - Absence of budget allocation, legislative process, or timeline
# Coding rule: Code 1 if promise of rapid change AND absence of mechanism
# Connection to model: Discursive correlate of high σ̃ claimed without verification
# κ = 0.76

# RELIABILITY PROTOCOL
# --------------------
# 1. Two independent coders trained on 10 practice texts (not in corpus)
# 2. Disagreements resolved by discussion in training phase
# 3. Main coding: each coder codes all 47 texts independently
# 4. Inter-coder reliability calculated with Cohen's kappa per indicator
# 5. External arbitrator reviews all disagreements (κ < 0.70 trigger)
# 6. Final code = majority rule (2/2 agreement) or arbitrator decision

# SAMPLING PROTOCOL
# -----------------
# Corpus: n = 47 texts, stratified by:
#   - Channel: plan de gobierno (n=1), speeches (n=18), interviews (n=14),
#              social media (n=9), debate statements (n=5)
#   - Period: Aug-Oct 2025 (n=12), Nov 2025-Jan 2026 (n=18), Feb-Apr 2026 (n=17)
# Inclusion criteria: public text with fiscal content, min 200 words
# Exclusion: texts without fiscal/programmatic content

# CO-OCCURRENCE ANALYSIS
# ----------------------
# I2 × I5 co-occurrence rate: 68% of texts (32/47)
# Interpretation: "corruption savings" (I2) + "immediate transformation" (I5)
# is the discursive signature of Type III IPE (mixed type, Colombia 2026)
# Prediction: Co-occurrence I2×I5 > 60% distinguishes Type III from Types I and II

# R CODE FOR KAPPA CALCULATION
# ----------------------------

calculate_reliability <- function(coder1, coder2) {
  # coder1, coder2: vectors of 0/1 codes for same texts
  n <- length(coder1)
  agree <- sum(coder1 == coder2) / n
  
  p1 <- mean(coder1); p2 <- mean(coder2)
  pe <- p1 * p2 + (1 - p1) * (1 - p2)
  
  kappa <- (agree - pe) / (1 - pe)
  
  # PABAK (prevalence and bias adjusted kappa)
  pabak <- 2 * agree - 1
  
  return(list(
    kappa = round(kappa, 3),
    pabak = round(pabak, 3),
    agreement = round(agree, 3),
    n = n
  ))
}

# Example usage:
# coder1_I1 <- c(1,0,1,1,0,...)  # Vector of codes from coder 1
# coder2_I1 <- c(1,1,1,0,0,...)  # Vector of codes from coder 2
# reliability_I1 <- calculate_reliability(coder1_I1, coder2_I1)

# RESULTS SUMMARY (Table 8)
# -------------------------
results_summary <- data.frame(
  Indicator = c("I1", "I2", "I3", "I4", "I5", "AVERAGE"),
  Definition = c(
    "El pueblo como actor homogéneo y virtuoso",
    "Élite corrupta como causante de males del pueblo",
    "Candidato como encarnación de la voluntad popular",
    "Descalificación de instituciones y mediadores",
    "Transformación inmediata y total sin mecanismos",
    ""
  ),
  N_texts = c(36, 38, 29, 31, 34, 33.6),
  Rate_pct = c(76.6, 80.9, 61.7, 66.0, 72.3, 71.5),
  Kappa = c(0.79, 0.82, 0.74, 0.77, 0.76, 0.78),
  Model_connection = c(
    "Amplifies (1-α̅); increases ρ",
    "Defines σ source (corrupt = latent σ)",
    "Reduces effective α",
    "Erodes τ_F; reduces CARF credibility",
    "Discursive correlate of high σ̃ claimed",
    "Type III IPE mixta confirmed"
  )
)

print(results_summary)
