# Inconsistencia Programática Estructural: Un Marco General
## Replication Materials

**Author:**  
Juan Ambrose Birtwistle De Figueredo

---

## Abstract

Institutionalized fiscal rules do not constrain fiscal populism: they intensify it. We formally demonstrate — through a two-period Bayesian signaling model with oligopolistic competition and financial market integration — that Structural Programmatic Inconsistency (SPI) is a necessary equilibrium when a candidate combines spending expansion promises, revenue reduction, and non-verifiable savings claims in economies with active fiscal rules. The central result (Proposition 2) cannot be derived as an extension of Alesina & Tabellini (1990), Dornbusch & Edwards (1991), or Frankel (2011), all of which predict the opposite.

**Keywords:** structural programmatic inconsistency, fiscal populism, Bayesian signaling, fiscal rule, differential verifiability, electoral political economy, Colombia 2026.

---

## Repository Structure

```
IPE-Colombia2026/
│
├── README.md                  # This file
├── paper/
│   └── IPE_MarcoGeneral_DYS84.pdf       # Published paper
│
├── code/
│   ├── monte_carlo_simulation.R         # Main Monte Carlo simulation (10,000 iterations)
│   ├── panel_regression.R               # Panel regression (Table 10, n=18)
│   └── figures.R                        # Replication of all figures
│
├── data/
│   ├── fiscal_gap_colombia2026.xlsx     # Fiscal gap computation (Table 5)
│   ├── panel_candidacies_2015_2026.csv  # Panel of 18 Latin American candidacies
│   └── sovereign_spreads_LAC.csv        # CDS spread data (Bloomberg, 2015–2025)
│
└── codebook/
    ├── content_analysis_instrument.pdf  # Populism coding instrument (HRM 2012 adapted)
    ├── coding_sheet.xlsx                # Full coding sheet (n=47 texts)
    └── intercoder_reliability.R         # Cohen's kappa calculation scripts
```

---

## Replication Instructions

### Requirements

- **R** version ≥ 4.2.0
- Required packages: `tidyverse`, `fixest`, `sandwich`, `lmtest`, `mc2d`, `betareg`

Install all dependencies with:

```r
install.packages(c("tidyverse", "fixest", "sandwich", "lmtest", "mc2d", "betareg"))
```

### Running the Monte Carlo Simulation

```r
source("code/monte_carlo_simulation.R")
```

This replicates:
- `Pr(IPE* > 0) = 0.9987` [95% CI: 0.998–0.999]
- `IPE* mean = $31.4bn` [95% CI: $19.8–$44.1bn]
- `Pr(Ω > 0.8) = 0.7234`
- `cor(θ_L, IPE*) = −0.61`

### Running the Panel Regression

```r
source("code/panel_regression.R")
```

This replicates Table 10 (columns 1–5), including the theoretical displacement test:  
`β_RF_activa > 0` across all five specifications.

---

## Key Results

| Result | Value | Location |
|--------|-------|----------|
| Pr(IPE* > 0) | 99.87% | Table 5 / Monte Carlo |
| Structural fiscal gap (base scenario) | $30.8bn COP | Table 5 |
| Ω (base scenario) | 0.96 | Table 5 |
| β_RF_activa (displacement test) | 0.312** | Table 10 |
| Inter-coder reliability (κ) | 0.78–0.81 | Tables 7–8 |

---

## Falsifiable Predictions

The framework generates six falsifiable predictions (Table 9). **PF1 is the critical test:**

> In countries with active fiscal rules, populist candidates will exhibit higher IPE/Ḡflex than equivalent candidates without active fiscal rules.

**Falsification condition:** `β_RF_activa ≤ 0` in an expanded panel (n > 50).

Researchers wishing to replicate or extend the panel analysis can find the full data collection protocol in `/codebook/content_analysis_instrument.pdf`.

---

## Data Availability

| Dataset | Source | Location |
|---------|--------|----------|
| Fiscal gap components | MinHacienda, BanRep, DIAN (2025–2026) | `/data/fiscal_gap_colombia2026.xlsx` |
| Panel of candidacies | Guriev & Papaioannou (2022), national fiscal institutes | `/data/panel_candidacies_2015_2026.csv` |
| Sovereign CDS spreads | Bloomberg (2015–2025) | `/data/sovereign_spreads_LAC.csv` |
| Discursive corpus | Public speeches, plans de gobierno (2025–2026) | Available upon request (privacy protocol) |

---

## Citation

```bibtex
@unpublished{birtwistle2026ipe,
  author = {Birtwistle De Figueredo, Juan Ambrose},
  title  = {Inconsistencia Programática Estructural: Un Marco General de Economía Política
            para el Populismo Fiscal bajo Restricciones Institucionales},
  year   = {2026},
  note   = {Working paper. Available at: github.com/jbirtwistle/IPE-Colombia2026}
}
```

---

## License

© 2026 Juan Ambrose Birtwistle De Figueredo.  
This work is licensed under [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/).

---

## Contact

📧 Juan Ambrose Birtwistle De Figueredo — Harvard Department of Economics
