# Narcissism, synergy, and friction in sports teams

> **Status:** archival, exploratory research code. The analyses were developed mainly in 2020-2021 and have not been verified as a reproducible pipeline on a current R installation. The repository is useful as a record of the project, but its outputs should not be treated as final results without a fresh audit and rerun.

## Project overview

This project investigates how narcissism at the individual and team levels is associated with functioning in interdependent sports teams. The scripts combine several student/research datasets and examine whether agentic narcissism (NPI), communal narcissism (CNI), and their interaction relate to:

- group-goal commitment;
- task, relationship, and process conflict; and
- social and task cohesion.

Most analyses account for players being nested within teams. They range from frequentist multilevel models (`lme4`) to Bayesian multilevel and multivariate models (`brms`). One notebook also explores latent profiles based on NPI and CNI scores. The models commonly separate within-team scores from team means and test individual-level, team-level, and cross-level interactions.

A presentation stored in the original working copy, `Narcissismandgroupfunctioning-2.pptx`, describes a related analysis titled *The More the Merrier: Narcissism and Team Functioning*. It reports a sample of 211 players in 20 teams and a conditional indirect-effects model linking narcissism, intragroup conflict, and cohesion. That presentation is not tracked by Git, and its sample and model should not be assumed to correspond exactly to every script in this repository.

## Measures and variable names

| Abbreviation | Meaning in this project |
| --- | --- |
| NPI | Narcissistic Personality Inventory; used as the agentic narcissism measure |
| CNI | Communal Narcissism Inventory |
| GGC | Group-goal commitment |
| IGC / ICS-S | Intragroup conflict, divided into task, relationship, and process conflict |
| GEQ | Group Environment Questionnaire cohesion dimensions: individual attractions to the group (social/task) and group integration (social/task) |

Names such as `npi_mean`, `cni_mean`, `ggc_mean`, `igc_task`, `igc_rel`, and `igc_proc` in the cleaned data refer to derived scale scores. See [`r_scripts/cleaning_ross_data.R`](r_scripts/cleaning_ross_data.R) for the item recoding and scoring rules actually used.

## Data flow

```text
authorised SPSS source files in raw_data/
                 |
                 v
      r_scripts/cleaning_ross_data.R
                 |
                 v
       cleaned RDS files in data/
                 |
                 +--> r_scripts/     frequentist multilevel models
                 |
                 +--> r_markdown/    Bayesian models, plots, and LPA
                                      |
                                      v
                              rendered HTML reports
```

The local processed snapshots dated 30 November 2020 contain:

| File | Rows | Columns | Distinct non-missing team labels |
| --- | ---: | ---: | ---: |
| `amber.rds` | 409 | 132 | 42 |
| `brom_combined.rds` | 400 | 47 | 44 |
| `brom_msc.rds` | 141 | 93 | 14 |
| `toby_matt.rds` | 306 | 242 | 24 |
| `amber_brom_msc_merged.rds` | 550 | 9 | 56 |

These are row counts in the cleaned files, not necessarily the sample sizes used by each model. Individual notebooks apply further missing-data and small-team exclusions. The merged dataset is a row-bind of the Amber and Brom MSc data.

## Repository map

| Path | Contents |
| --- | --- |
| [`r_scripts/cleaning_ross_data.R`](r_scripts/cleaning_ross_data.R) | Imports six SPSS files, cleans four analysis datasets, derives scale scores, creates the Amber/Brom MSc merge, and writes five RDS files |
| [`r_scripts/`](r_scripts) | Frequentist plots and multilevel models for the Amber, Brom combined, and Toby/Matt datasets; two files are unfinished placeholders |
| [`r_markdown/`](r_markdown) | Bayesian `brms` reports for Amber, Brom MSc, and their merge, plus a `tidyLPA` profile analysis |
| [`r_notebooks/exploring_raw_data.Rmd`](r_notebooks/exploring_raw_data.Rmd) | Early data inspection, scoring questions, and development of the cleaning steps |
| [`pils_connect/`](pils_connect) | A separate reference package of data, codebooks, and example analyses accompanying Geukes et al. (2018), *Explaining the Longitudinal Interplay of Personality and Social Relationships in the Laboratory and in the Field: The PILS and CONNECT Study* |
| `raw_data/` | Original project SPSS files; intentionally not versioned |
| `data/` | Cleaned RDS files and an item-key workbook; intentionally not versioned |

The PILS/CONNECT material is not wired into the sports-team cleaning or modelling pipeline. Its scripts retain their own working-directory assumptions and should be treated as third-party methodological reference material.

## Software and intended execution order

The project requires R. It was developed interactively in RStudio, but no historical R version, package lockfile, or complete `sessionInfo()` was saved. Packages referenced by the core files include:

- data preparation and exploration: `tidyverse`, `haven`, `janitor`, `DataExplorer`, `psych`, and `tidyselect`;
- frequentist modelling: `lme4`, `lmerTest`, `performance`, `RColorBrewer`, and `Amelia`;
- Bayesian reports: `brms`, `RcppEigen`, `ggmcmc`, `ggthemes`, `ggridges`, `knitr`, `papaja`, `broom`, `broom.mixed`, and `tidybayes`; and
- profile analysis: `tidyLPA`, `mousetrap`, and `mice`.

`brms` also requires a working Stan/C++ toolchain. The Bayesian notebooks fit many models with thousands of iterations, so a clean render can be computationally expensive.

The historical execution order was intended to be:

1. Place the authorised source `.sav` files, with the exact filenames used by the script, in `raw_data/`.
2. From the repository root, run `Rscript r_scripts/cleaning_ross_data.R` to create the files in `data/`.
3. Run a relevant frequentist script from the repository root, for example `Rscript r_scripts/2_brom_combined_multilevel.R`.
4. Render a report, for example `Rscript -e "rmarkdown::render('r_markdown/2_amber_brom_msc_merged_lpa.Rmd')"`. R Markdown uses the document directory while knitting, which is why those files read from `../data/`.

These are entry points, not currently verified reproduction commands. Review the limitations below before using them.

## Current status and known limitations

Status was assessed from the repository and local working copy on 11 August 2026.

- The raw `.sav` files, cleaned `.rds` files, spreadsheets, PDFs, rendered HTML, and knitr caches are excluded by [`.gitignore`](.gitignore). A fresh clone therefore contains the analysis source but not the data or historical results needed to rerun it.
- Historical HTML reports and caches are present in the original working copy, with modification dates from 2020-2021, but they are not versioned and have not been reviewed as canonical outputs.
- The six plain R scripts pass a syntax-only parse check; end-to-end execution and R Markdown rendering have not been validated.
- The cleaning script contains selector expressions such as `"experience" | "tenure"` that require correction before a clean run with `dplyr`. The exploratory notebook contains the same development-era code.
- [`r_scripts/2_amber_multilevel.R`](r_scripts/2_amber_multilevel.R) models a variable named `ggc`, whereas the cleaned Amber data contains `ggc_mean`.
- [`r_scripts/2_amber_brom_msc_merged_multilevel.R`](r_scripts/2_amber_brom_msc_merged_multilevel.R) and [`r_scripts/2_linear_models.R`](r_scripts/2_linear_models.R) are placeholders rather than completed analyses.
- The two merged-data files [`2_amber_brom_msc_merged_brms.Rmd`](r_markdown/2_amber_brom_msc_merged_brms.Rmd) and [`2_amber_brom_msc_merged_brms_contextual.Rmd`](r_markdown/2_amber_brom_msc_merged_brms_contextual.Rmd) are byte-for-byte identical in the current revision.
- There is no automated pipeline, test suite, continuous integration, final manuscript, or consolidated results table. Model code and cached reports should therefore be interpreted as exploratory analysis history, not as a definitive finding.
- Two raw datasets loaded by the cleaning script (`danielle_raw` and `dan_james_raw`) are explored but do not contribute to the five saved processed datasets.

The presentation cautiously reports some evidence that the association between individual narcissism and team cohesion may depend on team narcissism. That statement is historical context only; it should be re-established from an audited model and documented data provenance before being cited as a project conclusion.

## Data governance

The sports-team files contain human-participant research data. Confirm the applicable consent, ethics, ownership, and sharing conditions before copying or publishing them. Do not commit raw or participant-level processed data merely to make the repository runnable. If the project is revived, prefer a documented access procedure and, where permitted, a synthetic or disclosure-controlled example dataset.

## Recommended work before revival

1. Confirm the provenance, codebook, permissions, and intended role of every source dataset.
2. Correct the known cleaning and variable-name issues and decide which duplicated or placeholder analyses to retain.
3. Create a versioned environment (for example with `renv`) and record R, package, Stan, and system-toolchain versions.
4. Turn the selected workflow into a clean, scripted pipeline with explicit inputs and outputs.
5. Rerun all selected models, check diagnostics and exclusions, and produce a single reviewed results report with `sessionInfo()`.

## License

The repository includes the [GNU General Public License version 3](LICENSE). Third-party PILS/CONNECT data and supporting files may be subject to their original authors' terms and citation requirements.
