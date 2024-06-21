# Climate Change Engagement of Scientists

[![DOI](https://zenodo.org/badge/817736345.svg)](https://zenodo.org/doi/10.5281/zenodo.12187344)

This repository contains code and data to reproduce all (quantitative) analyses and figures for the paper Dablander, F.<sup>&#11089;</sup>, Sachisthal, M.<sup>&#11089;</sup>, Cologna, V., Strahm, N., Bosshard, A., Grüning, N., Green, A., Brick, C., Aron, A., & Haslbeck, J.M.B.<sup>&#11089;</sup> (accepted). Climate Change Engagement of Scientists. *Nature Climate Change*. [[Link](https://osf.io/preprints/psyarxiv/73w4s)]. If you use these data or code anywhere, please cite the paper.

- `helpers.R` includes useful functions.
- `analyses/`
    - `main_analyses.Rmd` runs the analyses reported in the main text of the paper (except creating Figure 1).
    - `representativity_analysis.Rmd` runs the sample representativity analysis.
    - `world_map.R` creates a world map showing observations across countries.
    - `figure1.R` creates Figure 1.
- `data/`
    - `DataS1_Anonymized.RDS` is the raw anonymized data.
    - `DataS2_Imputed.RDS` is the imputed data.
    - `DataS3_Final.RDS` is the final data set used for quantitative analysis.
    - `DataS4_Qualitative.RDS` is the data set used for qualitative analysis.
    - `Codebook_DataS4_Qualitative.xlx` is the codebook for the qualitative data.
    - `Codebook_DataS1_Quantitative.xlx` is the codebook for the quantitative data (`DataS1_Anonymized`).
    - `map_data.csv` is the data used to render the world map.
- `models/` is where the estimated models are being saved to.
- `results/` is where the effect size measures are being saved to.
- `figures/` is where the figures are being saved to.
    - `AME/` includes figures for the average marginal effects. We report marginal effects at the mean in the paper. See `main_analyses.Rmd` for details.

---

The data and code provided in this repository are licensed under Creative Commons Attribution 4.0 International. To view a copy of this license, visit https://creativecommons.org/licenses/by/4.0/