```text
[![DOI](https://zenodo.org/badge/1379522811.svg)](https://doi.org/10.5281/zenodo.22872992)

paper4-manuscript-repo/
├── data/
│   ├── input/           # Comment annotations and monthly predictors
│   ├── validation/      # Human gold standard, model selection and prompt comparisons
│   └── output/
│       ├── monthly/     # Monthly measures
│       ├── diagnostics/ # Regression diagnostics
│       ├── inference/   # Hypothesis tests
│       └── validation/  # Human–model agreement and consistency across runs
├── prompts/
│   ├── production/      # Final annotation prompts
│   └── iterations/      # Prototype prompts and revisions
├── src/                 # Python and R analysis scripts
├── run.py               # Run the analysis
├── requirements.txt     # Python packages
└── r_requirements.txt   # R packages
```
