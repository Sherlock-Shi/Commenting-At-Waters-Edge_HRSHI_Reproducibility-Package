from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
DATA = REPO_ROOT / "data"
SRC = REPO_ROOT / "src"
CASES = DATA / "input" / "comment_annotations.parquet"
PBI_INPUT = DATA / "input" / "party_brand_index_timeseries.csv"
STANCE_INPUT = DATA / "input" / "monthly_stance.csv"
MONTHLY_OUTCOMES = DATA / "output" / "monthly" / "monthly_outcomes.csv"
DIAGNOSTICS_OUT = DATA / "output" / "diagnostics"
INFERENCE_OUT = DATA / "output" / "inference"
VALIDATION_IN = DATA / "validation"
VALIDATION_OUT = DATA / "output" / "validation"
DIAGNOSTIC_R = SRC / "diagnostic.R"
INFERENCE_R = SRC / "inference.R"
STUDY_MONTHS = [
    "2022-03",
    "2022-04",
    "2022-05",
    "2022-06",
    "2022-07",
    "2022-08",
    "2022-09",
    "2022-10",
    "2022-11",
    "2022-12",
    "2023-01",
    "2023-02",
    "2023-03",
    "2023-04",
    "2023-05",
    "2023-06",
    "2023-07",
    "2023-08",
    "2023-09",
    "2023-10",
    "2023-11",
    "2023-12",
    "2024-01",
    "2024-02",
    "2024-03",
    "2024-04",
    "2024-05",
    "2024-06",
    "2024-07",
    "2024-08",
    "2024-09",
    "2024-10",
    "2024-11",
    "2024-12",
]
GATE_THRESHOLD = 0.50
SHIFT_FIRST_MONTH = "2023-09"
BOOTSTRAP_SEED = 20260804
BOOTSTRAP_REPLICATIONS = 10000
HAC_MAX_LAG = 1


def ensure_output_dirs():
    for directory in (
        MONTHLY_OUTCOMES.parent,
        DIAGNOSTICS_OUT,
        INFERENCE_OUT,
        VALIDATION_OUT,
    ):
        directory.mkdir(parents=True, exist_ok=True)
    return None
