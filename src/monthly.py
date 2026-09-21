import numpy as np
import pandas as pd

from . import config


def build(cases, pbi, stance):
    eligible_battery1 = cases["battery1_eligible"].fillna(False).astype(bool)
    eligible_downstream = (
        cases["downstream_eligible"].fillna(False).astype(bool)
    )
    observed_rows = cases["observed_unconditional_psi"].notna()
    subsets = {
        "E": eligible_battery1,
        "D": eligible_downstream,
        "U": observed_rows,
    }
    specifications = [
        (
            "affiliation_recognizable_ev_mean",
            "Affiliation Recognizability",
            "E",
            "affiliation_recognizable_ev",
        ),
        (
            "conditional_psi_mean",
            "Conditional Partisan Strength",
            "D",
            "conditional_psi",
        ),
        (
            "observed_unconditional_psi_mean",
            "Observed Unconditional Partisan Strength",
            "U",
            "observed_unconditional_psi",
        ),
        (
            "ingroup_self_positioning_mean",
            "In-group Self-positioning",
            "D",
            "ingroup_self_positioning_scaled",
        ),
        (
            "emotional_intensity_mean",
            "Emotional Intensity",
            "D",
            "emotional_intensity_scaled",
        ),
        (
            "outgroup_construction_mean",
            "Out-group Construction",
            "D",
            "outgroup_construction_scaled",
        ),
        (
            "rep_skew_mean",
            "Republican Skew",
            "D",
            "rep_skew",
        ),
        (
            "dem_skew_mean",
            "Democratic Skew",
            "D",
            "dem_skew",
        ),
        (
            "direction_slope",
            "Conditional Partisan Strength Direction Slope",
            "D",
            "slope",
        ),
    ]
    pbi_values = (
        pbi.set_index("year_month")["party_brand_index"]
        .reindex(config.STUDY_MONTHS)
        .astype(float)
        / 10
    )
    republican_values = (
        stance.loc[
            stance["dimension"].eq("q1")
            & stance["party"].eq("republican")
        ]
        .set_index("year_month")["mean"]
        .reindex(config.STUDY_MONTHS)
        .astype(float)
        / 10
    )
    democratic_values = (
        stance.loc[
            stance["dimension"].eq("q1")
            & stance["party"].eq("democrat")
        ]
        .set_index("year_month")["mean"]
        .reindex(config.STUDY_MONTHS)
        .astype(float)
        / 10
    )
    rows = []
    for outcome, outcome_label, subset_name, value_name in specifications:
        if outcome == "rep_skew_mean":
            predictor = "q1_stance_republican_10"
            predictor_label = "Republican Q1 stance (10-point units)"
            predictor_values = republican_values
        elif outcome == "dem_skew_mean":
            predictor = "q1_stance_democratic_10"
            predictor_label = "Democratic Q1 stance (10-point units)"
            predictor_values = democratic_values
        else:
            predictor = "party_brand_index_10"
            predictor_label = "Party Brand Index (10-point units)"
            predictor_values = pbi_values
        for sample_month in config.STUDY_MONTHS:
            selected = (
                subsets[subset_name]
                & cases["sample_month"].eq(sample_month)
            )
            month_cases = cases.loc[selected]
            if value_name == "rep_skew":
                case_value = np.maximum(
                    month_cases["coalition_direction_centered"].to_numpy(
                        dtype=float
                    ),
                    0,
                ).mean()
            elif value_name == "dem_skew":
                case_value = np.maximum(
                    -month_cases["coalition_direction_centered"].to_numpy(
                        dtype=float
                    ),
                    0,
                ).mean()
            elif value_name == "slope":
                x = month_cases[
                    "coalition_direction_centered"
                ].to_numpy(dtype=float)
                y = month_cases["conditional_psi"].to_numpy(dtype=float)
                if len(month_cases) < 2:
                    case_value = np.nan
                else:
                    centered_x = x - x.mean()
                    denominator = np.sum(centered_x * centered_x)
                    case_value = (
                        np.sum(centered_x * (y - y.mean())) / denominator
                        if denominator != 0
                        else np.nan
                    )
            else:
                case_value = month_cases[value_name].mean()
            rows.append(
                {
                    "outcome": outcome,
                    "outcome_label": outcome_label,
                    "predictor": predictor,
                    "predictor_label": predictor_label,
                    "sample_month": sample_month,
                    "predictor_value": predictor_values.loc[sample_month],
                    "case_value": case_value,
                    "n_cases": int(len(month_cases)),
                }
            )
    result = pd.DataFrame(
        rows,
        columns=[
            "outcome",
            "outcome_label",
            "predictor",
            "predictor_label",
            "sample_month",
            "predictor_value",
            "case_value",
            "n_cases",
        ],
    )
    result["n_cases"] = result["n_cases"].astype(int)
    return result
