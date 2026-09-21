import numpy as np
import pandas as pd


def derive(cases):
    derived = pd.DataFrame(index=cases.index)
    derived["ingroup_self_positioning_scaled"] = (
        cases["battery3a_ev"] - 1
    ) / 2
    derived["emotional_intensity_scaled"] = (
        cases["battery3b_ev"] - 1
    ) / 2
    derived["outgroup_construction_scaled"] = (
        cases["battery3c_ev"] - 1
    ) / 2
    derived["coalition_direction_centered"] = (
        2 * cases["battery2_turn2_ev"] - 1
    )
    component_columns = [
        "ingroup_self_positioning_scaled",
        "emotional_intensity_scaled",
        "outgroup_construction_scaled",
    ]
    derived["conditional_psi"] = derived[component_columns].mean(
        axis=1,
        skipna=False,
    )
    gate_pass = cases["gate_pass"].astype("boolean")
    battery1_present = cases["battery1_turn2_ev"].notna()
    complete_components = derived[component_columns].notna().all(axis=1)
    observed = pd.Series(np.nan, index=cases.index, dtype=float)
    observed.loc[battery1_present & gate_pass.eq(False)] = 0.0
    passing_complete = (
        battery1_present
        & gate_pass.eq(True)
        & complete_components
    )
    observed.loc[passing_complete] = derived.loc[
        passing_complete,
        "conditional_psi",
    ]
    derived["observed_unconditional_psi"] = observed
    return derived[
        [
            "ingroup_self_positioning_scaled",
            "emotional_intensity_scaled",
            "outgroup_construction_scaled",
            "coalition_direction_centered",
            "conditional_psi",
            "observed_unconditional_psi",
        ]
    ]


def verify(cases):
    derived = derive(cases)
    differences = {}
    for column in derived.columns:
        comparable = derived[column].notna() & cases[column].notna()
        absolute = (
            derived.loc[comparable, column]
            - cases.loc[comparable, column]
        ).abs()
        maximum = float(absolute.max()) if len(absolute) else 0.0
        if not np.isfinite(maximum) or maximum > 1e-12:
            raise ValueError(f"{column} differs by {maximum}")
        differences[column] = maximum
    return differences
