from pathlib import Path

import numpy as np
import pandas as pd


REPO_ROOT = Path(__file__).resolve().parents[1]
INPUT_DIR = REPO_ROOT / "data" / "validation"
OUTPUT_DIR = REPO_ROOT / "data" / "output" / "validation"


def _confusion_matrix(gold, prediction, labels):
    matrix = np.zeros((len(labels), len(labels)), dtype=float)
    positions = {label: index for index, label in enumerate(labels)}
    for gold_value, prediction_value in zip(gold, prediction):
        matrix[
            positions[int(gold_value)],
            positions[int(prediction_value)],
        ] += 1
    return matrix


def _cohen_kappa(matrix):
    count = matrix.sum()
    observed = np.trace(matrix) / count
    expected = (
        matrix.sum(axis=1) * matrix.sum(axis=0)
    ).sum() / count**2
    return (observed - expected) / (1 - expected)


def _weighted_kappa(matrix):
    count = matrix.sum()
    expected = np.outer(matrix.sum(axis=1), matrix.sum(axis=0)) / count
    weights = np.fromfunction(
        lambda row, column: ((row - column) / 2) ** 2,
        matrix.shape,
    )
    return 1 - (weights * matrix).sum() / (weights * expected).sum()


def _spearman(gold, prediction):
    gold_ranks = pd.Series(gold).rank(method="average").to_numpy(dtype=float)
    prediction_ranks = (
        pd.Series(prediction).rank(method="average").to_numpy(dtype=float)
    )
    return float(np.corrcoef(gold_ranks, prediction_ranks)[0, 1])


def _prepare_tasks():
    battery1 = pd.read_csv(INPUT_DIR / "battery1_runs.csv")
    battery2 = pd.read_csv(INPUT_DIR / "battery2_runs.csv")
    battery3 = pd.read_csv(INPUT_DIR / "battery3_runs.csv")

    selected_battery1 = battery1.loc[
        battery1["prompt_version"].str.contains(
            "v1_battery1_selected_validation_qwen36plus_rep",
            na=False,
        )
    ].copy()
    selected_battery1["run_id"] = selected_battery1["prompt_version"]
    selected_battery1["gold"] = selected_battery1[
        "gold_battery1_label_0_or_1"
    ]
    selected_battery1["prediction"] = (
        selected_battery1["battery1_turn2_ev"] >= 0.50
    ).astype(int)

    selected_battery2 = battery2.loc[
        battery2["prompt_version"].str.contains(
            "v1_battery2_selected_validation_qwen36plus_rep",
            na=False,
        )
    ].copy()
    selected_battery2["run_id"] = selected_battery2["prompt_version"]
    selected_battery2["gold"] = selected_battery2[
        "gold_battery2_direction_0_dem_or_1_rep"
    ]
    selected_battery2["prediction"] = (
        selected_battery2["battery2_turn2_ev"] >= 0.50
    ).astype(int)

    component_settings = [
        (
            "Battery 3A",
            "battery3a",
            "qwen36plus",
            [
                f"v1_battery3a_selected_validation_qwen36plus_rep{rep}"
                for rep in range(1, 6)
            ],
        ),
        (
            "Battery 3B",
            "battery3b",
            "deepseek_pro",
            [
                f"v1_battery3b_selected_validation_deepseek_pro_rep{rep}"
                for rep in range(1, 6)
            ],
        ),
        (
            "Battery 3C",
            "battery3c",
            "qwen36plus",
            [
                f"v1_battery3c_selected_validation_qwen36plus_rep{rep}"
                for rep in range(1, 6)
            ],
        ),
    ]

    tasks = [
        {
            "task": "Battery 1",
            "model": "qwen36plus",
            "frame": selected_battery1,
            "labels": [0, 1],
        },
        {
            "task": "Battery 2",
            "model": "qwen36plus",
            "frame": selected_battery2,
            "labels": [0, 1],
        },
    ]
    for task, component, model, run_labels in component_settings:
        selected = battery3.loc[
            battery3["component"].eq(component)
            & battery3["model_key"].eq(model)
            & battery3["run_label"].isin(run_labels)
        ].copy()
        selected["run_id"] = selected["run_label"]
        selected["gold"] = selected["gold_label"]
        selected["prediction"] = selected["pred_label"]
        tasks.append(
            {
                "task": task,
                "model": model,
                "frame": selected,
                "labels": [1, 2, 3],
            }
        )
    return tasks


def _label_distribution():
    human = pd.read_csv(INPUT_DIR / "human_labels.csv")
    settings = [
        ("Affiliation Recognizability", "battery1", [0, 1]),
        (
            "Coalition Direction",
            "battery2_direction_0_dem_or_1_rep",
            [0, 1],
        ),
        ("In-group Self-positioning", "battery3a", [1, 2, 3]),
        ("Emotional Intensity", "battery3b", [1, 2, 3]),
        ("Out-group Construction", "battery3c", [1, 2, 3]),
    ]
    rows = []
    for task, column, labels in settings:
        counts = human[column].value_counts(dropna=True)
        row = {
            "task": task,
            "gold_0_count": np.nan,
            "gold_1_count": np.nan,
            "gold_2_count": np.nan,
            "gold_3_count": np.nan,
        }
        for label in labels:
            row[f"gold_{label}_count"] = int(counts.get(label, 0))
        rows.append(row)
    return pd.DataFrame(rows)


def run():
    tasks = _prepare_tasks()
    summary_rows = []
    two_class_rows = []
    ordered_rows = []
    run_rows = []

    for setting in tasks:
        frame = setting["frame"].loc[
            setting["frame"]["gold"].notna()
        ].copy()
        run_ids = sorted(frame["run_id"].unique())
        if len(run_ids) != 5:
            raise ValueError(f"{setting['task']} must contain five runs")

        agreements = []
        kappas = []
        correlations = []
        matrices = []
        two_category_errors = 0
        prediction_columns = {}

        for run_id in run_ids:
            selected = frame.loc[frame["run_id"].eq(run_id)].sort_values(
                "sample_index"
            )
            gold = selected["gold"].astype(int).to_numpy()
            prediction = selected["prediction"].astype(int).to_numpy()
            matrix = _confusion_matrix(
                gold,
                prediction,
                setting["labels"],
            )
            agreements.append(float(np.mean(gold == prediction)))
            matrices.append(matrix)
            prediction_columns[run_id] = pd.Series(
                prediction,
                index=selected["sample_index"].astype(int),
            )

            if len(setting["labels"]) == 2:
                kappas.append(_cohen_kappa(matrix))
            else:
                kappas.append(_weighted_kappa(matrix))
                correlations.append(_spearman(gold, prediction))
                two_category_errors += int(
                    np.sum(np.abs(gold - prediction) == 2)
                )

        mean_matrix = np.mean(matrices, axis=0)
        summary_rows.append(
            {
                "task": setting["task"],
                "model": setting["model"],
                "agreement": float(np.mean(agreements)),
                "cohen_kappa": (
                    float(np.mean(kappas))
                    if len(setting["labels"]) == 2
                    else np.nan
                ),
                "quadratic_weighted_kappa": (
                    float(np.mean(kappas))
                    if len(setting["labels"]) == 3
                    else np.nan
                ),
                "spearman_correlation": (
                    float(np.mean(correlations))
                    if correlations
                    else np.nan
                ),
                "two_category_errors": (
                    two_category_errors
                    if len(setting["labels"]) == 3
                    else np.nan
                ),
            }
        )

        if len(setting["labels"]) == 2:
            two_class_rows.append(
                {
                    "task": setting["task"],
                    "model": setting["model"],
                    "true_positive": mean_matrix[1, 1],
                    "true_negative": mean_matrix[0, 0],
                    "false_positive": mean_matrix[0, 1],
                    "false_negative": mean_matrix[1, 0],
                }
            )
        else:
            ordered_row = {
                "task": setting["task"],
                "model": setting["model"],
            }
            for gold_index, gold_label in enumerate(setting["labels"]):
                for prediction_index, prediction_label in enumerate(
                    setting["labels"]
                ):
                    ordered_row[
                        f"gold_{gold_label}_pred_{prediction_label}"
                    ] = mean_matrix[gold_index, prediction_index]
            ordered_rows.append(ordered_row)

        predictions = pd.DataFrame(prediction_columns).sort_index()
        pairwise = []
        for left_index in range(len(run_ids)):
            for right_index in range(left_index + 1, len(run_ids)):
                pairwise.append(
                    float(
                        np.mean(
                            predictions[run_ids[left_index]]
                            == predictions[run_ids[right_index]]
                        )
                    )
                )
        run_rows.append(
            {
                "task": setting["task"],
                "model": setting["model"],
                "mean_pairwise_agreement": float(np.mean(pairwise)),
                "unanimous_share": float(
                    predictions.nunique(axis=1, dropna=False).eq(1).mean()
                ),
            }
        )

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    _label_distribution().to_csv(
        OUTPUT_DIR / "label_distribution.csv",
        index=False,
    )
    pd.DataFrame(summary_rows).to_csv(
        OUTPUT_DIR / "agreement_summary.csv",
        index=False,
    )
    pd.DataFrame(two_class_rows).to_csv(
        OUTPUT_DIR / "two_class_matrices.csv",
        index=False,
    )
    pd.DataFrame(ordered_rows).to_csv(
        OUTPUT_DIR / "ordered_matrices.csv",
        index=False,
    )
    pd.DataFrame(run_rows).to_csv(
        OUTPUT_DIR / "run_agreement.csv",
        index=False,
    )
    print("Validation outputs written.")
    return OUTPUT_DIR


if __name__ == "__main__":
    run()
