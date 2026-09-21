import sys

import pandas as pd

from src import config, diagnostics, inference, measures, monthly, validation


def main():
    config.ensure_output_dirs()
    print("Output directories ready.")
    cases = pd.read_parquet(config.CASES)
    pbi = pd.read_csv(config.PBI_INPUT)
    stance = pd.read_csv(config.STANCE_INPUT)
    print("Inputs loaded.")
    measures.verify(cases)
    print("Measures verified.")
    monthly_outcomes = monthly.build(cases, pbi, stance)
    monthly_outcomes.to_csv(config.MONTHLY_OUTCOMES, index=False)
    print("Monthly outcomes written.")
    validation.run()
    print("Validation finished.")
    diagnostics.run()
    print("Diagnostics finished.")
    inference.run()
    print("Inference finished.")


if __name__ == "__main__":
    if len(sys.argv) != 1:
        raise SystemExit("No arguments are accepted.")
    main()
