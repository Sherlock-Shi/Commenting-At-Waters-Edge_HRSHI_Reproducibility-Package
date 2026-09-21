import subprocess

from . import config


def run():
    subprocess.run(
        [
            "Rscript",
            str(config.DIAGNOSTIC_R),
            str(config.MONTHLY_OUTCOMES),
            str(config.DIAGNOSTICS_OUT),
        ],
        check=True,
    )
    return config.DIAGNOSTICS_OUT
