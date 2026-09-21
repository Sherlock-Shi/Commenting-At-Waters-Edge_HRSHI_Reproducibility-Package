import subprocess

from . import config


def run():
    subprocess.run(
        ["Rscript", str(config.INFERENCE_R), "--run"],
        check=True,
    )
    return config.INFERENCE_OUT
