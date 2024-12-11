#!/usr/bin/python3
import datetime
import pathlib
from subprocess import Popen

# find files with .md extension min depth 2
for path in pathlib.Path("/workspace").rglob("*/*.md"):
    print(f"Processing {path}...")
    # get the file's last modified time
    last_modified = datetime.datetime.fromtimestamp(path.stat().st_mtime).strftime("%Y-%m-%d %H:%M:%S")
    output_filename = path.parent / f"{path.stem}.html"
    Popen(
        [
            "pandoc",
            "--template",
            "template.html",
            "--toc",
            "--katex=https://cdn.jsdelivr.net/npm/katex@0.16.8/dist/",
            "--metadata",
            "toc-title=Indice",
            "--metadata",
            "lang=it",
            "--metadata",
            f"date={last_modified}",
            "--metadata",
            "maxwidth=100vw",
            "--metadata",
            "margin-left=0",
            "--metadata",
            "margin-top=0",
            "--metadata",
            "margin-right=0",
            "--metadata",
            "margin-bottom=0",
            "--verbose",
            "-o",
            output_filename,
            path,
        ]
    ).wait()

print("Done! :)")
# TODO: move index build process here
