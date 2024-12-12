#!/usr/bin/python3
import datetime
import pathlib
from subprocess import Popen
from io import StringIO


def build_docs_and_index():
    with StringIO() as index_entries:
        for path in pathlib.Path("/workspace").rglob("*/*.md"):
            print(f"Processing {path}...")
            last_modified = datetime.datetime.fromtimestamp(path.stat().st_mtime).strftime("%Y-%m-%d %H:%M:%S")
            output_filename = path.parent / f"{path.stem}.html"
            index_entries.write(f'<li><a href="{output_filename}">{path.parent.name}</a></li>\n')
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
                    "--verbose",
                    "-o",
                    output_filename,
                    path,
                ]
            ).wait()

        with open("/workspace/index.html", "r+") as index_file:
            original_html = index_file.read()
            index_file.write(original_html.replace("{{{PLACEHOLDER}}}", index_entries.getvalue()))


if __name__ == "__main__":
    build_docs_and_index()

print("Done building docs! :)")
