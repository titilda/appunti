#!/usr/bin/python3
import datetime
import pathlib
from subprocess import Popen
from io import StringIO


def build_docs_and_index():
    jobs = []
    with StringIO() as index_entries:
        for path in sorted(pathlib.Path("/workspace").rglob("*/*.md")):
            print(f"Adding {path} to the build queue")
            last_modified = datetime.datetime.fromtimestamp(path.stat().st_mtime).strftime("%Y-%m-%d %H:%M:%S")
            output_filename = path.parent / f"{path.stem}.html"
            if output_filename.name == "index.html":
                index_entries.write(
                    f'<li><a href="/{path.parent.name}/{output_filename.name}>{path.parent.name}</a></li>\n'
                )
            jobs.append(
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
                        # Output gets very messy with concurrent verbose output
                        # "--verbose",
                        "-o",
                        output_filename,
                        path,
                    ]
                )
            )
        original_html = ""
        with open("/workspace/index.html", "r") as index_file:
            original_html = index_file.read()

        with open("/workspace/index.html", "w") as index_file:
            index_file.write(original_html.replace("{{{PLACEHOLDER}}}", index_entries.getvalue()))

    print("Waiting for jobs to complete...")
    for job in jobs:
        job.wait()
    print("Done! :)")


if __name__ == "__main__":
    build_docs_and_index()

print("Done building docs! :)")
