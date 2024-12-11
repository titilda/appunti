#!/bin/bash
pwd
echo $HOME
echo ~
git config --global --add safe.directory /workspace

find . -type f -name "*.md" -mindepth 2 -print0 | sort -z | while read -d $'\0' doc
do
    echo "Processing ${doc}..."
    output_dir=$(dirname "${doc}")
    output_dir_name=$(basename "${output_dir}")
    output_html="${output_dir}/"
    output_html+=$(basename "${doc}" .md)
    output_html+=".html"
    lm_date=$(LC_ALL=it_IT.utf8 git log -1 --format=%ad --date=format:'%d %B %Y' -- "${doc}")
    pandoc --template "template.html"  -F mermaid-filter --toc --katex="https://cdn.jsdelivr.net/npm/katex@0.16.8/dist/" --metadata "toc-title=Indice" --metadata "lang=it" --metadata "date=${lm_date}" --verbose -o "${output_html}" "${doc}"
done
