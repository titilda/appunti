#!/bin/sh

find . -type f -name "*.md" -mindepth 2 -print0 | while read -d $'\0' doc
    do
        output_dir=$(dirname "${doc}")
        output_dir_name=$(basename "${output_dir}")
        output_html="${output_dir}/"
        output_html+=$(basename "${doc}" .md)
        output_html+=".html"
        cp ./style.css "${output_dir}"
        lm_date=$(LC_ALL=it_IT.utf8 git log -1 --format=%ad --date=format:'%d %B %Y' -- "${doc}")
        pandoc --template "template.html"  -F mermaid-filter --toc -c "style.css" --katex="https://cdn.jsdelivr.net/npm/katex@0.16.8/dist/" -o "${output_html}" "${doc}" --metadata "toc-title=Indice" --metadata "lang=it" --metadata "date=${lm_date}"
    done

find . -type d -mindepth 1 -maxdepth 1 -print0 | sort -z | while read -d $'\0' dir
do
    if [[ -f "${dir}/index.html" ]]; then
        sed -i "s@{{{PLACEHOLDER}}}@<li><a href=\"${dir}/index.html\">$(basename "${dir}")</a></li>{{{PLACEHOLDER}}}@" index.html
    fi
done
sed -i "s@{{{PLACEHOLDER}}}@@g" index.html