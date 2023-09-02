# Appunti

Visita il sito per consultare gli appunti: https://appunti.titilda.org

## Aggiungere appunti

Puoi aggiungere nuovi appunti in formato Markdown (con $\TeX$) rispettando la *filename/directory structure* esistente. GitHub Actions è configurato per renderizzarli in HTML automaticamente usando Pandoc, generare una pulitissima pagina di indice con Bootstrap CSS, e caricarli sul sito GitHub Pages assieme al magnifico CSS custom presente su questo repo (cortesia di @etabeta1 🙌).

**NOTA:** se vuoi apportare modifiche al repository senza lanciare un CI workflow (i.e. senza ri-renderizzare tutto), è sufficiente iniziare il *Commit Message* con `skip ci`.
