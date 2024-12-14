# Appunti

Visita il sito per consultare gli appunti: https://appunti.titilda.org

## Aggiungere appunti

Puoi aggiungere nuovi appunti in formato Markdown (con $\TeX$) rispettando la *filename/directory structure* esistente. GitHub Actions è configurato per renderizzarli in HTML automaticamente usando Pandoc, generare una pulitissima pagina di indice con Bootstrap CSS, e caricarli sul sito GitHub Pages assieme al magnifico CSS custom presente su questo repo (cortesia di @etabeta1 🙌).

Per il commit, costruire il messaggio come [*indicato qui*](https://www.conventionalcommits.org/en/v1.0.0/).

**NOTA:** se vuoi apportare modifiche al repository senza lanciare un CI workflow (i.e. senza ri-renderizzare tutto), è sufficiente includere nel *Commit Message* il comando `[skip ci]`.

### Metadati

All'interno degli appunti è possibile aggiungere dei metadati.

Al momento i metadati supportati sono i seguenti

1. title
2. author
3. ~~date~~ (a partire dal 24 dicembre 2023 non è più necessario aggiungere manualmente la data di ultima modifica in quanto viene presa autimaticamente dalla data dell'ultimo commit per ciascun file)

Per aggiungere tali metadati ad un file markdown, è sufficiente aggiungere le seguenti righe al suo inizio:

    ---
    title: "Titolo"
    author:
    - "Autore 1"
    - "Autore 2"
    ---
