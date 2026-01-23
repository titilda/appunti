---
title: "Bioinformatics for Computational Biology"
author:
    - "Niccolò Papini"
---
## Introduction

Hello reader this is a short summary of our notes about the course "FAI". If you find an error or you think that one point isn't clear please tell me and I fix it (sorry for my bad english). -NP

## Chapter  One: The cell

**Cell**: Unit of living being

Divided in:

- Prokaryotes
- Eucaryotes

**Prokaryotes**: Have a nucleus not clearly separated from the rest of cellular matter. Are unicellular organisms.

Structure:

- Cell wall;
- Plasma membrane;
- Cytoplasm;
- Chromosome;
- Ribosome;
- Flagellum.


**Eukaryote**: enclosed by a plasma membrane that contain:

- Cytoplasm;
- Nucleus, enclosed by nuclear envelope.

### 1.1 The Big Four

All the cell are constituted by 4 main types of macromolecules:

- Proteins -> constituted by aminoacids;
- Polysaccharides;
- Lipids;
- Nucleic acids (e.g DNA).


Aminoacids' structure:

1 Central atom of carbon ($C$) linked with:

- 1 atom of $H$
- 1 amino group ($-NH_2$)
- 1 carboxylate group ($-COOH$)
- 1 side-chain ($R$)

Nucleic acids:

- **Ribonucleic acids, or RNA**
- **Deoxyribonucleic acids, or DNA**

**DNA**= contains ALL the genetic information that is necessary for the life of the host organism. It's organized in chromosomes.

Prokaryotes have only one chromosomes.

A cell can be haploid, diploid, triploid,...

This means that the n° of the chromosomes are $n$, $2n$, $3n$,...

### 1.2 Mitosis

**Mitosis** -> Asexual reproduction for a single cell.

!["Mitosis"](assets/chapter_one/Mitosis-Equational-Division-july4.png)

### 1.3 Meiosis

**Meiosis** -> Sexual Reproduction between:

- One male sexual cell -> spermatozoon
- One female sexual cell -> ovum

!["Meiosis"](assets/chapter_one/meiosis.jpg)

## Chapter Two: Mendelian Genetics

In the 1865 Gregor Mendel create the **inheritance transmission laws**

### 2.1 First Law: Law of dominance 

Mendel crossed plans that differ for only one alternative discrete character so pure dominant ($RR$) and pure recessive ($rr$)

![](assets/chapter_two/punnett's%20table.jpg)

The result that him obtain was all the four children displayed only dominant traits.

### 2.2 Second Law: law of segregation

- Each individual produces equal quantity of gametes with one or the other allele
- Gametes combine **randomly**

!["Second generation"](assets/chapter_two/second_law.jpg)

Ratio is $3:1$

3 dominant;

1 recessive.

### 2.3 Third law: law of trait independent segregation

In F3 with 2 traits ratio is $(3:1) * (3:1) = 9:3:3:1$

9 dominant;

3 dominant trait and recessive

3 recessive and dominant

1 pure recessive

The second law evident that:

- The number of traits in an organism is much largest than the number of chromosomes -> each chromosome must contain more than one gene.

- Gene are not always inherited independently.


**Linkcage:** association between genes or groups of gene.

Thomas H. Morgan demonstrated the association among different genes of a chromosome exists, but is not total.

**crossing - over**: during meiosis, homolog chromosomes can exchange genetic material.

![](assets/chapter_two/crossing-over.jpg)

1 centimorgan $\implies$ percentage of a recombined chromosomes in off springs, as a measure of the relative distance for a gene pair.

The percentage of recombination between two genes is proportional to their relative distance.

### 2.4 Sexual characters

Inheritance of genetic characters has particular importance for genes located on sexual chromosomes.

Sexual chromosomes: $X$ and $Y$

- $XX$ -> female
- $XY$ -> male

In human, genes located in $X$ and $Y$ are associated with sex.

There are traits, influenced by sex and limited by sex.

Some pathologies linked to genetic alterations on $X$ or $Y$:

- Color blindness ("Daltonismo", incidence $8\%$ men, $0.00064\%$ women);
- Hemophilia ("Emofilia");
- Turner's syndrome (one $X$ chromosome in female sex, it brings short height and sterility);
- Klinefelter's syndrome (an extra $X$ chromosome in male sex, so $XXY$);
- Aneuploidy: anomaly in the number of chromosomes.

For the genetic transmission the normal allele is dominant, the mutated one is recessive.

Gene can interact, generating unpredicted phenotypes and anomaly, like:

- Incomplete dominance -> pink flowers (red x white)
- Co-dominance -> AB blood group
- Polygenic inheritance -> expression of more than one gene

**Phenotype**:  group of observable characteristics.

phenotype: genotype + environment 

Same genotype can express different phenotype example: a female bee with the same chromosome complement of others, can became queen bee only if fed with royal jelly, otherwise it becomes workers bee.

1908, G.Hardy and W. Weimberg $\implies$ in a balance population frequencies of genes and genotype tend to remain constant.

Given the distribution of paired $A$ and $a$ alleles we want to know the relative frequencies.

Pair: $AA, Aa, aa$ -> the frequencies of $A$ is unknown because it is present in $AA$ and in $Aa.$

$p:$ frequency of $A$.

$q:$ frequency of $a$.

$p+q = 100 \% \implies p = 1 - q \rightleftarrows q = 1 - p$

$AA: p^2$

$aa: q^2$

$aA: 2pq$

So if $AA + 2Aa + aa = (A+a)^2 = 1$

::: {.callout .callout-definition title="Hardy - Weimberg's law"}
$p^2 + 2pq + q^2 = (p+q)^2 = 1$.
:::

Now we proved that the frequencies remain constant.

Given as known that $AA$ is $p^2$, $Aa$ is $pq$ we can said that $f_A: p^2 + pq$, but we know that $q = 1 - p$ so:

$$
\begin{align*}
    f_a : p^2 + p(1-p) \implies p
\end{align*}
$$

we can do same reasoning fo $f_a$

When this happen we said that the population is balanced.

**Fitness:** The measure of reproductive ability of an individual.

## Chapter Three: Molecular genetics

### 3.1 DNA

DNA (Deoxyribonucleic Acid)

- Biggest macromolecule in the cell.
- Polymer composed by 4 monomers, **nucleotides**:

  - 1 molecule of sugar, with 5 atoms of carbon that bond with:

    - 1 phosphate group (phosphoric acid $-P$)
    - 1 molecule containing nitrogen ($N$) ($1$ of the $4$ nitrogenous bases: $A,C,G,T$)

![](assets/chapter_three/DNA.jpg)

**DNA Chain:** long sequence of nucleotides linked by the bond between the phosphoric acid of a nucleotide and the sugar of the subsequent nucleotide.

This bond is called $3' - 5'$

![](assets/chapter_three/DNA_Chain.jpg)

- Adenine ($A$) and Guanine ($G$) $\implies$ Purines
- Thymine ($T$), Cytosine ($C$) and Uracil ($U$) $\implies$ Pyrimidines

In 1953, Watson and Crick defined the exact spatial structure of DNA considering two experimental result:

- DNA base's composition;
- Diffraction spectra from $X-rays$ of pure DNA fiber's crystals.

In 1945 Chargaff found that in the DNA of each organism.

::: {.callout .callout-definition title="Chargaff's rules"}
- $n°\text{Adenine}(A) = n°\text{Thymine}(T)$.
- $n°\text{Cytosine}(C) = n°\text{Guanine}(G)$
- or $\frac{A}{T} = \frac{C}{G} = 1$
:::

In 1945 Rosalind Franklin and M. Wilkins obtained the first photographs of diffraction spectra from x rays of pure DNA fibers' crystals, that showed:

- DNA with helix structure.
- 2 characteristic periodicity, at $0.034 \mu m$ and $0.0034 \mu m$ on the main molecular axis.

From this information:

- Model of double helix;
- The structure of the DNA spontaneously arranges the temperature, pH (acidity) and humidity, with following characteristics:

  1. Double helix, two polynucleotide chains, right-handed coil. ![](assets/chapter_three/right_handed_coil.jpg)
  2. Nucleotide bases are arranged in the internal part of the helix, backbone sugar-phosphate is in the external part.
  3. Nucleotide bases interact by weak hydrogen bonds ($H$)
     -   $A$ and $T$, $2 H-$ bonds.
     -   $C$ and $G$, $3 H-$ bonds.
         
         $A-T$ and $C-G$ are called **pairs of complementary bases**
  4. $H-$ bonds are also strengthen structure of the helix.
  5. There are $10$ pairs of bases for each helix turn. Pairs are spaced of $0.00034 \mu m \implies$ pitch $0.0034 \mu m \implies$ double helix diameter: $0.002 \mu m$
  6. The two nucleotide chains are anti-parallel $\implies$ opposite direction (one from $3'$ to $5'$, the other from $5'$ to $3'$)
  7. Double helix has $2$ grooves on the surface, one bigger than the other, in these grooves, protein interactions occur for DNA replication and genetic transcription.

**bp:** base pair $\implies n°$ of base pair before a regular behaviour

DNA's structure types:

- Double helix of Watson and Crick $\implies$ **B-Shape** characteristic of living cells, if humidity is low DNA is more compact and wide $\implies$ **A-Shape**.
- Exist also a DNA structure $\implies$ **Z-Shape**, with zigzag course backbone sugar-phosphate, thinner structure and left-handed coiling.

Each human cell $\to$ $1m$ of DNA $\implies$ **packaging**, technique that ultra compress the DNA.

### 3.2 RNA Structure

RNA is the protagonist in the synthesis of proteins.

Structural different point of view, RNA to DNA:

- Ribose as sugar.
- $U \to$ Uracil instead of $T$.
- Always exists as single chain -> in a complex tridimensional structures.

In some living being without DNA, RNA plays a leading role in reproduction process.

In cells, there are different types of RNA:

- Messenger RNA (mRNA);
- Ribosomal RNA (rRNA);
- Transfer RNA (tRNA);
- Small nuclear RNA (snRNA);
- Small interfering RNAs (siRNA);
- micro RNA (miRNA);
- long non-coding RNAs (long ncRNAs);
- Antisense RNAs.

Of all these, mRNAs, rRNAs and tRNAs play important roles in proteins synthesis, others in regulation.

### 3.3 Virus

**Genome:** genetic material of an organism.

Generally indicates DNA, often also RNA and proteins.

- Bacterial cells -> haploid genome ($n$)
- Majority of Eukaryote cells -> diploid genome ($2n$)

**Viruses**: The most simple life forms -> cellular parasites.

They must use another cell for reproduce themself.

**Virus genome:** 1 molecule of nucleic acid (DNA or RNA) enclosed in a protein shell (capsid) with different shapes.

The viruses can be divided in 3 classes:

- Virus of bacteria;
- Virus of plants;
- Virus of animals.

**Bacteriophages**: capsid with icosahedral head, containing genetic material, connected to a hollow cylinder (tail) to which filamentous structures (spikes) are linked, which allow the hanging of the virus on the bacterial cell's wall.

![](assets/chapter_three/bacteriophage.jpg)

When hanged, virus injects it genetic material inside the cell, where it reproduces itself.

#### 3.3.1 Virus of eukaryote cells

- Capsid mainly icosahedral or filamentous.
- Genetic material is variable in structure:
  - single or double helix
  - linear or circular
  - segmented or complete
- Majority of plants' virus are RNA virus
- Many viruses can include their genome in the host cell $\implies$ **Cellular transformation**

**Cellular transformation:** phenomena coming from integration of virus in host cell's DNA.

#### 3.3.2 Retrovirus

**Retrovirus:** e.g Human Immunodeficiency Virus (HIV).

- $1$ or 2 molecules of RNA.
- Enzyme reverse transcriptase;

$$
\begin{align*}
  \text{Viral RNA} \xrightarrow{\text{reverse transcriptase}} \text{Viral DNA}
\end{align*}
$$

Then transformed in double helix by another enzyme active in cell nucleus, **DNA polymerase**.

$\implies$ genome virus can integrate in the host cell's genome and reproduce itself.

### 3.4 Bacterial genome

**Bacterial cells**: (prokaryotes) don't have a defined nucleus, but a compact structure (nucleoid):

- $1$ molecule of DNA
- usually circular
- $1mm \to$ bacterial cells in $1 \mu m$
- super-coiling
- $2k-2.5k$ genes in continuous sequence.

In many bacteria, there are also small circular molecules of DNA $\to$ **plasmid**

- advantageous characteristics;
- can integrate in cellular genome and detach themselves bringing a variable genetic mix with mix.

Exist variant plasmid:

- **R plasmid** (Resistance plasmid): determines cell's resistance to cations of heavy metals or antibiotics.
- **Degradation plasmid**: Allow the bacterium to metabolize stable chemical compounds $\to$ pollutes areas' recovery.
- **Fertility factor:** cells containing it are called male ($F^+$), others female ($F^-$):
  - sex pilus $F^+$ cells can transfer to $F^-$ a copy of $F$ plasmid -> conjugation $F^- \to F^+$.
  - Conjugation allows "horizontal transfer" of genetic material.

### 3.5 Genome of Eukaryote

- More complexity
- Contains different linear molecules of DNA -> each contained in a chromosome .
- $\#$ chromosomes is **NOT** proportional to dimension of genome.

Chromosomes $\xrightarrow{constituted}$ chromatin $\implies$ 

- $50\%$ DNA.
- $50\%$ protein and RNA.

If a protein is strictly linked to DNA is called **histones**

Chromatin has structured like a necklace:

![](assets/chapter_three/Chromatin.jpg)

**Context: cellular division**

**Metaphase:** Chromosomes assume the X shape:

- 2 linear elements $\to$ **chromatids**
- the center is called **centromere**

![](assets/chapter_three/metaphase.jpg)

The position of centromere, length of chromatids and dimension of chromosomes identify different chromosomes -> **karyotype** of the organism.

I can highlight areas with rich in $A$ and $T$ bases

![](assets/chapter_three/bands.jpg)

the areas with rich in $C$ and $G$ remains pale.

I generated a striped arrangement -> **bands**

**Nomenclature:** Each band has a specific nomenclature (e.g. $6p21.3$)

- $\#$ chromosome (6);
- arm (p -> short one);
- region (2 group of bands visible in the arm starting from centromere);
- band (1 -> counting from centromere to telomere);
- sub-band (3 -> counted from centromere).

Not all genetic material of eurokaryotes is in the nucleus.

Small fraction of circular DNA is in cellular organelles:

- Only in vegetal cells -> **chloroplasts**
- **mitochondria**

Extra-nuclear genes in **cytoplasm** are transmitted to the off spring just by the ovum -> **maternal inheritance**

To guarantee the transmission, the DNA is copied -> **duplication**

Same process in eukaryotes and prokaryotes, semi-conservative -> each daughter has $1$ strand of DNA  of the mother and $1$ of new synthesis.

**Semi-conservative duplication:**

- Zip opening of the double helix
- Exposition of single bases -> act as mold.
- Pairing of bases between free nucleotides in the cell and the complementary ones in the mold ($H-$ bond)
- Nucleotides of paired bases bond to create new strand.

The $2$ double helixes consisting in:

- $1$ parental helix
- $1$ new synthesized helix

**Replication fork:** Area where double helix opens and synthesis starts.

The duplication happens in specific position at a time (not concurrent):

1. Use a specific enzyme for opening localization.
2. Copying pairing of bases and polymerization of nucleotides.
   
   $\xrightarrow[\text{enzymes}]{}$ **DNA polymerase $Ⅲ$ and $Ⅰ \implies$ both direction 

   On $5'$ to $3'$ copying.

   On $3'$ to $5'$ copying but we have **okasaki fragments**, segment of DNA that are synthesized and linked on DNA discontinuously by **DNA ligase** enzyme.

3. Re- closing of double helix by specific enzymes.

### 3.6 Gene structure

**Codon:** triplet of bases.

But how the gene is structured ?

- **Prokaryotes**

![](assets/chapter_three/prokaryotes.jpg)

- **Eukaryotes**

![](assets/chapter_three/eukaryotes.jpg)

**start codon**: unique triplet which declares the beginning of the gene.

**stop codon:** unique triplet which declares the end of the gene.

**Human genome** -> $3000 Mb$ (Mega bases) $\implies 22k - 25k$ genes.

- Coding area $90 Mb$ ($3\%$ of genome).
- $> 50\%$ of genome -> repeated sequences:
  - Tandem repeats ($10\% - 15\%$ DNA) repetitions adjacent $ATTCG\bold{ATTCG}$
  - Interspersed repeats ($35 - 40\%$ DNA) repetitions are scattered along DNA.

**Central Dogma of Molecular Biology** (Crick 1958)

![](assets/chapter_three/central_dogma.png)

Transcription DNA to RNA from $5'$ to $3' \implies$ only one helix of DNA is used -> **mold helix**

Synthesis is catalyzed by enzyme **RNA polymerase** -> different in prokaryotes and eukaryotes.

- In prokaryotes:
  - $5$ subunit ($2 \alpha = \alpha_2, 1 \beta, 1 \beta'$ and $1 \sigma$)
  - Transcribes all types of RNA

3 phases:

1. Starting transcription
   
   - RNA polymerase bonds (with its $\sigma$) with gene's promoter.
   - $\sigma$ detaches and transcription starts
2. POlymerization of polynucleotide RNA -> **elongation**
3. Detaching of synthesized RNA  and end of transcription.

![](assets/chapter_three/rna_polymerase_prokaryotes.jpg)

**Transcription factors**: proteins;

The transcription factors  necessary for each polymerase.

RNA polymerase synthesizes RNA in continuous way.

- Eukaryotes, genes are fully transcribed -> **pre-RNA**

**splicing:** In eukaryotes, remove intron from pre-RNA -> **mature RNA**

There are different types of splicing:

![](assets/chapter_three/splicing.jpg)

In a gene with alternative splicing, the majority of exons is always included in final mRNA.

Exist 4 types:

- **cassette exons:** full exons transcribed only in some cases.
- **Isoforms of introns/exons:** boundaries of introns and/or exons can be different, with clipping/extension.
- **Introns retention:** Introns can be contained in final transcript.
- **Mutually exclusive exons:** different exons can be included in different final transcripts.

Splicing is not well-know process.

In some cases, small Nuclear RiboNucleoProtein (SNRNP) cut:

1. $5'$ end of intron by dinucleotide $GU$
2. In $3'$ end by dinucleotide $AG$
3. exons link together.

Exist some intron that follow $GU-AG$ rules without SNRNP -> **auto-splicing** -> these RNA are called **ribozyme**

After splicing -> **stabilizing mRNA** by adding:

- At initial extremity $7-$MethylGuanine
- At final extremity Adenines.

### 3.7 Different types of RNA 

- **Ribosomal RNA (rRNA)**

Structural and functional components of ribosomes, where the synthesis of proteins occurs.

- **Transfer RNA* (tRNA)**

- Small molecules ($75 - 95$ nucleotides)
- Function: transport of aminoacids to RNA bonded to ribosomes.
- tRNA -> cloverleaf structure -> NO bases' pairing (loops)
  - Acceptor stem (extremity $3'$) -> a particular aminoacid can link;
  - Anticodon area, 3 bases complementary to codon of mRNA to translate.

- **Messenger RNA (mRNA)**

- mRNA decides aminoacids sequences of codified protein;
- Intermediate between genes and protein;
- In prokaryotes, mRNA are translated after transcription.
- In eukaryotes, numerous modification, among which splicing before translation.

rRNA, tRNA and mRNA participate in translation.

### 3.8 Genetic Code

**Genetic code:** group of rules defining how the information of nucleotides' sequence in mRNA ($4$ bases $A,G,C,U$) is translated in aminoacids' sequence of the codified protein ($20$ aminoacids).

- Valid for almost all cells -> exception for mitochondrial genes of some organisms;
- $4^3 = 64$ possible triplets (codons)

Each codon:

- Insertion of specific aminoacids
- "Start" or "End"

![](assets/chapter_three/table_codon.png)

#### Feature of genetic code

- Each aminoacid is codified by triplet of bases.
- Triplets are "read", Open Reading Frame, OFR one after the other, without interruption.
- Each triplet can codify only one of $20$ aminoacids.
- $AUG \to \text{Met} \to$ START
- $UAA, UAG, UGA \to$ STOP -> NO aminoacids

This characteristic about triplets may occur concurrent encoding cause by splicing and mostly alternative splicing.

Some alternative transcripts are tissue-specific $\implies$ expressed only in one specific type of cell.

Mechanisms of genetic code and alternative splicing allow encoding and production of many proteins with different functions from the same DNA.

### 3.9 Translation

**Translation:** complex process involving many cellular components: rRNA, mRNA and tRNA.

tRNAs are junctions between nucleotides of mRNA and aminoacids of protein:

- Anticodon area bonds to codon on mRNA that codifies a specific aminoacid (Phe);
- In extremity $3'$ of tRNA only the aminoacid "Phe" bonds specifically and with  covalent bond.

The translation is same for prokaryotes and eukaryotes, has 3 phases:

1. Start
   - Ribosome bonds to mRNA by starting triplet (AUG);
   - Identification of mRNA's AUG triplet by complementary specific tRNA triplet (anticodon)
   - Bond of tRNA that brings aminoacid corresponding to AUG triplet (Met)
2. Synthesis:
   - Process goes on;
   - Ribosome moves along mRNA;
   - Only 1 triplet available at a time for bonding to specific tRNA;
   - Aminoacids brought by tRNA are near;
   - When ribosome moves, a peptide bond is created between last aminoacid transported and the previously one.
   - Protein chain extends due to ribosome moving.
3. End:
   - When ribosome reaches a stop triplet (UAA, UAG, UGA)
     - Detaches from mRNA.
     - Sets protein chain free.

Each ribosome builds only 1 protein at a time.

In bacteria (prokaryotes), requiring synthesis of many copies of the same protein in short time (minutes):

- More than one ribosome translated concurrently the same mRNA.
- Ribosomes can start translation of mRNA before its synthesis is completed.

In bacteria transcription and translation are paired.

**Central Dogma**

![](assets/chapter_three/central_dogma_gen.jpg)

![](assets/chapter_three/central_dogma_ex.jpg)

Not all genes are always necessary for the life of a cell -> only constituent one (necessary for the life of the cell) are always expressed, other expressed when necessary.

### 3.10 Genetic Expression

Expression of genes is controlled by cellular needs: environment conditions and functions to execute.

- Bacterium Escherichia coli (E. coli) living in human intestine and "eat" sugar. It can switch on and off the genes that produce enzymes for all types of sugar, is only one type is present it putting out the genes that codify enzymes for other types of sugars.
- In plant cells, genes of photosynthesis activated by sun light.

**Multi-cellular organisms**:

- Environment of a cell is the organism itself: single cells answer to stimuli produced by other cells of the organism.
- In addition, a mechanism called **differential regulation** -> one cell is divided in many specialized cells.
- Humans, $\cong 250$ types of cells
- Variety genetically established very early during growth of zygote. Only **stem cells** can differentiate in specialized cells.

**Bacteria**

Francois Jacob and Jaques Monod (1960 - 64) use lactose in E.coli.

Lactose is a disaccharide (sugar of 2 monomers, glucose and galactose) that can be utilized when divided into the 2 components inside the cell.

Splitting of lactose is realized by enzymes codified by 3 genes:

- IacZ -> $\beta -$galactosidase
- IacY -> lactose-permease
- IacA -> lactose-transacetylase

In default of lactose, in the cell $\cong 5$ molecules of each enzyme.

As for the sugar, if lactose is the only source of energy, synthesis of enzymes is rapidly stimulated $\implies$ **inducible enzymes**

Genes IacZ, IacY and IacA -> **structural genes**, are consecutive on bacterial chromosomes and transcribed in the same mRNA.

Before the big three, there is IacI that regulates them ; its elimination brings continuous synthesis of $3$ the enzymes.

- IacI codifies protein (repressor), bonds to and area on chromosome called operator (o), between promoter (p) and first of them (IacZ)

![](assets/chapter_three/lactose.jpg)

**Mechanism of regulation**

Repressor bonded to operator prevents RNA polymerase transcription of $3$ structural genes.

If lactose is present, it bonds to repressor, changes its 3D conformation preventing its bond to operator.

- Repressor detaches from DNA allowing transcription of operon genes.

When lactose is totally consumed, repressor bonds again to operator and synthesis stops.

**Superior Organisms**

Main mechanisms are similar but regulation is more complex.

Genetic expression regulated by proteins, **transcription factors**, bond DNA sites before gene, **Transcription Factor Binding Sites**, and can allow or stop bond of RNA polymerase to promoter of gene.

Example

Protein metallothionein that protects cells from toxic effect of metals free in the environment:

- Small quantities of metallothionein are always present in the cell.

Gene of metallothionein is transcribed by RNA polymerase $II$

Many traits of DNA before gene are involved in its expression:

- Binding site of polymerase;
- Sequences (enchanters), probably control tissue-specific expression of gene.

Such zones, elements of response to metals, modulate transcription based on metals' concentration.

Transcription factors have leading role in regulation:

- They have structure that let them enter in DNA grooves and interact with nucleotide bases -> **DNA binding proteins**

More common structure $\implies$ **helix-turn-helix** and **zinc-finger**

### 3.11 Proteins

**Proteins:** Macro-polymers constituted by linking of aminoacids (minimum 3); there are 20 aminoacids:

![](assets/chapter_three/20_amino_acids.png)

![](assets/chapter_three/amino_acid.jpg)

- 1 central atom of carbon
- 1 atom of hydrogen
- 1 amine group ($-NH_2$)
- 1 carboxylic acid group ($-COOH$)
- 1 side chain (R), it depends on the aminoacid. -> determines chemical properties

**Peptide:** short polymer constituted by the linkage of aminoacids bonded with **peptide bonds.**

**Peptide bond:** bond between $N-$terminus of an aminoacid and $C-$terminus of another one -> planar and rigid $\implies$ NO rotable-bond

**Polypeptides:** have $1$ free $N-$terminus (beginning) and $1$ free $C-$terminus (end) -> contains from 3 to various hundreds of aminoacids.

- 3D conformation of polypeptide is determined by torsion angles around $C \alpha - N (\phi)$ and $C \alpha - C (\psi)$ bonds of each aminoacid.
- The possible values of $\phi$ and $\psi$ depend on side chain of aminoacid.

![](assets/chapter_three/peptide_bond.jpg)

Proteins have different functions, ultimate for all organisms:

- Energetic
- Immune
- Structural of support (constitute backbone of cell)
- of transport (oxygen, lipid, metals)
- of identification of genetic identity
- Enzymatic (catalyze)
- Hormonal (lead regulative functions and transmit signals within the organism)
- Contractile

Function executed by protein depends on properties of protein, determined by:

- Aminoacidic sequence it is composed of
- Mainly of 3D structure adopted by the protein.

### 3.12 Proteins' structure

It's structured in 4 related levels:

- Primary structure: sequence of amino acids.
- Secondary structure: local 3D conformation with regular and repetitive bonding of polypeptide chain in substructure with well-defined and fixed geometric structures:
  - Spiral $(\alpha -\text{helix})$: pitch $0.54 nm, 3.6$ residual per turn; hydrogen bonds inside polypeptide chain.
  - Plane $(\beta - \text{strand}, \text{or} \beta - \text{sheet})$: in parallel or anti-parallel shape depending on direction of polypeptide chain, stabilized by hydrogen bonds between adjoining part of the chain.
  - Loops: linkages between $\alpha -$helix and $\beta -$strand (often they change their direction) averagely in protein $40 \%$ loops and $60 \% \ \alpha-$helix and $\beta -$strand.

![](assets/chapter_three/protein_structure.jpg)

- Tertiary structure: 3D arrangement of secondary structure elements in the environment.
  - Stabilized by hydrogen bonds, disulfide bonds, Van der Waals forces and hydrophobic interactions.
  - Assumed shape is the one with lower free energy.
  - Mainly globular or fibrous.
  - Defines properties and function of protein.
- Protein domain: part of the sequence and protein structure that can exist, work and evolve independently of the remainder of protein chain.
- Each domain has stable 3D structure and folds always in the same way, independently of the environment.
- Length: $25$ to $500$ amino acids -> shortest one is the zinc finger.
- It can be present in proteins evolutionary-related.
- Often corresponds to a functional unit.
- Quaternary structure: spatial organization of multi-subunit complexes (two or more polypeptides with defined tertiary structure, linked in specific way):
  - phosphorylase ($4$ sub-unit);
  - hemoglobin ($2$ sub-unit)

- Primary structure highly determines tertiary one.
- 3D conformation is vital for biological activity of protein
  - Denatured protein doesn't execute its function, until tertiary structure's restoration.

**Isoform:** two protein which are different for little details, due to alternative splicing or to polymorphisms.

### 3.13 Genetic mutations

During duplication of DNA it is possible to have variation in the sequence of nucleotide bases (**mutations**) that are transmitted to offspring (**mutants**):

- Rare ($1:10k - 1M$ individuals);
- Mainly spontaneous and accidental;
- Can generate individuals with new characteristics.

- Vital for evolution.
- Can be pathogenic:
  - Direct cause of abnormal phenotype.
  - Increased susceptibility to a pathology -> more likely to contract the pathology
- Mutations can be lethal for single individual:
  - All organisms have various cellular mechanisms to fix possible damages to DNA
  - Low $\%$ of codifying DNA on total DNA decreases probability of mutation in codifying areas
- Non-lethal mutations are transmitted to offspring (prole), introducing between individuals of a species.
  - **Polymorphisms:** Frequency in population $> 0.01 \%$
- In an individual, the majority of mutations is inherited.
- Genome is unique for each individual.

**Single Nucleotide Polymorphism**, or **SNP**: is the variation of $1$ single nucleotide in an individual's DNA sequence.

- The frequency of SNPs in human DNA $> 1 \%$
- SNPs in codifying regions have more chance of altering biological functionality

$$
\begin{align*}
  CCU \to \text{Pro} \xrightarrow{SNP} CCC \to \text{Pro} \text{in this case nothing change}
  \\
  AAG \to \text{Lys} \xrightarrow{SNP} GAG \to \text{Glu} \text{in this case SNP change the protein synthesized} 
\end{align*}
$$

SNPs are likely to be good biological markers.

- **Haplotypes:** group of SNPs always present together.
- Many common human pathologies are caused by complex interactions among genes, environment and lifestyle.

### 3.14 Types of genetic mutations

3 classes of mutations:

- **Genetic:** alter gene's structure, also SNP.
- **Chromosomal:** alter chromosomal structure.
- **Genomic:** alter number of chromosomes.

#### 3.14.1 Genetic Mutations

Genetic mutations can derive from different alterations:

- **Substitutions:** of a base with another.
- **Insertion** of one base.
- **Deletion** of one base.
- **Inversion** of short nucleotide sequence, due to excision of a stretch of double helix followed by re-insertion in opposite direction.

In the substitution scenario is possible have a situation like: $A-T \xrightarrow{substitution} G-T$, in this case, $G-T$ is an unstable bond and in the next replication we can have $G-C$ or $A-T$

- Mutation can be effective or not on phenotype:
  - **neutral** (or **silent**) if it brings a new codon codifying the same amino acid
  - **missense** if it brings a new triplet codifying a different amino acid

- If the amino acid has important role, but protein not essential for life, cell survives; otherwise mutation is lethal.

#### 3.14.2 Chromosomal mutations

Chromosomal mutations: changes in chromosomal structure compared to normal karyotype

- Easily detectable with optical microscope in dividing cell.

Main types of chromosomal anomalies:

- **Deletion**
- **Duplication**
- **Inversion**
- **Translocation**

- **Deletion**: loss of a stretch of chromosome of variable length
  - Phenotypical consequences depend on genes lost
    - In homozygotes, deletions often lethal.
    - In heterozygotes, effects can be (partly) balanced by normal genes on homolog chromosome

- **Duplication:** doubling of a stretch of chromosome
  - **In tandem** when segment is repeated in the same direction
  - **Inverse** when duplication has opposite direction

Less harmful than deletion

- **Inversion:** change of order of two or more genes in a chromosome
- **Translocation:** Alteration of structure due to detachment of a stretch of a chromosome and attachment to non-homolog chromosome:
  - **Simple**
  - **Mutual** when involves $2$ chromosomes

**!!! In human, involved in tumor on set !!!**

#### 3.14.3 Genomic Mutations

Genomic mutations concern total number of chromosomes in each cell of an individual

- **Polyploidy** (or **euploidy**) if number of chromosomes is multiple of haploid arrangement ($3n, 4n, ...$)
  - In plants common
  - In animal dangerous
- **Aneuploidy** if loss or addition of 1 or few chromosomes
  - In animals strong alterations in phenotype
  - In plants common

Example:

- Turner's syndrome: due to lack of one $X$ chromosomes in female sex -> short height and sterility $1:5k$ women.
- Klinefelter's syndrome: due to one extra $X$ chromosome in $XXY$ individuals -> male aspect, small testicles, developed breast, tall height and backwardness $1:500/2k$ men

From errors in meiotic process, like failed disjunction in pair of homolog chromosomes:

- 1 gamete has pair of chromosomes -> **trisomic**
- 1 gamete without chromosomes -> **monosomic**

### 3.15 Mutagens

Frequency of mutations can increase if organism is exposed to substances and radiations (**mutagens**) that interact with DNA and can induce changes in nucleotide sequence.

- **Physical:** radiations with different wave length
  - Heat: break bond between A or G base and sugar, base lost -> **apurinic site**
  - High energy radiations (wave length $< 30 \mu m$)
    - Ionizing radiations (cosmic rays, $\gamma$ rays and $x$ rays): penetrate cellular tissues and ionize molecules -> provoke breaking of DNA, loss of bases -> used for tumoral treatment.
    - Ultraviolet radiations (wave length $\cong 26 \mu m$ absorbed by DNA bases): can provoke formation of dimers of T or C, like 

![](assets/chapter_three/dimer.jpg)

- **Chemical**
  - Analogs of nucleotide bases: can substitute normal bases during replication
  - Reactive of nucleic acids: chemically react with DNA bases altering them.
  - Intercalating agents of DNA bases: synthesized substances that inserts between bases -> brings genetic mutations of triplets reading

- **Environmental mutagenesis**: presence in the environment of many mutagenic substances capable of inducing tumors.

### 3.16 Fixing DNA and Genome

All living being have various cellular mechanisms for fixing DNA damages:

- **Photoreativation of dimers $\bold{T-T}$:** due to enzyme DNA photolyase, that cuts bond $T-T$ if activated by light.
- **Repair towards damage's excision:** complex process that requires many enzymes:
  - Detection of damage
  - Cutting of DNA the damage's area
  - Removal of damaged stretch of single helix $\implies$ excision.
  - Polymerization of missing stretch (DNA polymerase) and welding of extremities (DNA ligase).

In mankind lack or reduction of one or more involved enzymes is associated with inherited pathology that brings formation of skin tumors due to ultraviolet radiations present in solar rays.

**Genome:** Entire genetic material of an organism

- Identical in each cell of the same individual
- It's for $99\%$ the same in all individuals of the same species
- The term indicates also all products (RNAs, proteins, ...)

In bioinformatics, **genomic data/information:** whole of available data and information, related to genetic material of an organism.

**Transcriptome:** whole of all possible transcripts of an organism.

- In bioinformatics, **data/information of transcriptome:** whole of available data and information, related to all possible transcripts of an organism.

**Proteome:** whole of all possible proteins of an organism, deriving from different transcripts.

- In bioinformatics, **data/information of proteome:** whole of available data and information, related to all possible proteins of an organism.

### 3.17 Evolutionary biology 

**Evolutionary biology** is a sub-field of biology regarding the origin of species from a common ancestor, as well as their changes, multiplications and diversifications over time.

Then:

- **Homology:** similarity between characters due to descent from a common ancestor.
- **Similarity:** an observable property that comes with a significance measure.

Today:

- **Homology:** among proteins and DNA is often concluded on the basis of sequence similarity
- **Sequence similarity** may arise from different ancestors
  - Such sequences are similar, but no homologous.

Sequence regions that are homologous are also called **conserved**

Sequence homology may indicate **common function**

Homologous sequences are said orthologous if they were separated by a speciation event.

- **Orthologs**, or orthologous genes, are genes in different species that are similar to each other because the originated from a common ancestor.

Homologous sequences are said **paralogous** if they were separated by a gene duplication event.

- Paralogs typically have the same or similar function, but sometimes do not: due to lack of the original selective pressure the copy is free to mutate and acquire new functions.

So, homologous sequences can be divided into two groups:

- **Orthologs**: genes that share the same ancestral gene and perform the same biological function in different species
- **Paralogs:** genes within the same genome that share an ancestral gene and perform different biological functions.

**Phylogenesis**or phylogenetic: study of life's evolution

- Fundamental instrument that reconstructs relations of evolutionary kinship of group oof organisms in any systematic level.

**Taxonomy:** classification of organisms depending on similarities.

**Phylogenetic trees:** diagram that shows relation of common descent of taxonomic groups of organisms.

**Computational phylogenetic:** concerns the compilation of phylogenetic tree and the study of anatomic, biochemical, genetic and paleontological data used for their construction.

Phylogenetic trees are built on the base of a high number of genetic sequences.

Many techniques used to identify the best tree -> complexity NP (Nondet.Polinomial- time)

Phylogenetic trees are important but have some limits:

- Often don't represent exact evolutionary history of a gene or organism
- Are based on distributed data by different factors:
  - Genetic horizontal transfer
  - Hybridization between different species
  - Converging evolution
  - Conservation of genetic sequences


## Chapter Four: Biomolecular Sequence Analysis

Why do we do sequence comparison ?

- Measure yhe degree of similarity.
- Determine the correspondence between elements of distinct sequences.
- Observe the patterns of conservation and variability
- Deduce the evolutionary relationship
- Sequences alignment is the most important problem together with searching for a specific sequence in a database.
- It can help us to prediction of structure
- Similarity of sequence -> similarity of function
- Preserved positions may represent important function sites
- Phylogenetic analysis
- Homology, already discussed
- Similarity: Observable quantity that can be expressed as a percentage. -> degree of similarity

Two types of alignment:

- Global alignment: whole sequences
- Local alignment: defines the longest subsequence

Different technique:

- Alignment 2 sequence
  - Dot matrix
  - Pairwise Alignment
  - Needleman and Wunsch
  - Smith and Waterman Algorithm
- Multiple alignment
  - Dynamic programming
  - Heuristics
  - ClustalW

### 4.1 Alignment 2 sequences

#### 4.1.1 Dot matrix

Simplest one, we build a matrix with sequence 1 as column and sequence 2 as row and we put an "x" when we have a match.

![](assets/chapter_four/dot_matrix.jpg)

- Diagonal lines correspond to similarity regions.

**Filtering of background noise**

- Strong background noise
- Filtering can improve readability
  - We usa a stringency: minimum number of correct alignment in a window size.

Pros:

- All possible matches between two sequences are found
- You can find repeated sequences, direct and inverse
- Useful for quick visual inspection

Cons:

- Visual inspection
- Method not fully automated
- Image compression for long sequences

Practice

- To compare DNA: large windows and high stringency
- To compare protein small windows and not necessarily high stringency

#### 4.1.2 Pairwise alignment

Also simple, it's based in 3 type of action:

- Match
- Gap
- Mismatch

We write the two sequences and compare.

![](assets/chapter_four/pairwise.jpg)

$-$: gap

$C \\ | \\ C$: match

$G \\ | \\ C$: mismatch

We can assign a score, use:

gap $= - 2$

mismatch $= -1$

match $= +2$

highest is the score better is the alignment.

Distance between two strings:

- **Hamming distance:** defined between two strings of equal length as the number of mismatch
- **Levenshtein distance:** minimum number of operation (insertion, deletions, substitution) to transformation a string in the other.
- **optimal alignment:** of $S$ and $T$ (seq.1 and seq.2) that maximizes the score and reduce the distance between $S$ and $T$

Now we will talk about the score assign to gap, mismatch and match. Why ?

Because biologically the substitution cannot be consider equal each other.

We have to consider:

- Chemical and physically property
- Frequencies of substitution calculated on the protein sequences known to be homologous.

#### 4.1.3 Substitution matrix

**Substitution matrix:** Assign value to each possible pair of characters

- Nucleotides: simple scoring schemes are sufficient.
- Amino acids: their chemical differences should be considered.

There are two main types of matrices:

- PAM (Percent/Point Accepted Mutations)
- BLOSUM (BLOck SUbstitution Matrices)

##### PAM

PAM matrices: developed in the late 70s looking for mutations in closely correlated superfamilies of amino acid sequences.

Accepted Mutation: accepted by evolution.

For the construction of PAM matrices homogeneous blocks of aligned sequences are considered.

To avoid the problem of multiple substitutions, very similar sequences are chosen to determine PAM matrix:

- Consider substitutions in $71$ groups of protein sequences similar to at least $85 \%$
- Counted 1572 changes, or "accepted" mutations.

For each amino acid $(j)$, count all $N_{jk}$ changes (quantity of changes) in another amino acid $(k)$

Normalize by dividing by the total number of changes $(\sum_{m} A_{jm}, 1 \leq m \leq n)$

$n =$ number of amino acids $= 20$

$A_{jk} =\frac{N_{jk}}{\sum_m A_{jm}}$

- PAM matrix (P), of transition probability of each amino acid into another amino acid, is defined as the matrix that in each step allows preservation of $99\%$ of the sequence.

  - Calculated from substitution matrix $A$ as follow
    - $P_{jk} = c * A_{jk}, k \neq j$ and $P_{jj} = 1 - \sum_k P_{jk}, 1 \leq k \leq n, k \neq j$
    - $c$ chosen in order that the portion of expected changes by the model in a step is equal to $1\%$
    - $c$ is obtained by: $\sum_k \sum_{j \neq k}P_{jk} P_j = c \sum_k \sum_{j \neq k} A_{jk} P_j = 0.01$

PAM contains the log odd probability (p) of transition of each amino acid into another amino acid

$p = log (odd(P))$

$odd(P) = \frac{P}{1-P} \implies p = log (\frac{P}{1-P})$


If $PAM_{i,j} > 0$, likely transition of $i$ in $j$

If $PAM_{i,j} = 0$, random transition of $i$ in $j$

If $PAM_{i,j} < 0$, unlikely transition of $i$ in $j$

The classical PAM expresses the probability of change in one step $\implies PAM1$

If we want in two step: $PAM1 * PAM1 \implies (PAM1)^2 \implies PAM2$

In ten: $(PAM1)^10=PAM10$

This is the percentages of change, PAM2 $\implies 2\%$

The number identify the evolutionary step, the change of an aminoacid out of $100$ ones.

The $PAM250$ is the most used, the amino acid sequences maintain at this level $20\%$ of similarity.

Example

Take the $PAM250(F \to Y): 0.15$

Divide by frequency of changes into $F(0.04) = log_{10}(\frac{0.15}{0.04}) = 0.57$

like wise for $Y \to F : log_{10} (\frac{0.2}{0.03}) = 0.83$

Calculate the score for a change $F,Y$ as $10 * \frac{(0.83 + 0.57)}{2} = 7$

We will obtain something like this:

!["PAM250 log odds"](assets/chapter_four/pam250.gif)

##### BLOSUM

BLOSUM of substitution of amino acids

- PAM based on global alignments between sequences
- BLOSUM based on alignments of block of segments of amino acid sequences closely related

A block is a highly conserved region without gaps

How calculate the matrix ?

For each pair of amino acids $x$ and $y$, calculate the ratio of the like hood $(e_{xy})$ that $x$ and $y$ are aligned by chance

- Calculated based on frequency of occurrences of $x$ and $y$ in the block and portion of times that $x$ and $y$ are in the same column
  - $e_{xy} = \frac{P_x * P_y}{P_{xy}}, x=y$
  - $e_{xy} = 2 * \frac{P_x * P_y}{P_{xy}}, x \neq y$
  - $p_{xy}: probability of finding $x$ and $y$ paired in the same column = $\frac{n_{xy}}{\sum n_{xy}}, x \neq y$
- Then calculate $-2log_2(e_
{xy})$ and round to the nearest integer.

Example

![](assets/chapter_four/blosum_example.jpg)

Values calculated based on the substitutions in a set of $2000$ conserved patterns

To avoid that very similar sequences in a block polarize the estimation, clusters are created in the block.

- BLOSUM-n means that the sequences used in each cluster are similar to at least $n\%$

To find relationships between sequences close in time by evolutionary point of view, a large $n$ is used.

$BLOSUM62$ is the standard.

![](assets/chapter_four/blosum62.png)

##### BLOSUM vs PAM

- PAM based on explicit evolutionary model
- BLOSUM based on implicit evolutionary model
- PAM based on mutations observed in global alignment, which includes both highly conserved regions and highly mutated ones.
- BLOSUM based only on highly conserved regions in series of alignments without gaps
- Substitutions are counted differently: Unlike PAM matrices, BLOSUM procedure uses clusters of sequences whose mutations are counted not in the same way.
- High numbers in names of PAM matrices indicate high evolutionary distance, while high numbers un name of BLOSUM matrices show high sequence similarity.
- Gaps $"-"$ are used to align sequences with different length.
- Now in the reality we distinguish between gap opening $(g_o)$ and gap extension $(g_e)$, the gap extention is the continuous of the first gap.

So the gap penalty is $g = g_o + g_e * (l-1)$

where $g_o$ is the first gap penalty, high

$g_e$ is the gap extension, lower penalty

$l$ is the length of the gap block


In the scholar exercise we use a linear gap penalty for simplify.

But in the real software we use the real gap penalty.

This because in biology is likely that a mutation event makes a long gap than a lot of scattered gaps.

#### 4.1.4 Needleman - Wunsh

This method is an algorithm, the optimal one.

This algorithm is also complete.

**Optimal algorithm:** find the best solution, if it find one.

**Complete algorithm:** If exist, the algorithm find always a solution.

How works ?

First penalty and rewards:

- gap
- mismatch
- match

BUild the matrix like the dot matrix but an additional row and column.

Now starting the matrix with a $0$ in the $(0,0)$ cell, now we write the score, there are $3$ movement:

- horizontal: gap in the vertical sequence
- vertical: gap in the horizontal sequence
- diagonal: match/mismatch

We have to complete the matrix continue the score with the movement that maximize this one.

We start filled the row $0$ and the column $0$ with "gap movement"

After fulfill the matrix we have the best score in the last cell $(4,4)$, in this case, now we backtrack the movement and obtain the solution(s).

![](assets/chapter_four/needl.jpg)

- match $+2$
- mismatch $-1$
- gap $-2$


Solution:

$ATGC \\ ATCC$

Score: $5$

In general we use PAM or BLOSUM matrix for the score.

We can obtain more of one solution, in this case we write all of them.

This algorithm is used for global alignment, so for find the best alignment in the whole sequences.

#### 4.1.5 Smith - Waterman algorithm

This algorithm is for local alignment so for find the best sub-sequences.

It's completely equal to the Needleman-Wunsh algorithm with one crucial difference there aren't negative score. So the same matrix above become:

![](assets/chapter_four/smith.jpg)

So in this case the best sub-sequence is

$ATGC \\ ATCC$

Score: $5$

It's possible there are multiple sub-sequence with the highest score, we have to write all of them, REMEMBER if a score going to zero this is a reset point, so next score is for another sub-sequence, either if a score decrease but don't go to zero is the same sub-sequence, as in the example.


For ANY pairwise alignment, the used measure are:

- $z:$ measure of how the found correspondence is different from the random one, high $z$
- $p:$ probability that the alignment found is not better that the random one, low $p$.
- $E:$ number of sequences in a database of random sequences with equal length of the query sequence that have the score of the alignment with the query sequence grater than or equal to that of the found sequence.

For the score $z$ a score $\geq 5$ suggests significance of the alignment found between the two sequences.

Probability $p$ is obtained by: $p = 1 - e^{-kmne^{-\lambda S}}$

- $m:$ length of query sequence.
- $n:$ length of sequence found in the queried database.
- $S:$ score obtained.
- $k$ and $\lambda :$ parameters dependent on the substitution matrix used and the queried database
  - $10^{-10} \leq p < 10^{-10}$ closely related, homology is certain or almost.
  - $p \geq 10^{-10}$ probability not significant correspondence

Guide for the $E$ score:

- $10^{-100} \leq E < 10^{-10}$ sequences usually homologous
- $E < 10^{-5}$ good alignment
- $1 \leq E \leq 10$ often related sequences

### 4.2 Database Research

The classic programs that search for sequences in databases are FASTA and BLAST

The heuristic principle that these programs use is the search for "words" in databases.

**word:** short series of characters in the sequences of amino acids or nucleic acids.

These words are indicated with the term $k-$tuple -> k $= n°$ of characters.

- **Sensitivity:** is the ability to identify sequences related, although evolutionary distant.
- **Specificity:** is the ability to avoid false positives.

#### 4.2.1 FASTA

FASTA (FAST - All) it's an heuristic program that can search for global homology of sequences.

Exist two variants that can search for local homology:

- LFASTA
- PLFASTA

FASTA is specific but not quite sensitive.

$4$ phases:

- Phase $1a: k-$tuple

Initially, create a positional table containing all the positions for each amino acid (or nucleotide) in the query sequence and in each sequence in the database

![](assets/chapter_four/FASTA_pahse_1a.jpg)

I can built considering the position individually $1-$tuple or in pair $2-$tuple. For the nucleotide $4-$tuple or  $6-$tuple.

- Phase $1b:$ offset calculation

Calculate the difference of positional values of each amino acid between the query and the database.

![](assets/chapter_four/FASTA_phase_1b.jpg)

- Phase $2:$ Evaluation of substitutions between nucleotides

The best $10$ regions (best $10$ subsequence) selected are evaluated through the score matrices, the sub regions that contain the bases that maximize the regions score are identified.

The aim is finding the initial region with the best score, to be used to create a rank of the sequences in the database, in order to define which of them are the most similar to the query sequence.

- Phase $3:$ Joining of the initial regions

FASTA evaluates if it's possible to join together different regions of similarity.

Constraints to create the join:

- Excluding any areas of overlap between regions;
- Score above a threshold;
- Introduction of a scoring penalty for each gap introduced to join $2$ regions.

- Phase $4:$ optimization alignment

Sequences with higher similarity are aligned to the query sequence using the procedure based on a modified Smith-Waterman algorithm $\implies$ optimized score (OPT)

#### 4.2.2 BLAST

BLAST (Basic Local Alignment Search Tool)

It searches for best local alignment between a query sequence and the sequences in a database

- Developed and supported by NCBI (US National Center for Biotechnology Information)

Features:

- Local alignments
- Alignment with gaps
- Heuristic
- Rapid

While FASTA searches all possible words of the same length, BLAST limits the search to the most significant words using a preventive filter.

For score, in case of protein it uses BLOSUM62

BLAST fixes the length of the word to:

- $3$ for proteins
- $11$ for nucleotides

$3$ Phases:

- Phase $1:$ Generation of words

It generated a list of words of length $W$ from the query sequence.

![](assets/chapter_four/BLAST_phase_one.jpg)

For each words, we assigned to each $20^3$ words found in the database.

Use a threshold $T$ to limit the number of analogous words.

- Phase $2:$ Find the words in the database

The search (exact) of the best analogous words in the sequences of the sequences of the database is performed.

- Phase $3:$ Hit's extension

When searched analogous words are found in database's sequences, they identify regions of possible local alignment (without gap) between the query sequence and the sequences found in the database

The algorithm tries to extend aligned regions, without allowing gaps, and until extended alignment score does not decrease $\implies$ High - Scoring Segmented Pairs (HPS)

HPS is considered relevant if exceeds a threshold value $S.$

Important: At the end, it generated the best alignment according to Smith-Waterman algorithm.

Variation of $E$ and $p$

- $E$ is equal to the number of sequences that we would expect to find if the database contained random sequences.

$p-value$

- $p = 1 - e^{-E}$

Filters

- BLAST has filters to skip regions with repetitions or low complexity
  - $SEG:$ filters low protein complexity regions
  - $DUST:$ filters low DNA complexity regions
  - $XNU:$ filters regions containing protein tandem repetitions.

##### BLAST vs FASTA

- BLAST
  - Standard tool, used in practice
  - MOre sensitive in protein research 
  - Local alignment
  - Fast 
  - First analysis
- FASTA
  - More sensitive, in particular for nucleotide sequences.
  - Global alignment
  - Slow
  - Secondary analysis

Both heuristic

They don't grant to find the best alignment

Variant of BLAST

- BLASTP: searches a protein sequence in a database of proteins.
- BLASTN:searches a nucleotide sequence in a database of nucleotide sequences
- BLASTX: searches a nucleotide sequence translated in all six possible reading frame in a database of proteins.
- TBLASTN: searches a protein sequence in a database of nucleotide sequences that are automatically translated into all six possible reading frames.
- TBLASTX: searches translations in all six possible reading frame of sequence of nucleotides in a database of nucleotide sequences dynamically translated.
- MegaBLAST: it's an optimized program to align nucleotide sequences that differ slightly and therefore the could originate from sequencing errors
- PSI-BLAST(Position Specific Iterated BLAST): designed for analysis of protein sequences, it increases the sensitivity of the algorithm using iterative procedure.
- BEAUTY (BLAST - Enhanced Alignment UTilitY): it adds information to the output of BLAST with text and graphics.

### 4.3 Information

Motifs are regular combinations of protein secondary structures associated with particular functions.

So same motif $\implies$ similar function

Search for protein motifs can identify new genes and study the diffusion of specific motifs in different genomes.

- $S.M.A.R.T. :$ Simple Modular Architecture Research Tool

Uses for search protein motifs

### 4.4 Multiple Alignment

Why ?

The alignment in pair allows:

- Searching for sequences similar to a given sequence
- Evaluating the homology of two sequences

The multiple alignment allows:

- Molecular phylogeny, through the comparison among sequences, to construct phylogenetic trees that illustrate distances and evolutionary relationships among analyzed molecules.
- The study of the evolution of genomes
- Identification of recurring motifs and functionally important sites, which help to characterize genes and proteins with unknown function.
- Characterization of sequences with unknown function, through the identification of recurrent motifs and functionally important sites.

Formal definition:

- Given an alphabet $\Sigma$ and some sequences $S_1, ..., S_k : S_i \in \Sigma  \ \text{for} \ 1 \leq i \leq k$

A multiple alignment associates with $S_1, ..., S_k$ the sequences $S_1', ..., S_k' : S_i' \in (\Sigma \cup \{-\})$ for $1 \leq i \leq k$ so that:

- $|S_1'| = |S_2'| = ... = |S_k'| = /$
- removing gaps from $S_1', ..., S_k'$ we obtain $S_1, ..., S_k$

#### Profiles

Profiles are useful structures for summarizing the common proprieties of groups of sequences and they are the basis of many methods of multiple sequence alignment 

- Given a multiple alignment of $M$ sequences of length $l:$
  - The profile of $M$ is a matrix $l \times |\Sigma \cup \{-\}|$, where $\Sigma$ is the alphabet of the sequences of $M$, whose single column:
    - Represents  a position in the alignment.
    - Contains the frequencies of tach character that appears in that position or, the weighted average of the scores derived from a score matrix
  - The profile matrix is also called matrix of weights or Position Specific Scoring Matrix (PSSM)

Example:

![](assets/chapter_four/Profiles.jpg)

**Shannon Entropy**

GIven a probability space $(s,p)$, the entropy $H$ is a measure of dispersion of the probability function of the objects in the space $S$

- $H$ is a measure of uncertainty about the identity of the objects in a set 

$$
\begin{align*}
  H = - \sum_{i=1}^m p_i log_2 p_i
\end{align*}
$$

**Information content**

Given a matrix of weights that models sequence alignment, you can determinate the information content $I(k)$ for each alignment position $k$:

$$
\begin{align*}
  I(k) = log_2(m) - (H(k) + e(n))
\end{align*}
$$

- $m:$ number of distinct characters in the alphabet of the aligned sequences ($m=4$ in DNA/RNA, $m=20$ for amino acids)
- $e(n) =$ correction factor, it depend on the number $n$ of sequences in the alignment modeled by the weight matrix.

The alignment logo shows the information content of each position of the multiple alignment.

![](assets/chapter_four/logo.png)

**Usefully of extraction profile**

- Identify new members of the profiles in databases
- Identify consensus sequences (e.g. for Transcription Factor Binding Sites)
- Define weight matrices proportional to the degree of conservation to be used in alignment.
- Profiles can be used to predict the membership of new sequences to the group of sequences with a given profile.

Databases of profiles/patters:

- PROSITE, Pfam, PRINTS, TransFac, Jaspar

To align a sequence to a profile, we use Needleman - Wunsh but with a different scoring function.

$$
\begin{align*}
  \sigma_{sp} (b,i) = \sum_{a \in \Sigma} P_{i,a} \sigma (a,b)
\end{align*}
$$

To align two profiles -> $\sigma_{pp} (i,j) = \sum_{k=1}^{|\Sigma| + 1} f(P_{i,k}', P_{j,k}'')$

- $f$ is a function that assigns a score to column pairs by taking into account the frequency of individual characters of the alphabet $\Sigma$ of the profiles.

So different multiple alignment $\implies$ we need a score standard to be able to compare them:

- Starting point: BLOSUM, PAM, etc... and gap penalty.

The most used function is the **Sum - of - Pairs** score, sum of the scores, sum of the scores of the pairwise alignments induced by the multiple alignment:

$$
\begin{align*}
  \sigma (s) = \sum_{i=1}^{i<z} \sum_{j=i+1}^z S(s_i, s_j)
\end{align*}
$$

$S(s_i,s_j)$ is the score of alignment of pairs of sequences $s_i$ and $s_j$ induced by multiple alignment $M$.

$z$ is the number of sequences in the multiple alignment.

Example


$$
S1: ACTCT
\\
S2: A-TTT
\\
S3: A-TTT
\\
\sigma(s) = S(s_1,s_2) + S(s_1,s_3) + S(s_2, s_3) = 3 + 3 + 6 = 12
$$

Other function of scoring:

- Entropy
- Circular-Sum

**Entropy**

$H(A) = \sum_{c \in A} H(c)$

$H(c) = - (\sum_{x \in \Sigma} p_x log_2 p_x)$

$c:$ column of the alignment $A$

$p_x:$ frequency of the symbol $x$ in column $c$.

Example

$$
ACT
\\
ACA
\\
A-T
\\
H(1) = - (\frac{3}{3} log_2 \frac{3}{3} + 0 + 0 + 0 + 0) = 0
\\
H(2) = - (0 + \frac{2}{3} log_2 \frac{2}{3} +0 + 0 + \frac{1}{3} log_2 \frac{1}{3}) = 0.92
\\
H(3) = - (\frac{1}{3} log_2 \frac{1}{3} + 0 + 0 + \frac{2}{3} log_2 \frac{2}{3} + 0) = 0.92
\\
H(A) = 0 + 0.92 + 0.92 = 1.84
$$

**Circular Sum**

$CS(A) = \frac{1}{2} \sum_{i=1}^z MPA(a_i, a_{i+1})$

We do the pairwise score sum immediately, so:

match: +1

mismatch/gap: -1

$$
ACA
\\
ACC
\\
AT-
\\
MPA(a_1,a_2) = 1 + 1 - 1 = 1
\\
MPA(a_2,a_3) = 1 - 1 - 1 = -1
\\
MPA(a_3,a_1) = 1 - 1 - 1 = -1
\\
CS(A) = \frac{1}{2}(1 - 1 - 1) = -1 
$$

**Sum-of-Pair** vs **Circular Sum**

Sum-of-pair is clearly  inefficient from an evolutionary point of view

#### 4.4.1 Dynamic programming

Now if we have $2$ sequences we need a $2d-$matrix, $3$ sequences $3d-$matrix, $n$ sequences $nd-$matrix. This approach is very complex, it's called NP-complete(Non Polynomial) $\implies$ very difficult and a lot of time.

Example $10$ sequences each length $= 100$ we have $100^{10} = 10^{20}$ elements $\implies 100$ mil. terabytes

Solution $\implies$ Heuristic and approximations.

Heuristic methods:

- Progressive alignment
- Center-star alignment
- Iterative alignment

#### 4.4.2 Progressive alignment

Simple and the most common

Idea: we align 2 sequence, then we align other 2 and we continue then we align the  2 aligned sequences with other 2 or with an unaligned one.

Like MERGE-SORT

![](assets/chapter_four/progressive.jpg)

Heuristic: similarity degree

So we aligned the similar ones, until we remain without sequence unaligned.

**Feng - Doolittle**

Algorithm that implements progressive alignment heuristics:

- Calculate $\binom{n}{2}$ pairwise alignment ($n=$ number of sequences) and convert score in distance.
- Build a phylogenetic tree.
- Align in order of the tree.

#### 4.4.3 Star-Center

Given a set $S$ of $z$ sequences, we define **central sequence** $S_C \in S$ the sequence that minimizes the function:

$$
\sum_{S_j \in S} D(S_C, S_j)
$$

or, the sum of the distances of all the sequences from $S_C$ will be the minimum possible.

Then we use Sum-of-Pairs

#### 4.4.4 Iterative Alignment

It starts aligning the newest couple of sequences according to a certain definition of distance (not the same pf progressive).

Then, at each step it takes the sequence with the minimum distance from all sequences already aligned and it aligns it to the alignment profile already created.

In case, create new space $"-"$

### 4.5 Multiple alignment tool

ClustalW is the most popular tool for the multiple alignment.

- Given a set $S$ of $z$ sequences to be aligned, ClustalW aligns all the pairs of sequences of $S$ separately and builds a similarity matrix with the distances between each couple.

Then, it builds a phylogenetic tree, it consider the first couple how a singular sequence and builds another similarity matrix and go until finish the tree.

![](assets/chapter_four/ClustalW.jpg)

It is obtained a tree with branches of length proportional to the distance between the sequences $\implies$ **dendrogram**

##### Details of ClustalW's output

At the bottom of each column:

- symbol $"*": 100 \%$ match.
- $":" = > 75\%$ high similarity.
- $".": (50\% - 75 \%)$ average similarity
- No symbol$: < 50\%$ low similarity

## Chapter Five: Measurement of Genetic Expression

![](assets/chapter_five/GE.jpg)

**Genetic Expression:** Conversion of coded information in a gene, for coding genes, first in messenger RNA and the in protein.

Not every gene is always necessary for the cell life

- Only constitutive genes are always expressed, other ones only when they are necessary.

Gene expression is regulated by the cell necessity: environment conditions and functions necessary to be performed.

- In multi-cellular organisms:
  - The environment of a cell is the organism itself
  - Starting from the same cell, the "differential gene regulation" mechanism causes the creation of different specialized cells.

The genetic expression, is different depending on the cell type and the answer from the environment.

The transcriptome is the complete set of gene transcripts and of their levels of expression, in a particular type of cells or tissue, in well defined conditions.

- Only $20\%$ of the transcriptome is expressed by a cell

To understand biological organisms it is necessary to study:

- gene expression and regulation;
- synthesized protein functionality;
- quantitative occurrences of metabolites;
- effects of gene defects on organism phenotype.

System biology: study of interactions between components of a biological system and how such interactions induce functions and behaviour of the system.

For functional analysis of genomes:

- Transcriptomics
- Proteomics
- Metabolomics

High - throughput procedures.

These approaches of genotyping must be correlated with phenotypic analysis of model organisms and cells in vitro.

### 5.1 Gene expression analysis techniques

How to measure the gene expression ?

Methods to measure the expression level $\frac{gene(s)}{time}:$ RT-PCR (Reverse Transcriptase Polymerase Chain Reaction)

Main analysis techniques of the whole transcriptome:

- cDNA microarray
- Oligonucleotide microarray
- SAGE (Serial Analysis of Gene Expression)

1980: RNA analysis of one or few genes at a time

- Northern blotting
- quantitative PCR

1995: RNA analysis whole genome

- Molecular biology technologies

Two main technologies of DNA microarrays:

- cDNA spotted arrays
- Oligonucleotide arrays

#### 5.1.1 Northern Blot

Laboratory technique to study genetic expression, by finding the RNA (or isolated mRNA) in a sample

- Gene expression quantifying the level of abundance of a transcript in a single sample.
- Gene regulation: behaviour of the transcript in comparison test-control.

In $4$ step:

1. RNA Extraction: we extract the RNA.
2. Preparation of the probe: fragment of the gene that we have to analyze.
3. Hybridization: We wait until the probe and the gene create a bond, if the gene is expressed.
4. The probe is a marker (radioactive or fluorescent) and its insensitive is proportional to the quantity of expressed gene.

#### 5.1.2 RT-procedure

The polymerase chain reaction (PCR) is a laboratory technique exploiting DNA replication to amplify a single or        few couple of specific sequence of DNA, up to $\cong 10kb$ long, also $40kb$.

PCR is based on thermal cycles of heating and cooling of a solution where the replication reaction of DNA occurs, we use high temperature for divide the helix of DNA, and low temperature for replication of DNA.

The reverse transcription polymerase chain reaction (RT-PCR) is a variation of the PCR, in which a RNA helix, firstly is reverse-transcribed in its complementary DNA (cDNA), by using the enzyme reverse transcriptase, and the resulting cDNA is amplified by using traditional PCR, or real-time PCR, made in a thermal cycler for automatic time and temperature control.

Another way to replicate pieces of DNA uses plasmids if bacterial as vectors to clone DNA sequences, the DNA fragment are inserted in the DNA sequence of the plasmid and the DNA ligase enzyme to bind to the plasmid DNA fragment to be cloned -> recombinant plasmid.

### 5.2 DNA microarrays

Microarrays: orderly and miniaturized arrangements of fragments of DNA with know sequences on solid support.

The microarrays is the evolution of Northern blot, microarrays can analyze the entire genome while the Northern blot only one or few genes.

Application:

- Measurement of genetic expression;
- Measurement of abundance of genetic transcript.
- Characterization of a gene sequence.
- Characterization of alteration of the number of copies of a given gene or DNA sequence.
- Characterization of DNA-proteins interactions.

Since they allow to determine the profile of expression of the expression of the cell in a given state, it's also said that microarrays allow **expression profiling**

#### 5.2.1 cDNA microarrays

$4$ steps:

1. BUilding of the cDNA microarrays: full section of ESTs (Expressed Sequence Tags, short sub-sequences of a transcribed cDNA sequence).
2. Sample preparation: two mRNA samples are prepared, retro-transcribed into cDNA and made fluorescent with different colors (Cys3, green, uses for the control; Cys5, red, uses for the test).
3. Hybridization: gene transcripts expressed in sample, prepared and marked are hybridized with their complementary sequence on the microarray
4. Measure of the gene expression: the fluorescent measure in every spot gives a measure of which genes are expressed in each of the two samples.

Images of cDNA microarray:

- Red: expressed gene in tests.
- Green: expressed gene in controls.
- Yellow:expressed gene in both samples.
- Gray: no expressed gene in any samples.

From images to data: A laser take the insensitive of each spot and transform it in a data.

Pros and Cons of "spotted" technology:

- Competitive hybridization: analysis of mRNA from cells in two conditions
  - Pro: relative measures.
  - Cons: The definition of the reference, colometric problems, possible difference in the amount of the two mRNA
- Difficult to compare results from different arrays: the intensity depends on the amount of probes deposited
- It takes a lot of mRNA to prepare the target $(50 - 200 \mu g)$

#### 5.2.2 Oligonucleotide microarrays