# :fish: Comparative population genomics analysis of the Atlantic/Mediterranean suture zone to assess the life-history determinants of speciation in marine fishes :dna:

Scripts and files to generate results and output of the Chapter 2 from the PhD of Pierre Barry.

## Sampling information

:file_folder: Files:
- `sampling.Rdata `: Location, date, precise location, longitude, latitude, method of capture and tissues of samples of all sequenced individuals.

:bar_chart: Scripts:
- `` : 
 ```ruby

```

## :computer: Pre-processing fasta files

:file_folder: Files:
- `Summary_fastp.txt `: various statistics of reads quality, number and GC content before and after fastp processing files

:bar_chart: Scripts:
- `` : 
 ```ruby

```

## :computer: Reference genome alignment

:file_folder: Files:
- ``: 

:bar_chart: Scripts:
- `` : 
 ```ruby

```

## :computer: Mapping

:file_folder: Files:
- `samtools_report.Rdata `: Reference genome mapping statistic of all species.

:bar_chart: Scripts:
- `` : 
 ```ruby

```

## :computer: Variant Calling

:file_folder: Files:
- `VCF_SPECIES.Rdata`: Number of SNPs, Ts/Tv and mutation spectrum   (to open in R, load(file="VCF_SPECIES.Rdata")
- `VCF_STATS_INDIV.Rdata`: VCF stats per individual (number of SNPS, depth, percentage of missing genotypes, phased genotypes, SNPs same as reference) (to open in R, load(file="VCF_STATS_INDIV.Rdata")
- `VCF_DEPTH_QUALITY_PER_SITE.Rdata`: Variant depth and quality for all species (to open in R, load(file="VCF_DEPTH_QUALITY_PER_SITE.Rdata")

:bar_chart: Scripts:
- `` : 
 ```ruby

```

## :computer: General popgen stats

:file_folder: Files:
- `FST.xlsx`: Whole-genome FST, weighted and unweighted per species, estimated with vcftools. 
- `dxy_all.Rdata` : Whole-genome Absolute (dxy), net divergence (da), diversity per species estimated from VCFs with variant and non-variant sites.

:bar_chart: Scripts:
- `` : 
 ```ruby

```

## :computer: PCA

:file_folder: Files:
- `PCA_list.Rdata`: Principal Component Analysis estimations per species with various maf (from 0 to 0.5) and on the whole or pruned by linkage desiquilibrium data set (to open in R, load(file="PCA_list.Rdata"): the corresponding object is a list, each element of the list corresponding to a species, and each sublist, PCA invidiual coordinates from maf and variant dataset).

:bar_chart: Scripts:
- `` : 
 ```ruby

```

## :computer: Semi-permeability

:file_folder: Files:
- `*_fst_f3_wgs_50kb.csv`: species FST and f3 statistics in 50kb windows
- `Delta_Frac_16_species.txt` : (Fst[in]-Fst[out])/Fst[out] estimated from 50kb windows for all species
- `data_fd.Rdata` : distribution of f[d] inferred with Dsuite in 50 SNPs window in 10 species with known ancestal data.


:bar_chart: Scripts:
- `` : 
 ```ruby

```

## :computer: BUSCO analyses

:file_folder: Files:
- `*_BUSCO_stats.csv`: species polymorphism statistic, Fst and f3 values of each BUSCO genes. 
- `all_gene_data.Rdata`: genetic diversity (pi), divergence (dxy), differentiation (fst), admixture (f3) difference betwee non-BUSCO and BUSCO stats (DIFF) and correspondings t-test per species.
- `dxy_gene_*.txt` : Absolute divergence between pair of populations for BUSCO genes for the corresponding species.
- `pi_gene_Hgutt.txt` : Genetic diversity for BUSCO genes for the corresponding species.


:bar_chart: Scripts:
- `` : 
 ```ruby

```

## :computer: ABC analyses

:file_folder: Files:
- `data_ABC.csv`: Parameters of ABC inference per species and per population comparisons (inner: Mu-Fa/outer : Li-Ga)

:bar_chart: Scripts:
- `` : 
 ```ruby

```

## :chart_with_upwards_trend: Figures

:file_folder: Files:
- ``: 

:bar_chart: Scripts:
- `` : 
 ```ruby

```

## Miscellaneous

:file_folder: Files:
- `data/*.png`: images of each studied species 


## :wrench: Tools needed

* [fastp v.0.20.0](https://github.com/OpenGene/fastp)
* [bwa v.0.7.17](http://bio-bwa.sourceforge.net/bwa.shtml)
* [picard v.2.23.2](https://broadinstitute.github.io/picard/)
* [GATK v.4.1.6.0](https://gatk.broadinstitute.org/hc/en-us/articles/360036194592-Getting-started-with-GATK4)
* [vcftools v.0.1.17](https://vcftools.github.io/index.html)
* [snakemake v.3.5.0](https://github.com/snakemake/snakemake)
* [R v.3.6.1](https://cran.r-project.org/bin/windows/base/old/3.6.1/)
