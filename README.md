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
- `reference_genome.R ` : display the distribution of scaffold length of species reference genome

```ruby

```

## :computer: Mapping

:file_folder: Files:
- `samtools_report.Rdata `: Reference genome mapping statistic of all species.

:bar_chart: Scripts:
- `mapping.R ` : plot mapping statistics

 ```ruby

```

## :computer: Variant Calling

:file_folder: Files:
- `VCF_SPECIES.Rdata`: Number of SNPs, Ts/Tv and mutation spectrum   (to open in R, load(file="VCF_SPECIES.Rdata")
- `VCF_STATS_INDIV.Rdata`: VCF stats per individual (number of SNPS, depth, percentage of missing genotypes, phased genotypes, SNPs same as reference) (to open in R, load(file="VCF_STATS_INDIV.Rdata")
- `VCF_DEPTH_QUALITY_PER_SITE.Rdata`: Variant depth and quality for all species (to open in R, load(file="VCF_DEPTH_QUALITY_PER_SITE.Rdata")

:bar_chart: Scripts:
- `vcf_stat.R` : plot the statistics of the species VCF.
 
 ```ruby

```

## :computer: General popgen stats

:file_folder: Files:
- `FST.xlsx`: Whole-genome FST, weighted and unweighted per species, estimated with vcftools. 
- `dxy_all.Rdata` : Whole-genome Absolute (dxy), net divergence (da), diversity per species estimated from VCFs with variant and non-variant sites.
- `fst_continuum.R` : plot the continuum of Fst with corresponding species PCA.

:bar_chart: Scripts:
- `dxy_analyze.R ` : estimate whole-genome dxy, da from 10kb windows per species.
- `dxy_gradient_correlation_fst_dxy_da.R` : gradient of species dxy, and correlation between whole-genome Fst, dxy and da.
 
 ```ruby

```

## :computer: PCA

:file_folder: Files:
- `PCA_list.Rdata`: Principal Component Analysis estimations per species with various maf (from 0 to 0.5) and on the whole or pruned by linkage desiquilibrium data set (to open in R, load(file="PCA_list.Rdata"): the corresponding object is a list, each element of the list corresponding to a species, and each sublist, PCA invidiual coordinates from maf and variant dataset).

:bar_chart: Scripts:
- `RUN_PCA.R` : run PCA analysis per species with various maf parameters (0 to 0.5, with the whole variants dataset or pruned by linkage disequilibrium dataset)
- `PCA.R` : plot PCA 

 ```ruby

```

## :computer: Introgression

:file_folder: Files:
- ``

:bar_chart: Scripts:
- `f3.R ` : plot f3 result for all 12 3-populations topologies for all species
- `f3_all.R` : the same but plot all results in one plot

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
- `busco.R ` : plot results of BUSCO analysis of all species
- `diff_gene_wgs.R `: compute the difference of pi, fst, dxy and f3 in non-BUSCO 50 kb windows and BUSCO stats.

 ```ruby

```

## :computer: ABC analyses

:file_folder: Files:
- `data_ABC.csv`: Parameters of ABC inference per species and per population comparisons (inner: Mu-Fa/outer : Li-Ga)

:bar_chart: Scripts:
- `` : 
 ```ruby

```

## :computer: Infer tree sequences with tsinfer and tsdate

:file_folder: Files:
- ``

:bar_chart: Scripts:
- `create_ancestral_file.py` : creat a file with chrom, position and ancestral state.
- `check_AA.R` : check if ancestral state in the VCFs corresponds to either the reference or alternative allele 
- `local_gnn.py` : estimate Genetic Nearest Neighours retrieved and adapted from [Kelleher et al. 2019](https://www.nature.com/articles/s41588-019-0483-y)
- `job` : deploy snakefile on IFB clusters

```ruby
```

## :computer: Correlation with life-history traits

:file_folder: Files:
- ``

:bar_chart: Scripts:
- `correlation_lfh_fst_dxy_da_tsplit.R` : correlation between species Life history traits and, differentiation (fst), absolute (dxy), net (da) divergence and time of ancestral split (Tsplit) inferred by ABC.
- `phylo_signal.R` : test the phylogenetic signal of life-history traits and genetic characteristics
- `plot_life_history_traits.R` : plot the 10 life-history traits

 ```ruby
```



## :chart_with_upwards_trend: Figures

:file_folder: Files:
- ``: 

:bar_chart: Scripts:
- `Fig1.R` : script for generating Fig1 from the PhD manuscript
- `Fig2.R` : script for generating Fig2 from the PhD manuscript
- `Fig3.R` : script for generating Fig3 from the PhD manuscript
- `Fig4.R` : script for generating Fig4 from the PhD manuscript
- `Fig5.R` : script for generating Fig5 from the PhD manuscript
- `Fig6.R` : script for generating Fig6 from the PhD manuscript
- `Fig7.R` : script for generating Fig7 from the PhD manuscript

 ```ruby

```

## Miscellaneous

:file_folder: Files:
- `data/*.png`: images of each studied species 

:bar_chart: Scripts:
- `plot_map_occurence.R`: display maps of occurence of species bashed on FishBase data.

## :wrench: Tools needed

* [fastp v.0.20.0](https://github.com/OpenGene/fastp)
* [bwa v.0.7.17](http://bio-bwa.sourceforge.net/bwa.shtml)
* [picard v.2.23.2](https://broadinstitute.github.io/picard/)
* [GATK v.4.1.6.0](https://gatk.broadinstitute.org/hc/en-us/articles/360036194592-Getting-started-with-GATK4)
* [vcftools v.0.1.17](https://vcftools.github.io/index.html)
* [snakemake v.3.5.0](https://github.com/snakemake/snakemake)
* [R v.3.6.1](https://cran.r-project.org/bin/windows/base/old/3.6.1/)
