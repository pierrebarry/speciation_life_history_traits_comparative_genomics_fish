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

:bar_chart: Scripts:
- `dxy_analyze.R ` : estimate whole-genome dxy, da from 10kb windows per species.
- `dxy_gradient_correlation_fst_dxy_da.R` : gradient of species dxy, and correlation between whole-genome Fst, dxy and da.
- `fst_continuum.R` : plot the continuum of Fst with corresponding species PCA.

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
- `tsinfer_create_input_files.py` : create input files for tsinfer
- `tsinfer_infer.py` : infer tree genealogies with tsinfer
- `tsdate_infer.py` : infer branch lengths with tsdate 
- `snakefile` : all steps to infer and date topologies from phased VCFs with unknown ancestral state.
- `job` : deploy snakefile on IFB clusters
- `plot_stats.R`: plot distribution of TMRCA

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
- `article_Fig2.pdf`: Fig2 from the manuscript 
- `article_Fig3.pdf`: Fig3 from the manuscript 
- `article_Fig3_supp.pdf`: TMCA distributions not scaled by species generation time
- `article_Fig4.pdf`: Fig4 from the manuscript 
- `article_Fig5.pdf`: Fig5 from the manuscript 
- `article_Fig6.pdf`: Fig6 from the manuscript 
- `article_Fig7.pdf`: Fig7 from the manuscript 
- `article_FigS2.pdf` : same as Fig2 of the manuscript but replacing Fst on the x-axis by net divergence (da)
- `article_Figsupp_distri_dxy_da.pdf` : gradient of species absolute divergence (dxy) and correlation between species Fst, dxy and da
- `article_fig_1map.png` : map of the Figure 1
- `f3_plot.pdf` : f3 result for all species all triplets topologies
- `fd.pdf` : correlation between fd and Fst
- `tsinfer_infer_interpop.png` : evaluation of performance of tsinfer to infer TMRCA between populations splitted by a secondary contact using various parameters
- `tsinfer_infer_intrapop.png` : evaluation of performance of tsinfer to infer TMRCA within populations using various parameters
- `tsinfer_tsdata_si_im.pdf` : simulation and evaluation of signature of a Strict isolation and Isolation with Migration models on TMRCA distributions 
- `tsinfer_tsdata_sc_mp.pdf` : simulation and evaluation of signature of a Secondary contact and Migration pulse models on TMRCA distributions 
- `tsinfer_tsdata_ai.pdf` : simulation and evaluation of signature of a Ancestral introgression model on TMRCA distributions 
- `ALL_PCA.pdf` : PCA of all species with maf = 0.05 and whole variant data set
- `All_mapped.pdf` : Percentage of all individual reads mapped on species reference genome
- `diff_gene.pdf` : difference between diversity, differentiation and divergence between species non-BUSCO and BUSCO genes.
- `lfh_fst_li_ga.pdf` : correlation between 9 life-history traits and Fst between outer populations 
- `lfh_dxy_li_ga.pdf` : correlation between 9 life-history traits and dxy between outer populations 
- `lfh_da_li_ga.pdf` : correlation between 9 life-history traits and da between outer populations 
- `D_fd_*.png` : result of test of differential introgression with the D statistic
- `*_occurence.pdf` : distribution of each species based on FishBase data

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
