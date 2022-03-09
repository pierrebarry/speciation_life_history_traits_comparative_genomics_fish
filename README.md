# :fish: Comparative population genomics analysis of the Atlantic/Mediterranean suture zone to assess the life-history determinants of speciation in marine fishes :dna:

Scripts and files to generate results and output of the Chapter 2 from the PhD of Pierre Barry.

## Sampling information

:file_folder: Files:
- ``: 

:bar_chart: Scripts:
- `` : 
 ```ruby

```

## :computer: Pre-processing fasta files

:file_folder: Files:
- ``: 

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

## :computer: Variant Calling

:file_folder: Files:
- ``: 

:bar_chart: Scripts:
- `` : 
 ```ruby

```


## :computer: Semi-permeability

:file_folder: Files:
- `*_fst_f3_wgs_50kb.csv`: species FST and f3 statistics in 50kb windows
- `Delta_Frac_16_species.txt` : (Fst[in]-Fst[out])/Fst[out] estimated from 50kb windows for all species

:bar_chart: Scripts:
- `` : 
 ```ruby

```

## :computer: BUSCO analyses

:file_folder: Files:
- `*_BUSCO_stats.csv`: species polymorphism statistic, Fst and f3 values of each BUSCO genes. 

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
