import cyvcf2 
import tsinfer
import tsdate 
import json 
import tskit 
import pandas as pd 
import sys 
import numpy as np
from csv import writer
import gzip

def add_diploid_sites(vcf, samples):
    """
    Read the sites in the vcf and add them to the samples object, reordering the
    alleles to put the ancestral allele first, if it is available.
    """
    pos = 0
    for variant in vcf:  # Loop over variants, each assumed at a unique site
        if pos == variant.POS:
            raise ValueError("Duplicate positions for variant at position", pos)
        else:
            pos = variant.POS
        if any([not phased for _, _, phased in variant.genotypes]):
            raise ValueError("Unphased genotypes for variant at position", pos)
        alleles = [variant.REF] + variant.ALT
        ancestral = variant.INFO.get("AA", variant.REF)
        # Ancestral state must be first in the allele list.
        ordered_alleles = [ancestral] + list(set(alleles) - {ancestral})
        allele_index = {
            old_index: ordered_alleles.index(allele)
            for old_index, allele in enumerate(alleles)
        }
        # Map original allele indexes to their indexes in the new alleles list.
        genotypes = [
            allele_index[old_index]
            for row in variant.genotypes
            for old_index in row[0:2]
        ]
        samples.add_site(pos, genotypes=genotypes, alleles=alleles)

def chromosome_length(vcf):
    assert len(vcf.seqlens) == 1
    return vcf.seqlens[0]


def add_populations(vcf, samples):
    """
    Add tsinfer Population objects and returns a list of IDs corresponding to the VCF samples.
    """
    # In this VCF, the first letter of the sample name refers to the population
    samples_first_letter = [sample_name[5:7] for sample_name in vcf.samples]
    pop_lookup = {}
    pop_lookup["Fa"] = samples.add_population(metadata={"population": "Algarve"})
    pop_lookup["Ga"] = samples.add_population(metadata={"population": "Bay of Biscay"})
    pop_lookup["Li"] = samples.add_population(metadata={"population": "Gulf of Lion"})
    pop_lookup["Mu"] = samples.add_population(metadata={"population": "Costa Calida"})
    return [pop_lookup[first_letter] for first_letter in samples_first_letter]


def add_diploid_individuals(vcf, samples, populations):
    for name, population in zip(vcf.samples, populations):
        samples.add_individual(ploidy=2, metadata={"name": name}, population=population)


######## IMPORT VCF

arg=sys.argv
#arg=['infer_tree.py','/shared/projects/abc_fish/tsinfer/Dlabr_phased_LG7_AA_remove_fake_AA.vcf','1e-9','1e-2']
print(arg)

if arg[3]=="Hgutt":
  vcf = cyvcf2.VCF(arg[1],samples=['HguttFa2','HguttFa3','HguttFa4','HguttFa5','HguttFa6','HguttGa9','HguttGa6','HguttGa3','HguttGa7','HguttGa11','HguttLi1','HguttLi3','HguttLi4','HguttLi5','HguttLi6','HguttMu1','HguttMu2','HguttMu3','HguttMu4','HguttMu5'])
elif arg[3]=="Lbude":
  vcf = cyvcf2.VCF(arg[1],samples=['LbudeFa1','LbudeFa2','LbudeFa4','LbudeFa5','LbudeFa6','LbudeGa1','LbudeGa2','LbudeGa3','LbudeGa4','LbudeGa6','LbudeLi1','LbudeLi2','LbudeLi3','LbudeLi5','LbudeLi6','LbudeMu1','LbudeMu3','LbudeMu4'])
else:
  vcf = cyvcf2.VCF(arg[1])  

with tsinfer.SampleData(
  path=arg[2], sequence_length=chromosome_length(vcf)
) as samples:
	populations = add_populations(vcf, samples)
	add_diploid_individuals(vcf, samples, populations)
	add_diploid_sites(vcf, samples)

print(
  "Sample file created for {} samples ".format(samples.num_samples)
  + "({} individuals) ".format(samples.num_individuals)
  + "with {} variable sites.".format(samples.num_sites),
  flush=True,
)
