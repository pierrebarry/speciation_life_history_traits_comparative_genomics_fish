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
#arg=['infer_tree.py','ts']
print(arg)

#ts=tskit.load("Dlabr_LG7_pop_1e-8_1e-2.trees")

ts=tskit.load(arg[1])
ts_simplify=ts.simplify(keep_unary=False)

prior=tsdate.build_prior_grid(ts_simplify, timepoints=int(arg[2]))
#dated_ts.dump("/shared/projects/abc_fish/tsinfer/Dlabr_LG7_dated_pop_prior200.trees")
 
#dated_ts = tsdate.date(ts_simplify, Ne=92500, mutation_rate=1e-8,priors=prior)
dated_ts = tsdate.date(ts_simplify, Ne=float(arg[3]), mutation_rate=float(arg[4]),priors=prior)
dated_ts.dump(arg[5])


breaks=list(dated_ts.breakpoints()) 
samples=list(dated_ts.samples()) 

corr=['Fa','Fa','Fa','Fa','Fa','Fa','Fa','Fa','Fa','Fa', 
      'Ga','Ga','Ga','Ga','Ga','Ga','Ga','Ga','Ga','Ga',
      'Li','Li','Li','Li','Li','Li','Li','Li','Li','Li',
      'Mu','Mu','Mu','Mu','Mu','Mu','Mu','Mu','Mu','Mu']


IND1=[] 
IND2=[] 
BLOCK_LENGTH=[] 
POSITION=[]
DIVERGENCE=[] 
POP1=[]
POP2=[]

for ind1 in range(0,samples[-1]):
  for ind2 in range(ind1+1,samples[-1]+1):
    print(str(ind1)+"-"+str(ind2))
    time=-1
    mrca_ref=-1
    a=0
    for tree in dated_ts.trees():
      if time!=-1:
        if tree.mrca(ind1,ind2)!=mrca_ref:
          IND1+=[ind1] 
          IND2+=[ind2] 
          BLOCK_LENGTH+=[breaks[a]-block_start] 
          POSITION+=[breaks[a]]
          DIVERGENCE+=[time] 
          POP1+=[corr[ind1]]
          POP2+=[corr[ind2]]
          time=tree.tmrca(ind1,ind2)
          mrca_ref=tree.mrca(ind1,ind2)
          block_start=breaks[a]
      else:
        time=tree.tmrca(ind1,ind2)
        mrca_ref=tree.mrca(ind1,ind2)
        block_start=0
      a=a+1 

df2 = pd.DataFrame(
{
  "IND1": IND1, 
  "IND2": IND2, 
  "BLOCK_LENGTH": BLOCK_LENGTH, 
  "POSITION": POSITION,
  "DIVERGENCE": DIVERGENCE,
  "POP1": POP1,
  "POP2": POP2
}
)

compression_opts = dict(method='zip',

                        archive_name='out.csv') 
#df2.to_csv(arg[6], index=False,
#          compression=compression_opts)  
df2.to_csv(arg[6], index=False)
