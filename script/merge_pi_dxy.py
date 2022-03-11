import os
import glob
import pandas as pd

#SPECIES=['Aboye','Afall''Cgale','Cjuli','Dlabr','Dpunt','Hgutt','Lbude','Lmorm','Mmerl','Msurm','Peryt','Scabr','Scant','Scine','Spilc','Ssard','Styph']
SPECIES=['Cgale']

for sp in SPECIES:
  print(sp)
  os.chdir("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/dxy_pi/")
  extension = 'txt'
  all_filenames = [i for i in glob.glob('*.{}'.format(extension))]
  all_filenames = [i for i in all_filenames if i[0:16]=="dxy_window_"+sp]
  if len(all_filenames)>0:
  	combined_csv = pd.concat([pd.read_csv(f) for f in all_filenames ])
  	#export to csv
  	combined_csv.to_csv( "/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/"+sp+"/dxy_"+sp+".csv", index=False, encoding='utf-8-sig')
  os.chdir("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/dxy_pi/")
  extension = 'txt'
  all_filenames = [i for i in glob.glob('*.{}'.format(extension))]
  all_filenames = [i for i in all_filenames if i[0:15]=="pi_window_"+sp]
  if len(all_filenames)>0:
  	combined_csv = pd.concat([pd.read_csv(f) for f in all_filenames ])
  	#export to csv
  	combined_csv.to_csv( "/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/"+sp+"/pi_"+sp+".csv", index=False, encoding='utf-8-sig')

