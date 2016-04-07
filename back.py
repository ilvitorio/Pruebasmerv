import os
import subprocess

path_to_files = 'C:\Users\Vitty2\Documents\Maquinola\Merval\Merval2\\'
path_to_rscript = 'C:\Users\Vitty2\Documents\Maquinola\Live\Pruebasmerv-master\Main.R'

def sorted_ls(path):
#"""list files sorted by time of creation"""
    mtime = lambda f: os.stat(os.path.join(path, f)).st_mtime
    return list(sorted(os.listdir(path), key=mtime))

def run_r(path_to_files, spots, options):
#"""launch R"""
    # Define command and arguments
    r_path_args = [path_to_rscript,
                   path_to_files+spots,
                   path_to_files+options]
    # Build subprocess command

    cmd = ['C:\Program Files\R\R-3.0.2' + '\\'+ 'bin\Rscript'] + r_path_args
    # check_output will run the command and store to result
    x = subprocess.check_output(cmd, universal_newlines=True)
    print 'The R output is: ', x

# Run R for each pair of files
# assumption: the files were created spot, opt, spot,opt
# and are group in pairs
l = sorted_ls(path_to_files)
#cmd= 'PATH C:\Program Files\R\R-3.0.2' + '\\'+ 'bin'
#subprocess.call(cmd)

for i in range(len(l)-1):
    if i % 2 != 0: continue
    print i, l[i], l[i+1]
    run_r(path_to_files, l[i], l[i+1])

  
