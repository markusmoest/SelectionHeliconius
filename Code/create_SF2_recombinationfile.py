## Creates recombination files for SF2. Input "<scaffoldname<_chrompos_cM.txt" file that 
## contains scaffold and chromosomepositions and the respective cM positions for several linkage maps
## for each site and a SF2 input file and creates a SF2 recombination file with scaffold position and cM DISTANCE 
## between the sites present in the SF" input file

## testruns: python Create_SF2_recombinationfile.py -i BES_C_filtered_BGSFS_test.out -r Hmel218003_chrompos_cM_test.txt -o testoutfile

import sys,argparse

##FUNCTIONS

##something that does the cM distance calculation

##ARGUMENTS

parser=argparse.ArgumentParser()
parser.add_argument("-i", "--SF2inputfile", help="Input SF2 input file", action="store")
parser.add_argument("-r", "--recratefilecM", help="Input recrate file with cM positions", action="store", required = True)
parser.add_argument("-o", "--recoutfile", help="Output recombination rate file for SF", action="store", required = True)

args=parser.parse_args()

#open SF2 inputfile or if not given use stdin for pipe
if args.SF2inputfile:
   SF2input = open(args.SF2inputfile, "r") # read SF2input
else: SF2input = sys.stdin # if no file given read from stdin, allows piping

#open recratefilecM (required)
recratefilecM = open(args.recratefilecM,"r")

#open outputfile
recoutfile = open(args.recoutfile,"w")

#read and print the headers of the input files (just for writing/testing the script)
header_SF2 = SF2input.readline().split()
header_rec = recratefilecM.readline().split()
#print(header_SF2)
#print(header_rec)

#write outputfile header
recoutfile.write("position" + "\t" + "rate" + "\n")

#deal with the first occurrence of the SF2 position
#read SF2 inputfile position
pos_input = SF2input.readline().split()[0]
while True:
    rec_line = recratefilecM.readline().split()
    if rec_line[1] == pos_input:
        recoutfile.write(pos_input + "\t" + "0.0000" +"\n")
        last_rec_line = rec_line
        break

#need to define last_rec_line for first time it's used somehow or place it somewhere else or read first line and set pointer back
pos_input = SF2input.readline().split()[0]
for line in recratefilecM:
    rec_line = line.split()
    #print rec_line 
    if rec_line[1] == pos_input:
        cMdist = float(rec_line[13])-float(last_rec_line[13]) 
        #print cMdist 
        recoutfile.write(pos_input + "\t" + "{:0.4f}".format(cMdist) +"\n")
        last_rec_line = rec_line
        input_line = SF2input.readline().split()
        #print input_line
        if input_line == []:
            print "End of SF2 input file"
            break
        else:
            pos_input = input_line[0]
        
    
        










