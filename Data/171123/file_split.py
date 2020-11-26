'''
file_split.py - Splits a .prn file into multiple .csv files.
'''

import os

def close_file(_file):
    '''
    Attempts to flush and close the provided file.
    '''
    try:
        _file.flush() # flush data in case some is still pending to be written
        _file.close() # close the file
    except IOError as ioe:
        print 'Failed to close file: ' + repr(ioe)

def rename_file(_src, _dst):
    '''
    Attempts to rename the provided src file to dst. If dst already exists it will be removed.
    '''
    try:
        if os.path.isfile(_dst): # delete the file if it exists for some reason
            os.remove(_dst)
        os.rename(_src, _dst) # rename the file as we should know its name
    except IOError as ioe:
        print 'Failed to rename %s to %s. %s' % (_src, _dst, repr(ioe))

# using raw_inpute over input as it does not require quotes.
FILE = open(raw_input('Enter file name: '), 'r')
FOLDER = raw_input('Enter folder destination name: ')

TEMP = 'temp.csv'

if not os.path.isdir(FOLDER):
    os.mkdir(FOLDER)
os.chdir(FOLDER)

NAME = ''
COUNTER = 0
OUT = open(TEMP, 'w')
for line in FILE:
    if line == '\n' and COUNTER > 0:
        close_file(OUT)
        rename_file(TEMP, NAME)
        OUT = open(TEMP, 'w')
        COUNTER = 0 # reset counter
    if line.strip() == '': # strip leading and trailing white spaces in case its full of spaces
        continue    # just skip if its empty
    else: # proceses the data and add it to OUT
        if COUNTER == 1: # line contains name
            head = line.split(',')
            # 1:-1 strips the "" off the name
            NAME = head[0][1:-1] + '.csv'
        if COUNTER > 1: # strip first col after header
            # find the index of the first comma and use the rest of the string
            line = line[line.index(',') + 1:]
            if COUNTER == 2:
                # insert 's' into '"Time(sec)"'. Should be 9th index.
                # could also just do '"Time(secs)"' + line[11:]
                #line = line[:9] + 's' + line[9:]
				commas = line.count(',')
				line = "Time in Seconds" + line[11:] + "0,"*commas + "\n" 
        if COUNTER != 0: # don't write the first line
            OUT.write(line) # add any line after header
        COUNTER += 1

close_file(OUT)
rename_file(TEMP, NAME)

FILE.close()
