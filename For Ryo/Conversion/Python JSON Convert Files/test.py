import csv

dataFile = csv.reader(open('D2013-12-11-04_5741.csv', 'rU'))
dataFile.next() # skip the header line
#'D2013-12-11-04_5741.csv'

for data in dataFile:
    c = map(lambda x:x.strip(), data)

print c[0]