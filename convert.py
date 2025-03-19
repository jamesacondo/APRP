#This File is the conversion script for the SIPP. The fully converted output is included in the repository for ease of use.
#Data retrieved from U.S. Census Bureau https://www.census.gov/programs-surveys/sipp.html

import csv

def convert_csv(input_file, output_file):
    with open(input_file, 'r', encoding='utf-8') as infile:
        reader = csv.reader(infile, delimiter='|')
        
        rows = list(reader)
    
        headers = rows[0][0].split('|')
        
        with open(output_file, 'w', newline='', encoding='utf-8') as outfile:
            writer = csv.writer(outfile, quotechar='"', quoting=csv.QUOTE_MINIMAL)
            writer.writerow(headers)
            writer.writerows(rows[1:])

input_file = 'pu2023.csv'
output_file = 'output.csv'
convert_csv(input_file, output_file)
