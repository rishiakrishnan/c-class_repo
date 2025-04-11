import os
import sys
import re

def gen_schema_doc():
    text = open('../../configure/schema.yaml','r').read()
    rst_file = open('schema_doc.rst','w')
    x = re.findall("^###(?:(?:\r\n|[\r\n]).+$)*",text,re.M|re.U)
    for y in x:
        y = y.replace('#','')
        y = y.lstrip(' ')
        rst_file.write(y+'\n')
    
    rst_file.close()

