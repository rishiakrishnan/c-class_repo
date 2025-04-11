import re
import os
import sys

def get_version():
    changelog = open('../../CHANGELOG.md','r').read()
    x = re.findall(r'##\s*\[(.*?)\]',changelog)[0]
    return str(x)
