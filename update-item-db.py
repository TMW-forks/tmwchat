#!/usr/bin/python

"""
Update text file containing item IDs and names.
Author: Joseph Botosh <rumly111@gmail.com>
Licence: GPLv2.
"""

import os
import sys
from xml.etree.ElementTree import ElementTree

def ScanItemsXML(filename):
    result = []
    file1 = ElementTree(file=filename)
    for item1 in filter(lambda it: it.get('name'), file1.getroot()):
        file2 = ElementTree(file=item1.get('name'))
        for item2 in filter(lambda it: it.get('name'), file2.getroot()):
            file3 = ElementTree(file=item2.get('name'))
            for item3 in file3.getroot():
                name = item3.get('name')
                id_ = item3.get('id')
                if name is not None and id_ is not None and int(id_) > 0:
                    result.append((id_, name))
    return result

def PrintHelp():
    print('Usage: %s <items.xml>'.format(sys.argv[0]))

if __name__ == '__main__':
    if len(sys.argv) == 1:
        PrintHelp()
        sys.exit(0)
    filename = sys.argv[1]
    if os.path.isfile(filename):
        for id_, name in ScanItemsXML(filename):
            print(id_, name)
    else:
        print('File not found:', filename, file=sys.stderr)
        sys.exit(1)
