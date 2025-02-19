#!/bin/env python

# Generates and prints a list of reserved words in Godot

import xml.etree.ElementTree as ET
import requests


words_code = ""  # output
indentation = "                        "
max_line_len = 119
url = "https://raw.githubusercontent.com/godotengine/godot/master/doc/classes/%40GlobalScope.xml"

xml_string = requests.get(url).content
root = ET.fromstring(xml_string)

methods_list = []  # of string
constants_list = []  # of string
singletons_list = []  # of string

for method in root.iter('method'):
    methods_list.append(method.attrib['name'])
for constant in root.iter('constant'):
    constants_list.append(constant.attrib['name'])
for singleton in root.iter('member'):
    singletons_list.append(singleton.attrib['name'])

words_code += "%s// List generated from modules/openapi-generator/src/main/resources/gdscript/utils/extract_reserved_words.py\n" % indentation

current_line = ""

def new_line():
    global current_line
    current_line = "%s" % indentation

def write_line():
    global current_line, words_code
    words_code += "%s\n" % current_line

def add_word(word):
    global current_line, words_code
    words_string = "\"%s\", " % word
    if len(current_line) + len(words_string) > max_line_len:
        write_line()
        new_line()
    current_line += words_string

new_line()

words_code += "%s// Godot's global functions\n" % indentation
for reserved in methods_list:
    add_word(reserved)
words_code += "%s// Godot's global constants\n" % indentation
for reserved in constants_list:
    add_word(reserved)
words_code += "%s// Godot's singletons\n" % indentation
for reserved in singletons_list:
    add_word(reserved)

write_line()

print(words_code)
