Import("env")

## Compatibility for bourne to work on microcontrollers
# We insert '#define _GLIBCXX_USE_C99' in files that use std::stoll or std::to_string
def insert_c99_into(file):
    import fileinput

    path = env['PROJECT_LIBDEPS_DIR'] + "/" + env['PIOENV'] + "/bourne/src/bourne/" + file
    value = '#define _GLIBCXX_USE_C99 1\n'

    for line in fileinput.FileInput(path,inplace=1):
        if line.startswith('#define _GLIBCXX_USE_C99'):
            continue
        elif line.startswith('// D'):
            line=line.replace(line,line+value)
        print(line, end='')

def fix_parser():
    insert_c99_into('detail/parser.cpp')

def fix_json():
    insert_c99_into('json.cpp')

fix_parser()
fix_json()