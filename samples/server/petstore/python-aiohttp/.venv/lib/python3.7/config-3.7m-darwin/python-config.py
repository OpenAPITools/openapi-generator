#!/Library/Frameworks/Python.framework/Versions/3.7/bin/python3.7m
# -*- python -*-

# Keep this script in sync with python-config.sh.in

import getopt
import os
import sys
import sysconfig

valid_opts = ['prefix', 'exec-prefix', 'includes', 'libs', 'cflags',
              'ldflags', 'extension-suffix', 'help', 'abiflags', 'configdir']

def exit_with_usage(code=1):
    print("Usage: {0} [{1}]".format(
        sys.argv[0], '|'.join('--'+opt for opt in valid_opts)), file=sys.stderr)
    sys.exit(code)

try:
    opts, args = getopt.getopt(sys.argv[1:], '', valid_opts)
except getopt.error:
    exit_with_usage()

if not opts:
    exit_with_usage()

pyver = sysconfig.get_config_var('VERSION')
getvar = sysconfig.get_config_var

opt_flags = [flag for (flag, val) in opts]

if '--help' in opt_flags:
    exit_with_usage(code=0)

for opt in opt_flags:
    if opt == '--prefix':
        print(sysconfig.get_config_var('prefix'))

    elif opt == '--exec-prefix':
        print(sysconfig.get_config_var('exec_prefix'))

    elif opt in ('--includes', '--cflags'):
        flags = ['-I' + sysconfig.get_path('include'),
                 '-I' + sysconfig.get_path('platinclude')]
        if opt == '--cflags':
            flags.extend(getvar('CFLAGS').split())
        print(' '.join(flags))

    elif opt in ('--libs', '--ldflags'):
        libs = ['-lpython' + pyver + sys.abiflags]
        libs += getvar('LIBS').split()
        libs += getvar('SYSLIBS').split()
        # add the prefix/lib/pythonX.Y/config dir, but only if there is no
        # shared library in prefix/lib/.
        if opt == '--ldflags':
            if not getvar('Py_ENABLE_SHARED'):
                libs.insert(0, '-L' + getvar('LIBPL'))
            if not getvar('PYTHONFRAMEWORK'):
                libs.extend(getvar('LINKFORSHARED').split())
        print(' '.join(libs))

    elif opt == '--extension-suffix':
        print(sysconfig.get_config_var('EXT_SUFFIX'))

    elif opt == '--abiflags':
        print(sys.abiflags)

    elif opt == '--configdir':
        print(sysconfig.get_config_var('LIBPL'))
