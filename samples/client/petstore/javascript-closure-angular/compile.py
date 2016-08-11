#!/usr/bin/python

import httplib, urllib, sys

# Collect all the files in an order that will work. That is Models first then APIs
def concatFiles(files):
    code = ""
    for file in files:
        code += open(file).read()
    return code

def makeRequest(params):
    # Always use the following value for the Content-type header.
    headers = { "Content-type": "application/x-www-form-urlencoded" }
    conn = httplib.HTTPConnection('closure-compiler.appspot.com')
    conn.request('POST', '/compile', params, headers)
    response = conn.getresponse()
    data = response.read()
    conn.close()
    return data

def checkForCompilerErrors(files):
    params = urllib.urlencode([
        ('js_code', concatFiles(files)),
        ('compilation_level', 'SIMPLE_OPTIMIZATIONS'),
        ('language', 'ECMASCRIPT5_STRICT'),
        ('output_format', 'text'),
        ('output_info', 'errors'),
      ])

    return makeRequest(params)

def compile(output, files):
    params = urllib.urlencode([
        ('js_code', concatFiles(files)),
        ('compilation_level', 'SIMPLE_OPTIMIZATIONS'),
        ('language', 'ECMASCRIPT5_STRICT'),
        ('output_format', 'text'),
        ('output_info', 'compiled_code'),
      ])

    f = open(output, 'w')
    f.write(makeRequest(params))
    f.close()

targets = {
    "PetAPI": ["API/Client/Tag.js", "API/Client/Category.js", "API/Client/Pet.js", "API/Client/PetApi.js"],
    "StoreAPI": ["API/Client/Order.js", "API/Client/StoreApi.js"],
    "UserAPI": ["API/Client/User.js", "API/Client/UserApi.js"],
}

def main():
    for name, targetFiles in targets.iteritems():
        errors = checkForCompilerErrors(targetFiles)
        if errors:
            print "Compiler errors when building %s" % name
            print errors

    for name, targetFiles in targets.iteritems():
        compile("%s.compiled.js" % name, targets[name])

if __name__ == "__main__":
    sys.exit(main())
