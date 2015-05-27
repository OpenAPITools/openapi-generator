import base64


def get_api_key_with_prefix(key):
    global api_key
    global api_key_prefix

    if api_key_prefix[key]:
      return api_key_prefix[key] + ' ' + api_key[key]
    else:
      return api_key[key]

def get_basic_auth_token():
    global username
    global password

    return base64.base64encode('Basic ' + username + password)

def auth_settings():
    return { 
               'api_key': {
                   'type': 'api_key',
                   'in': 'header',
                   'key': 'api_key',
                   'value': get_api_key_with_prefix('api_key')
               },
             
           }



# Default Base url
host = "http://petstore.swagger.io/v2"

# Authentication settings

api_key = {}
api_key_prefix = {}
username = ''
password = ''
