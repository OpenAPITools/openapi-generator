from __future__ import absolute_import
import base64
import urllib3
import httplib
import sys
import logging

def singleton(cls, *args, **kw):
    instances = {}

    def _singleton():
        if cls not in instances:
            instances[cls] = cls(*args, **kw)
        return instances[cls]
    return _singleton


@singleton
class Configuration(object):

    def __init__(self):
        # Default Base url
        self.host = "http://petstore.swagger.io/v2"
        # Default api client
        self.api_client = None
        # Authentication Settings
        self.api_key = {}
        self.api_key_prefix = {}
        self.username = ""
        self.password = ""
        # Logging Settings
        self.logging_format = '%(asctime)s %(levelname)s %(message)s'
        self.__logging_file = None
        self.__debug = False
        self.init_logger()

    def init_logger(self):
        self.logger = logging.getLogger()
        formatter = logging.Formatter(self.logging_format)
        stream_handler = logging.StreamHandler()
        stream_handler.setFormatter(formatter)
        self.logger.addHandler(stream_handler)
        if self.__debug:
            self.logger.setLevel(logging.DEBUG)
        else:
            self.logger.setLevel(logging.WARNING)
        if self.__logging_file:
            file_handler = logging.FileHandler(self.__logging_file)
            file_handler.setFormatter(formatter)
            self.logger.addFilter(file_handler)

    @property
    def logging_file(self):
        return self.__logging_file

    @logging_file.setter
    def logging_file(self, value):
        self.__logging_file = value
        if self.__logging_file:
            formater = logging.Formatter(self.logging_format)
            file_handler = logging.FileHandler(self.__logging_file)
            file_handler.setFormatter(formater)
            self.logger.addHandler(file_handler)

    @property
    def debug(self):
        return self.__debug

    @debug.setter
    def debug(self, value):
        self.__debug = value
        if self.__debug:
            # if debug status is True, turn on debug logging
            self.logger.setLevel(logging.DEBUG)
            # turn on httplib debug
            httplib.HTTPConnection.debuglevel = 1
        else:
            # if debug status is False, turn off debug logging,
            # setting log level to default `logging.WARNING`
            self.logger.setLevel(logging.WARNING)

    def get_api_key_with_prefix(self, key):
        """ Return api key prepend prefix for key """
        if self.api_key.get(key) and self.api_key_prefix.get(key):
            return self.api_key_prefix[key] + ' ' + self.api_key[key]
        elif self.api_key.get(key):
            return self.api_key[key]

    def get_basic_auth_token(self):
        """ Return basic auth header string """
        return urllib3.util.make_headers(basic_auth=self.username + ':' + self.password)\
                           .get('authorization')

    def auth_settings(self):
        """ Return Auth Settings for api client """
        return { 
                   'api_key': {
                       'type': 'api_key',
                       'in': 'header',
                       'key': 'api_key',
                       'value': self.get_api_key_with_prefix('api_key')
                   },
                 
              }

    def to_debug_report(self):
        return "Python SDK Debug Report:\n"\
               "OS: {env}\n"\
               "Python Version: {pyversion}\n"\
               "Version of the API: 1.0.0\n"\
               "SDK Package Version: 1.0.0".format(env=sys.platform, pyversion=sys.version)



