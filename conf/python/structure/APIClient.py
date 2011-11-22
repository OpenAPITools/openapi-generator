#!/usr/bin/env python
"""Wordnik.com's Swagger generic API client. This client handles the client-
server communication, and is invariant across implementations. Specifics of
the methods and models for each application are generated from the Swagger
templates."""

import sys
import os
import re
import urllib
import urllib2
import httplib
import json

sys.path.append(os.path.dirname(os.path.abspath(__file__)) + '/../')
import model


class APIClient:
    """Generic API client for Swagger client library builds"""

    def __init__(self, apiKey=None, apiServer=None):
        if apiKey == None:
            raise Exception('You must pass an apiKey when instantiating the '
                            'APIClient')
        self.apiKey = apiKey
        self.apiServer = apiServer

    def callAPI(self, resourcePath, method, queryParams, postData,
                headerParams=None):

        url = self.apiServer + resourcePath
        headers = {}
        if headerParams:
            for param, value in headerParams.iteritems():
                headers[param] = value

        headers['Content-type'] = 'application/json'
        headers['api_key'] = self.apiKey

        data = None
        if method == 'GET':
            if queryParams:
                # Need to remove None values, these should not be sent
                sentQueryParams = {}
                for param, value in queryParams.iteritems():
                    if value != None:
                        sentQueryParams[param] = value
                url = url + '?' + urllib.urlencode(sentQueryParams)
            request = urllib2.Request(url=url, headers=headers)
        elif method in ['POST', 'PUT', 'DELETE']:
            data = postData
            if data:
                if type(postData) not in [str, int, float, bool]:
                    data = json.dumps(postData.__dict__)
            request = urllib2.Request(url=url, headers=headers, data=data)
            if method in ['PUT', 'DELETE']:
                # Monkey patch alert! Urllib2 doesn't really do PUT / DELETE
                request.get_method = lambda: method

        else:
            raise Exception('Method ' + method + ' is not recognized.')

        # Make the request
        response = urllib2.urlopen(request).read()

        try:
            data = json.loads(response)
        except ValueError: # PUT requests don't return anything
            data = None

        return data

    def toPathValue(self, obj):
        """Serialize a list to a CSV string, if necessary.
        Args:
            obj -- data object to be serialized
        Returns:
            string -- json serialization of object
        """
        if type(obj) == list:
            return ','.join(obj)
        else:
            return obj

    def deserialize(self, obj, objClass):
        """Derialize a JSON string into an object.

        Args:
            obj -- string or object to be deserialized
            objClass -- class literal for deserialzied object, or string
                of class name
        Returns:
            object -- deserialized object"""

        # Have to accept objClass as string or actual type. Type could be a
        # native Python type, or one of the model classes.
        if type(objClass) == str:
            try:
                objClass = eval(objClass)
            except NameError: # not a native type, must be model class
                objClass = eval('model.' + objClass + '.' + objClass)

        if objClass in [str, int, float, bool]:
            return objClass(obj)

        instance = objClass()

        for attr, attrType in instance.swaggerTypes.iteritems():
            if attr in obj:
                value = obj[attr]
                if attrType in ['str', 'int', 'float', 'bool']:
                    attrType = eval(attrType)
                    try:
                        value = attrType(value)
                    except UnicodeEncodeError:
                        value = unicode(value)
                    setattr(instance, attr, value)
                elif 'list<' in attrType:
                    match = re.match('list<(.*)>', attrType)
                    subClass = match.group(1)
                    subValues = []

                    for subValue in value:
                        subValues.append(self.deserialize(subValue, subClass))
                    setattr(instance, attr, subValues)
                else:
                    setattr(instance, attr, self.deserialize(value,
                                                             objClass))

        return instance
