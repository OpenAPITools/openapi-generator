# coding: utf-8

from __future__ import absolute_import
import unittest

from flask import json
from six import BytesIO

from openapi_server.models.client import Client  # noqa: E501
from openapi_server.models.file_schema_test_class import FileSchemaTestClass  # noqa: E501
from openapi_server.models.health_check_result import HealthCheckResult  # noqa: E501
from openapi_server.models.outer_composite import OuterComposite  # noqa: E501
from openapi_server.models.outer_object_with_enum_property import OuterObjectWithEnumProperty  # noqa: E501
from openapi_server.models.pet import Pet  # noqa: E501
from openapi_server.models.user import User  # noqa: E501
from openapi_server.test import BaseTestCase


class TestFakeController(BaseTestCase):
    """FakeController integration test stubs"""

    def test_fake_health_get(self):
        """Test case for fake_health_get

        Health check endpoint
        """
        headers = { 
            'Accept': 'application/json',
        }
        response = self.client.open(
            '/v2/fake/health',
            method='GET',
            headers=headers)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    @unittest.skip("Connexion does not support multiple consumes. See https://github.com/zalando/connexion/pull/760")
    def test_fake_http_signature_test(self):
        """Test case for fake_http_signature_test

        test http signature authentication
        """
        pet = {
  "photoUrls" : [ "photoUrls", "photoUrls" ],
  "name" : "doggie",
  "id" : 0,
  "category" : {
    "name" : "default-name",
    "id" : 6
  },
  "tags" : [ {
    "name" : "name",
    "id" : 1
  }, {
    "name" : "name",
    "id" : 1
  } ],
  "status" : "available"
}
        query_string = [('query_1', 'query_1_example')]
        headers = { 
            'Content-Type': 'application/json',
            'header_1': 'header_1_example',
            
        }
        response = self.client.open(
            '/v2/fake/http-signature-test',
            method='GET',
            headers=headers,
            data=json.dumps(pet),
            content_type='application/json',
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_fake_outer_boolean_serialize(self):
        """Test case for fake_outer_boolean_serialize

        
        """
        body = True
        headers = { 
            'Accept': '*/*',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/v2/fake/outer/boolean',
            method='POST',
            headers=headers,
            data=json.dumps(body),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_fake_outer_composite_serialize(self):
        """Test case for fake_outer_composite_serialize

        
        """
        outer_composite = {
  "my_string" : "my_string",
  "my_number" : 0.8008281904610115,
  "my_boolean" : true
}
        headers = { 
            'Accept': '*/*',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/v2/fake/outer/composite',
            method='POST',
            headers=headers,
            data=json.dumps(outer_composite),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_fake_outer_number_serialize(self):
        """Test case for fake_outer_number_serialize

        
        """
        body = 3.4
        headers = { 
            'Accept': '*/*',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/v2/fake/outer/number',
            method='POST',
            headers=headers,
            data=json.dumps(body),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_fake_outer_string_serialize(self):
        """Test case for fake_outer_string_serialize

        
        """
        body = 'body_example'
        headers = { 
            'Accept': '*/*',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/v2/fake/outer/string',
            method='POST',
            headers=headers,
            data=json.dumps(body),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_fake_property_enum_integer_serialize(self):
        """Test case for fake_property_enum_integer_serialize

        
        """
        outer_object_with_enum_property = {
  "value" : 2
}
        headers = { 
            'Accept': '*/*',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/v2/fake/property/enum-int',
            method='POST',
            headers=headers,
            data=json.dumps(outer_object_with_enum_property),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    @unittest.skip("image/png not supported by Connexion")
    def test_test_body_with_binary(self):
        """Test case for test_body_with_binary

        
        """
        body = '/path/to/file'
        headers = { 
            'Content-Type': 'image/png',
        }
        response = self.client.open(
            '/v2/fake/body-with-binary',
            method='PUT',
            headers=headers,
            data=json.dumps(body),
            content_type='image/png')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_test_body_with_file_schema(self):
        """Test case for test_body_with_file_schema

        
        """
        file_schema_test_class = {
  "file" : {
    "sourceURI" : "sourceURI"
  },
  "files" : [ {
    "sourceURI" : "sourceURI"
  }, {
    "sourceURI" : "sourceURI"
  } ]
}
        headers = { 
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/v2/fake/body-with-file-schema',
            method='PUT',
            headers=headers,
            data=json.dumps(file_schema_test_class),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_test_body_with_query_params(self):
        """Test case for test_body_with_query_params

        
        """
        user = {
  "firstName" : "firstName",
  "lastName" : "lastName",
  "password" : "password",
  "userStatus" : 6,
  "phone" : "phone",
  "id" : 0,
  "email" : "email",
  "username" : "username"
}
        query_string = [('query', 'query_example')]
        headers = { 
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/v2/fake/body-with-query-params',
            method='PUT',
            headers=headers,
            data=json.dumps(user),
            content_type='application/json',
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_test_client_model(self):
        """Test case for test_client_model

        To test \"client\" model
        """
        client = {
  "client" : "client"
}
        headers = { 
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/v2/fake',
            method='PATCH',
            headers=headers,
            data=json.dumps(client),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    @unittest.skip("application/x-www-form-urlencoded not supported by Connexion")
    def test_test_endpoint_parameters(self):
        """Test case for test_endpoint_parameters

        Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
        """
        headers = { 
            'Content-Type': 'application/x-www-form-urlencoded',
            'Authorization': 'Basic Zm9vOmJhcg==',
        }
        data = dict(integer=56,
                    int32=56,
                    int64=56,
                    number=3.4,
                    float=3.4,
                    double=3.4,
                    string='string_example',
                    pattern_without_delimiter='pattern_without_delimiter_example',
                    byte='byte_example',
                    binary='/path/to/file',
                    date='2013-10-20',
                    date_time='2013-10-20T19:20:30+01:00',
                    password='password_example',
                    param_callback='param_callback_example')
        response = self.client.open(
            '/v2/fake',
            method='POST',
            headers=headers,
            data=data,
            content_type='application/x-www-form-urlencoded')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    @unittest.skip("application/x-www-form-urlencoded not supported by Connexion")
    def test_test_enum_parameters(self):
        """Test case for test_enum_parameters

        To test enum parameters
        """
        query_string = [('enum_query_string_array', ['enum_query_string_array_example']),
                        ('enum_query_string', '-efg'),
                        ('enum_query_integer', 56),
                        ('enum_query_double', 3.4)]
        headers = { 
            'Content-Type': 'application/x-www-form-urlencoded',
            'enum_header_string_array': ['enum_header_string_array_example'],
            'enum_header_string': '-efg',
        }
        data = dict(enum_form_string_array='$',
                    enum_form_string='-efg')
        response = self.client.open(
            '/v2/fake',
            method='GET',
            headers=headers,
            data=data,
            content_type='application/x-www-form-urlencoded',
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_test_group_parameters(self):
        """Test case for test_group_parameters

        Fake endpoint to test group parameters (optional)
        """
        query_string = [('required_string_group', 56),
                        ('required_int64_group', 56),
                        ('string_group', 56),
                        ('int64_group', 56)]
        headers = { 
            'required_boolean_group': True,
            'boolean_group': True,
            'Authorization': 'Bearer special-key',
        }
        response = self.client.open(
            '/v2/fake',
            method='DELETE',
            headers=headers,
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_test_inline_additional_properties(self):
        """Test case for test_inline_additional_properties

        test inline additionalProperties
        """
        request_body = {'key': 'request_body_example'}
        headers = { 
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/v2/fake/inline-additionalProperties',
            method='POST',
            headers=headers,
            data=json.dumps(request_body),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    @unittest.skip("application/x-www-form-urlencoded not supported by Connexion")
    def test_test_json_form_data(self):
        """Test case for test_json_form_data

        test json serialization of form data
        """
        headers = { 
            'Content-Type': 'application/x-www-form-urlencoded',
        }
        data = dict(param='param_example',
                    param2='param2_example')
        response = self.client.open(
            '/v2/fake/jsonFormData',
            method='GET',
            headers=headers,
            data=data,
            content_type='application/x-www-form-urlencoded')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_test_query_parameter_collection_format(self):
        """Test case for test_query_parameter_collection_format

        
        """
        query_string = [('pipe', ['pipe_example']),
                        ('ioutil', ['ioutil_example']),
                        ('http', ['http_example']),
                        ('url', ['url_example']),
                        ('context', ['context_example']),
                        ('language', {'key': 'language_example'}),
                        ('allowEmpty', 'allow_empty_example')]
        headers = { 
        }
        response = self.client.open(
            '/v2/fake/test-query-parameters',
            method='PUT',
            headers=headers,
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    unittest.main()
