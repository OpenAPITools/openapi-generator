# Petstore::DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**get_xml_features**](DefaultApi.md#get_xml_features) | **GET** /fake/xmlFeatures | Get some XML
[**post_xml_features**](DefaultApi.md#post_xml_features) | **POST** /fake/xmlFeatures | Post some xml


# **get_xml_features**
> XmlObject get_xml_features

Get some XML

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::DefaultApi.new

begin
  #Get some XML
  result = api_instance.get_xml_features
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling DefaultApi->get_xml_features: #{e}"
end
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**XmlObject**](XmlObject.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml
 - **Accept**: application/xml



# **post_xml_features**
> post_xml_features(xml_object)

Post some xml

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::DefaultApi.new

xml_object = Petstore::XmlObject.new # XmlObject | 


begin
  #Post some xml
  api_instance.post_xml_features(xml_object)
rescue Petstore::ApiError => e
  puts "Exception when calling DefaultApi->post_xml_features: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xml_object** | [**XmlObject**](XmlObject.md)|  | 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml
 - **Accept**: application/xml



