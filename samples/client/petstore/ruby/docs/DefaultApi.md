# Petstore::DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**create_xml_item**](DefaultApi.md#create_xml_item) | **POST** /fake/create_xml_item | creates an XmlItem


# **create_xml_item**
> create_xml_item(xml_item)

creates an XmlItem

this route creates an XmlItem

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::DefaultApi.new
xml_item = Petstore::XmlItem.new # XmlItem | XmlItem Body

begin
  #creates an XmlItem
  api_instance.create_xml_item(xml_item)
rescue Petstore::ApiError => e
  puts "Exception when calling DefaultApi->create_xml_item: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xml_item** | [**XmlItem**](XmlItem.md)| XmlItem Body | 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml, application/xml; charset=utf-8, application/xml; charset=utf-16, text/xml, text/xml; charset=utf-8, text/xml; charset=utf-16
 - **Accept**: Not defined



