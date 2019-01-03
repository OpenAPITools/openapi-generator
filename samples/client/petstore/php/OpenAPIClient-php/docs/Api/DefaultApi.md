# OpenAPI\Client\DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createXmlItem**](DefaultApi.md#createXmlItem) | **POST** /fake/create_xml_item | creates an XmlItem


# **createXmlItem**
> createXmlItem($xml_item)

creates an XmlItem

this route creates an XmlItem

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$apiInstance = new OpenAPI\Client\Api\DefaultApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$xml_item = new \OpenAPI\Client\Model\XmlItem(); // \OpenAPI\Client\Model\XmlItem | XmlItem Body

try {
    $apiInstance->createXmlItem($xml_item);
} catch (Exception $e) {
    echo 'Exception when calling DefaultApi->createXmlItem: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xml_item** | [**\OpenAPI\Client\Model\XmlItem**](../Model/XmlItem.md)| XmlItem Body |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml, application/xml; charset=utf-8, application/xml; charset=utf-16, text/xml, text/xml; charset=utf-8, text/xml; charset=utf-16
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

