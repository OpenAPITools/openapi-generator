# DefaultApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**createEvent**](DefaultApi.md#createevent) | **POST** /events |  |
| [**listEvents**](DefaultApi.md#listevents) | **GET** /events/{onDate} |  |



## createEvent

> Event createEvent(startsOn, createdAt)



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { CreateEventRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  const body = {
    // string
    startsOn: 2013-10-20,
    // string (optional)
    createdAt: 2013-10-20T19:20:30+01:00,
  } satisfies CreateEventRequest;

  try {
    const data = await api.createEvent(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **startsOn** | `string` |  | [Defaults to `undefined`] |
| **createdAt** | `string` |  | [Optional] [Defaults to `undefined`] |

### Return type

[**Event**](Event.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/x-www-form-urlencoded`
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | the created event |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## listEvents

> Array&lt;Event&gt; listEvents(onDate, from, updatedSince)



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { ListEventsRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  const body = {
    // string
    onDate: 2013-10-20,
    // string (optional)
    from: 2013-10-20,
    // string (optional)
    updatedSince: 2013-10-20T19:20:30+01:00,
  } satisfies ListEventsRequest;

  try {
    const data = await api.listEvents(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **onDate** | `string` |  | [Defaults to `undefined`] |
| **from** | `string` |  | [Optional] [Defaults to `undefined`] |
| **updatedSince** | `string` |  | [Optional] [Defaults to `undefined`] |

### Return type

[**Array&lt;Event&gt;**](Event.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | matching events |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

