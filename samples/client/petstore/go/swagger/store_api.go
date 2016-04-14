package swagger

import (
    "strings"
    "fmt"
    "encoding/json"
    "errors"
    "github.com/dghubble/sling"
)

type StoreApi struct {
    Configuration Configuration
}

func NewStoreApi() *StoreApi{
    configuration := NewConfiguration()
    return &StoreApi {
        Configuration: *configuration,
    }
}

func NewStoreApiWithBasePath(basePath string) *StoreApi{
    configuration := NewConfiguration()
    configuration.BasePath = basePath
    
    return &StoreApi {
        Configuration: *configuration,
    }
}

/**
 * Delete purchase order by ID
 * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
 * @param orderId ID of the order that needs to be deleted
 * @return void
 */
func (a StoreApi) DeleteOrder (orderId string) (error) {

    _sling := sling.New().Delete(a.Configuration.BasePath)

    
    // create path and map variables
    path := "/v2/store/order/{orderId}"
    path = strings.Replace(path, "{" + "orderId" + "}", fmt.Sprintf("%v", orderId), -1)

    _sling = _sling.Path(path)

    // add default headers if any
    for key := range a.Configuration.DefaultHeader {
      _sling = _sling.Set(key, a.Configuration.DefaultHeader[key])
    }
    
    // accept header
    accepts := []string { "application/xml", "application/json" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }




  // We use this map (below) so that any arbitrary error JSON can be handled.
  // FIXME: This is in the absence of this Go generator honoring the non-2xx
  // response (error) models, which needs to be implemented at some point.
  var failurePayload map[string]interface{}

  httpResponse, err := _sling.Receive(nil, &failurePayload)

  if err == nil {
    // err == nil only means that there wasn't a sub-application-layer error (e.g. no network error)
    if failurePayload != nil {
      // If the failurePayload is present, there likely was some kind of non-2xx status
      // returned (and a JSON payload error present)
      var str []byte
      str, err = json.Marshal(failurePayload)
      if err == nil { // For safety, check for an error marshalling... probably superfluous
        // This will return the JSON error body as a string
        err = errors.New(string(str))
      }
  } else {
    // So, there was no network-type error, and nothing in the failure payload,
    // but we should still check the status code
    if httpResponse == nil {
      // This should never happen...
      err = errors.New("No HTTP Response received.")
    } else if code := httpResponse.StatusCode; 200 > code || code > 299 {
        err = errors.New("HTTP Error: " + string(httpResponse.StatusCode))
      }
    }
  }

  return err
}
/**
 * Returns pet inventories by status
 * Returns a map of status codes to quantities
 * @return map[string]int32
 */
func (a StoreApi) GetInventory () (map[string]int32, error) {

    _sling := sling.New().Get(a.Configuration.BasePath)

    // authentication (api_key) required
    
    // set key with prefix in header
    _sling.Set("api_key", a.Configuration.GetApiKeyWithPrefix("api_key"))
        

    // create path and map variables
    path := "/v2/store/inventory"

    _sling = _sling.Path(path)

    // add default headers if any
    for key := range a.Configuration.DefaultHeader {
      _sling = _sling.Set(key, a.Configuration.DefaultHeader[key])
    }
    
    // accept header
    accepts := []string { "application/json" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }


  var successPayload = new(map[string]int32)

  // We use this map (below) so that any arbitrary error JSON can be handled.
  // FIXME: This is in the absence of this Go generator honoring the non-2xx
  // response (error) models, which needs to be implemented at some point.
  var failurePayload map[string]interface{}

  httpResponse, err := _sling.Receive(successPayload, &failurePayload)

  if err == nil {
    // err == nil only means that there wasn't a sub-application-layer error (e.g. no network error)
    if failurePayload != nil {
      // If the failurePayload is present, there likely was some kind of non-2xx status
      // returned (and a JSON payload error present)
      var str []byte
      str, err = json.Marshal(failurePayload)
      if err == nil { // For safety, check for an error marshalling... probably superfluous
        // This will return the JSON error body as a string
        err = errors.New(string(str))
      }
  } else {
    // So, there was no network-type error, and nothing in the failure payload,
    // but we should still check the status code
    if httpResponse == nil {
      // This should never happen...
      err = errors.New("No HTTP Response received.")
    } else if code := httpResponse.StatusCode; 200 > code || code > 299 {
        err = errors.New("HTTP Error: " + string(httpResponse.StatusCode))
      }
    }
  }

  return *successPayload, err
}
/**
 * Find purchase order by ID
 * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
 * @param orderId ID of pet that needs to be fetched
 * @return Order
 */
func (a StoreApi) GetOrderById (orderId int64) (Order, error) {

    _sling := sling.New().Get(a.Configuration.BasePath)

    
    // create path and map variables
    path := "/v2/store/order/{orderId}"
    path = strings.Replace(path, "{" + "orderId" + "}", fmt.Sprintf("%v", orderId), -1)

    _sling = _sling.Path(path)

    // add default headers if any
    for key := range a.Configuration.DefaultHeader {
      _sling = _sling.Set(key, a.Configuration.DefaultHeader[key])
    }
    
    // accept header
    accepts := []string { "application/xml", "application/json" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }


  var successPayload = new(Order)

  // We use this map (below) so that any arbitrary error JSON can be handled.
  // FIXME: This is in the absence of this Go generator honoring the non-2xx
  // response (error) models, which needs to be implemented at some point.
  var failurePayload map[string]interface{}

  httpResponse, err := _sling.Receive(successPayload, &failurePayload)

  if err == nil {
    // err == nil only means that there wasn't a sub-application-layer error (e.g. no network error)
    if failurePayload != nil {
      // If the failurePayload is present, there likely was some kind of non-2xx status
      // returned (and a JSON payload error present)
      var str []byte
      str, err = json.Marshal(failurePayload)
      if err == nil { // For safety, check for an error marshalling... probably superfluous
        // This will return the JSON error body as a string
        err = errors.New(string(str))
      }
  } else {
    // So, there was no network-type error, and nothing in the failure payload,
    // but we should still check the status code
    if httpResponse == nil {
      // This should never happen...
      err = errors.New("No HTTP Response received.")
    } else if code := httpResponse.StatusCode; 200 > code || code > 299 {
        err = errors.New("HTTP Error: " + string(httpResponse.StatusCode))
      }
    }
  }

  return *successPayload, err
}
/**
 * Place an order for a pet
 * 
 * @param body order placed for purchasing the pet
 * @return Order
 */
func (a StoreApi) PlaceOrder (body Order) (Order, error) {

    _sling := sling.New().Post(a.Configuration.BasePath)

    
    // create path and map variables
    path := "/v2/store/order"

    _sling = _sling.Path(path)

    // add default headers if any
    for key := range a.Configuration.DefaultHeader {
      _sling = _sling.Set(key, a.Configuration.DefaultHeader[key])
    }
    
    // accept header
    accepts := []string { "application/xml", "application/json" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }

// body params
    _sling = _sling.BodyJSON(body)

  var successPayload = new(Order)

  // We use this map (below) so that any arbitrary error JSON can be handled.
  // FIXME: This is in the absence of this Go generator honoring the non-2xx
  // response (error) models, which needs to be implemented at some point.
  var failurePayload map[string]interface{}

  httpResponse, err := _sling.Receive(successPayload, &failurePayload)

  if err == nil {
    // err == nil only means that there wasn't a sub-application-layer error (e.g. no network error)
    if failurePayload != nil {
      // If the failurePayload is present, there likely was some kind of non-2xx status
      // returned (and a JSON payload error present)
      var str []byte
      str, err = json.Marshal(failurePayload)
      if err == nil { // For safety, check for an error marshalling... probably superfluous
        // This will return the JSON error body as a string
        err = errors.New(string(str))
      }
  } else {
    // So, there was no network-type error, and nothing in the failure payload,
    // but we should still check the status code
    if httpResponse == nil {
      // This should never happen...
      err = errors.New("No HTTP Response received.")
    } else if code := httpResponse.StatusCode; 200 > code || code > 299 {
        err = errors.New("HTTP Error: " + string(httpResponse.StatusCode))
      }
    }
  }

  return *successPayload, err
}
