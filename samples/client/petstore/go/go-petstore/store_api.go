package swagger

import (
    "strings"
    "fmt"
    "encoding/json"
    "errors"
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
func (a StoreApi) DeleteOrder (orderId string) (error, ApiResponse) {

  var httpMethod = "Delete"
      // create path and map variables
  path := a.Configuration.BasePath + "/store/order/{orderId}"
    path = strings.Replace(path, "{" + "orderId" + "}", fmt.Sprintf("%v", orderId), -1)

  // verify the required parameter 'orderId' is set
  if &orderId == nil {
      return errors.New("Missing required parameter 'orderId' when calling StoreApi->DeleteOrder"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}
  var fileBytes []byte

  
  // add default headers if any
  for key := range a.Configuration.DefaultHeader {
      headerParams[key] = a.Configuration.DefaultHeader[key]
  }
  

  // to determine the Content-Type header
  localVarHttpContentTypes := []string {
  }
  //set Content-Type header
  localVarHttpContentType := a.Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes)
  if localVarHttpContentType != "" {    
      headerParams["Content-Type"] = localVarHttpContentType
  }
  // to determine the Accept header
  localVarHttpHeaderAccepts := []string {
      "application/xml", 
      "application/json", 
  }
  //set Accept header
  localVarHttpHeaderAccept := a.Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts)
  if localVarHttpHeaderAccept != "" {  
      headerParams["Accept"] = localVarHttpHeaderAccept
  }



  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams, fileBytes)


  if err != nil {
    return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Returns pet inventories by status
 * Returns a map of status codes to quantities
 * @return map[string]int32
 */
func (a StoreApi) GetInventory () (map[string]int32, error, ApiResponse) {

  var httpMethod = "Get"
      // create path and map variables
  path := a.Configuration.BasePath + "/store/inventory"


  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}
  var fileBytes []byte

  // authentication (api_key) required
  
  // set key with prefix in header
  headerParams["api_key"] = a.Configuration.GetApiKeyWithPrefix("api_key")
      

  // add default headers if any
  for key := range a.Configuration.DefaultHeader {
      headerParams[key] = a.Configuration.DefaultHeader[key]
  }
  

  // to determine the Content-Type header
  localVarHttpContentTypes := []string {
  }
  //set Content-Type header
  localVarHttpContentType := a.Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes)
  if localVarHttpContentType != "" {    
      headerParams["Content-Type"] = localVarHttpContentType
  }
  // to determine the Accept header
  localVarHttpHeaderAccepts := []string {
      "application/json", 
  }
  //set Accept header
  localVarHttpHeaderAccept := a.Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts)
  if localVarHttpHeaderAccept != "" {  
      headerParams["Accept"] = localVarHttpHeaderAccept
  }


  var successPayload = new(map[string]int32)
  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams, fileBytes)


  if err != nil {
    return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  err = json.Unmarshal(httpResponse.Body(), &successPayload)

  return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Find purchase order by ID
 * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
 * @param orderId ID of pet that needs to be fetched
 * @return Order
 */
func (a StoreApi) GetOrderById (orderId int64) (Order, error, ApiResponse) {

  var httpMethod = "Get"
      // create path and map variables
  path := a.Configuration.BasePath + "/store/order/{orderId}"
    path = strings.Replace(path, "{" + "orderId" + "}", fmt.Sprintf("%v", orderId), -1)

  // verify the required parameter 'orderId' is set
  if &orderId == nil {
      return *new(Order), errors.New("Missing required parameter 'orderId' when calling StoreApi->GetOrderById"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}
  var fileBytes []byte

  
  // add default headers if any
  for key := range a.Configuration.DefaultHeader {
      headerParams[key] = a.Configuration.DefaultHeader[key]
  }
  

  // to determine the Content-Type header
  localVarHttpContentTypes := []string {
  }
  //set Content-Type header
  localVarHttpContentType := a.Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes)
  if localVarHttpContentType != "" {    
      headerParams["Content-Type"] = localVarHttpContentType
  }
  // to determine the Accept header
  localVarHttpHeaderAccepts := []string {
      "application/xml", 
      "application/json", 
  }
  //set Accept header
  localVarHttpHeaderAccept := a.Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts)
  if localVarHttpHeaderAccept != "" {  
      headerParams["Accept"] = localVarHttpHeaderAccept
  }


  var successPayload = new(Order)
  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams, fileBytes)


  if err != nil {
    return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  err = json.Unmarshal(httpResponse.Body(), &successPayload)

  return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Place an order for a pet
 * 
 * @param body order placed for purchasing the pet
 * @return Order
 */
func (a StoreApi) PlaceOrder (body Order) (Order, error, ApiResponse) {

  var httpMethod = "Post"
      // create path and map variables
  path := a.Configuration.BasePath + "/store/order"

  // verify the required parameter 'body' is set
  if &body == nil {
      return *new(Order), errors.New("Missing required parameter 'body' when calling StoreApi->PlaceOrder"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}
  var fileBytes []byte

  
  // add default headers if any
  for key := range a.Configuration.DefaultHeader {
      headerParams[key] = a.Configuration.DefaultHeader[key]
  }
  

  // to determine the Content-Type header
  localVarHttpContentTypes := []string {
  }
  //set Content-Type header
  localVarHttpContentType := a.Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes)
  if localVarHttpContentType != "" {    
      headerParams["Content-Type"] = localVarHttpContentType
  }
  // to determine the Accept header
  localVarHttpHeaderAccepts := []string {
      "application/xml", 
      "application/json", 
  }
  //set Accept header
  localVarHttpHeaderAccept := a.Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts)
  if localVarHttpHeaderAccept != "" {  
      headerParams["Accept"] = localVarHttpHeaderAccept
  }

  // body params
  postBody = &body

  var successPayload = new(Order)
  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams, fileBytes)


  if err != nil {
    return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  err = json.Unmarshal(httpResponse.Body(), &successPayload)

  return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
