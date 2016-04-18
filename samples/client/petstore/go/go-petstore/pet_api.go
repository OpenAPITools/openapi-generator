package swagger

import (
    "strings"
    "fmt"
    "encoding/json"
    "errors"
    "os"
)

type PetApi struct {
    Configuration Configuration
}

func NewPetApi() *PetApi{
    configuration := NewConfiguration()
    return &PetApi {
        Configuration: *configuration,
    }
}

func NewPetApiWithBasePath(basePath string) *PetApi{
    configuration := NewConfiguration()
    configuration.BasePath = basePath
    
    return &PetApi {
        Configuration: *configuration,
    }
}

/**
 * Add a new pet to the store
 * 
 * @param body Pet object that needs to be added to the store
 * @return void
 */
func (a PetApi) AddPet (body Pet) (error) {

    var httpMethod = "Post"
        // create path and map variables
    path := c.Configuration.BasePath + "/v2/pet"

    // verify the required parameter 'body' is set
    if &body == nil {
        return errors.New("Missing required parameter 'body' when calling PetApi->AddPet")
    }

    headerParams := make(map[string]string)
    queryParams := make(map[string]string)
    formParams := make(map[string]string)
    fileParams := make(map[string]string)
    formBody := make(interface{})

    // authentication (petstore_auth) required
        
    // oauth required
    if a.Configuration.AccessToken != ""{
        headerParams["Authorization"] =  "Bearer " + a.Configuration.AccessToken
    }


    // add default headers if any
    for key := range a.Configuration.DefaultHeader {
        headerParams[key] = a.Configuration.DefaultHeader[key]
    }
    

    // to determine the Content-Type header
    localVarHttpContentTypes := []string {
        "application/json", 
        "application/xml", 
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
    _sling = _sling.BodyJSON(body)



  // We use this map (below) so that any arbitrary error JSON can be handled.
  // FIXME: This is in the absence of this Go generator honoring the non-2xx
  // response (error) models, which needs to be implemented at some point.
  var failurePayload map[string]interface{}

  httpResponse, err := a.Configuration.ApiClient.CallApi(path, method, postBody, headerParams, queryParams, formParams, fileParams)
  //httpResponse, err := _sling.Receive(nil, &failurePayload)

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
 * Deletes a pet
 * 
 * @param petId Pet id to delete
 * @param apiKey 
 * @return void
 */
func (a PetApi) DeletePet (petId int64, apiKey string) (error) {

    var httpMethod = "Delete"
        // create path and map variables
    path := c.Configuration.BasePath + "/v2/pet/{petId}"
    path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%v", petId), -1)

    // verify the required parameter 'petId' is set
    if &petId == nil {
        return errors.New("Missing required parameter 'petId' when calling PetApi->DeletePet")
    }

    headerParams := make(map[string]string)
    queryParams := make(map[string]string)
    formParams := make(map[string]string)
    fileParams := make(map[string]string)
    formBody := make(interface{})

    // authentication (petstore_auth) required
        
    // oauth required
    if a.Configuration.AccessToken != ""{
        headerParams["Authorization"] =  "Bearer " + a.Configuration.AccessToken
    }


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
    // header params "api_key"
    headerParams["api_key"] = apiKey




  // We use this map (below) so that any arbitrary error JSON can be handled.
  // FIXME: This is in the absence of this Go generator honoring the non-2xx
  // response (error) models, which needs to be implemented at some point.
  var failurePayload map[string]interface{}

  httpResponse, err := a.Configuration.ApiClient.CallApi(path, method, postBody, headerParams, queryParams, formParams, fileParams)
  //httpResponse, err := _sling.Receive(nil, &failurePayload)

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
 * Finds Pets by status
 * Multiple status values can be provided with comma separated strings
 * @param status Status values that need to be considered for filter
 * @return []Pet
 */
func (a PetApi) FindPetsByStatus (status []string) ([]Pet, error) {

    var httpMethod = "Get"
        // create path and map variables
    path := c.Configuration.BasePath + "/v2/pet/findByStatus"

    // verify the required parameter 'status' is set
    if &status == nil {
        return *new([]Pet), errors.New("Missing required parameter 'status' when calling PetApi->FindPetsByStatus")
    }

    headerParams := make(map[string]string)
    queryParams := make(map[string]string)
    formParams := make(map[string]string)
    fileParams := make(map[string]string)
    formBody := make(interface{})

    // authentication (petstore_auth) required
        
    // oauth required
    if a.Configuration.AccessToken != ""{
        headerParams["Authorization"] =  "Bearer " + a.Configuration.AccessToken
    }


    // add default headers if any
    for key := range a.Configuration.DefaultHeader {
        headerParams[key] = a.Configuration.DefaultHeader[key]
    }
    
    queryParams["Status"] =  status

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


  var successPayload = new([]Pet)

  // We use this map (below) so that any arbitrary error JSON can be handled.
  // FIXME: This is in the absence of this Go generator honoring the non-2xx
  // response (error) models, which needs to be implemented at some point.
  var failurePayload map[string]interface{}

  httpResponse, err := a.Configuration.ApiClient.CallApi(path, method, postBody, headerParams, queryParams, formParams, fileParams)
  //httpResponse, err := _sling.Receive(successPayload, &failurePayload)

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
 * Finds Pets by tags
 * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
 * @param tags Tags to filter by
 * @return []Pet
 */
func (a PetApi) FindPetsByTags (tags []string) ([]Pet, error) {

    var httpMethod = "Get"
        // create path and map variables
    path := c.Configuration.BasePath + "/v2/pet/findByTags"

    // verify the required parameter 'tags' is set
    if &tags == nil {
        return *new([]Pet), errors.New("Missing required parameter 'tags' when calling PetApi->FindPetsByTags")
    }

    headerParams := make(map[string]string)
    queryParams := make(map[string]string)
    formParams := make(map[string]string)
    fileParams := make(map[string]string)
    formBody := make(interface{})

    // authentication (petstore_auth) required
        
    // oauth required
    if a.Configuration.AccessToken != ""{
        headerParams["Authorization"] =  "Bearer " + a.Configuration.AccessToken
    }


    // add default headers if any
    for key := range a.Configuration.DefaultHeader {
        headerParams[key] = a.Configuration.DefaultHeader[key]
    }
    
    queryParams["Tags"] =  tags

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


  var successPayload = new([]Pet)

  // We use this map (below) so that any arbitrary error JSON can be handled.
  // FIXME: This is in the absence of this Go generator honoring the non-2xx
  // response (error) models, which needs to be implemented at some point.
  var failurePayload map[string]interface{}

  httpResponse, err := a.Configuration.ApiClient.CallApi(path, method, postBody, headerParams, queryParams, formParams, fileParams)
  //httpResponse, err := _sling.Receive(successPayload, &failurePayload)

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
 * Find pet by ID
 * Returns a single pet
 * @param petId ID of pet to return
 * @return Pet
 */
func (a PetApi) GetPetById (petId int64) (Pet, error) {

    var httpMethod = "Get"
        // create path and map variables
    path := c.Configuration.BasePath + "/v2/pet/{petId}"
    path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%v", petId), -1)

    // verify the required parameter 'petId' is set
    if &petId == nil {
        return *new(Pet), errors.New("Missing required parameter 'petId' when calling PetApi->GetPetById")
    }

    headerParams := make(map[string]string)
    queryParams := make(map[string]string)
    formParams := make(map[string]string)
    fileParams := make(map[string]string)
    formBody := make(interface{})

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
        "application/xml", 
        "application/json", 
    }
    //set Accept header
    localVarHttpHeaderAccept := a.Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts)
    if localVarHttpHeaderAccept != "" {  
        headerParams["Accept"] = localVarHttpHeaderAccept
    }


  var successPayload = new(Pet)

  // We use this map (below) so that any arbitrary error JSON can be handled.
  // FIXME: This is in the absence of this Go generator honoring the non-2xx
  // response (error) models, which needs to be implemented at some point.
  var failurePayload map[string]interface{}

  httpResponse, err := a.Configuration.ApiClient.CallApi(path, method, postBody, headerParams, queryParams, formParams, fileParams)
  //httpResponse, err := _sling.Receive(successPayload, &failurePayload)

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
 * Update an existing pet
 * 
 * @param body Pet object that needs to be added to the store
 * @return void
 */
func (a PetApi) UpdatePet (body Pet) (error) {

    var httpMethod = "Put"
        // create path and map variables
    path := c.Configuration.BasePath + "/v2/pet"

    // verify the required parameter 'body' is set
    if &body == nil {
        return errors.New("Missing required parameter 'body' when calling PetApi->UpdatePet")
    }

    headerParams := make(map[string]string)
    queryParams := make(map[string]string)
    formParams := make(map[string]string)
    fileParams := make(map[string]string)
    formBody := make(interface{})

    // authentication (petstore_auth) required
        
    // oauth required
    if a.Configuration.AccessToken != ""{
        headerParams["Authorization"] =  "Bearer " + a.Configuration.AccessToken
    }


    // add default headers if any
    for key := range a.Configuration.DefaultHeader {
        headerParams[key] = a.Configuration.DefaultHeader[key]
    }
    

    // to determine the Content-Type header
    localVarHttpContentTypes := []string {
        "application/json", 
        "application/xml", 
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
    _sling = _sling.BodyJSON(body)



  // We use this map (below) so that any arbitrary error JSON can be handled.
  // FIXME: This is in the absence of this Go generator honoring the non-2xx
  // response (error) models, which needs to be implemented at some point.
  var failurePayload map[string]interface{}

  httpResponse, err := a.Configuration.ApiClient.CallApi(path, method, postBody, headerParams, queryParams, formParams, fileParams)
  //httpResponse, err := _sling.Receive(nil, &failurePayload)

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
 * Updates a pet in the store with form data
 * 
 * @param petId ID of pet that needs to be updated
 * @param name Updated name of the pet
 * @param status Updated status of the pet
 * @return void
 */
func (a PetApi) UpdatePetWithForm (petId int64, name string, status string) (error) {

    var httpMethod = "Post"
        // create path and map variables
    path := c.Configuration.BasePath + "/v2/pet/{petId}"
    path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%v", petId), -1)

    // verify the required parameter 'petId' is set
    if &petId == nil {
        return errors.New("Missing required parameter 'petId' when calling PetApi->UpdatePetWithForm")
    }

    headerParams := make(map[string]string)
    queryParams := make(map[string]string)
    formParams := make(map[string]string)
    fileParams := make(map[string]string)
    formBody := make(interface{})

    // authentication (petstore_auth) required
        
    // oauth required
    if a.Configuration.AccessToken != ""{
        headerParams["Authorization"] =  "Bearer " + a.Configuration.AccessToken
    }


    // add default headers if any
    for key := range a.Configuration.DefaultHeader {
        headerParams[key] = a.Configuration.DefaultHeader[key]
    }
    

    // to determine the Content-Type header
    localVarHttpContentTypes := []string {
        "application/x-www-form-urlencoded", 
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

    headerParams["Name"] = name
    headerParams["Status"] = status



  // We use this map (below) so that any arbitrary error JSON can be handled.
  // FIXME: This is in the absence of this Go generator honoring the non-2xx
  // response (error) models, which needs to be implemented at some point.
  var failurePayload map[string]interface{}

  httpResponse, err := a.Configuration.ApiClient.CallApi(path, method, postBody, headerParams, queryParams, formParams, fileParams)
  //httpResponse, err := _sling.Receive(nil, &failurePayload)

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
 * uploads an image
 * 
 * @param petId ID of pet to update
 * @param additionalMetadata Additional data to pass to server
 * @param file file to upload
 * @return ApiResponse
 */
func (a PetApi) UploadFile (petId int64, additionalMetadata string, file *os.File) (ApiResponse, error) {

    var httpMethod = "Post"
        // create path and map variables
    path := c.Configuration.BasePath + "/v2/pet/{petId}/uploadImage"
    path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%v", petId), -1)

    // verify the required parameter 'petId' is set
    if &petId == nil {
        return *new(ApiResponse), errors.New("Missing required parameter 'petId' when calling PetApi->UploadFile")
    }

    headerParams := make(map[string]string)
    queryParams := make(map[string]string)
    formParams := make(map[string]string)
    fileParams := make(map[string]string)
    formBody := make(interface{})

    // authentication (petstore_auth) required
        
    // oauth required
    if a.Configuration.AccessToken != ""{
        headerParams["Authorization"] =  "Bearer " + a.Configuration.AccessToken
    }


    // add default headers if any
    for key := range a.Configuration.DefaultHeader {
        headerParams[key] = a.Configuration.DefaultHeader[key]
    }
    

    // to determine the Content-Type header
    localVarHttpContentTypes := []string {
        "multipart/form-data", 
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

    headerParams["AdditionalMetadata"] = additionalMetadata
    headerParams["File"] = file

  var successPayload = new(ApiResponse)

  // We use this map (below) so that any arbitrary error JSON can be handled.
  // FIXME: This is in the absence of this Go generator honoring the non-2xx
  // response (error) models, which needs to be implemented at some point.
  var failurePayload map[string]interface{}

  httpResponse, err := a.Configuration.ApiClient.CallApi(path, method, postBody, headerParams, queryParams, formParams, fileParams)
  //httpResponse, err := _sling.Receive(successPayload, &failurePayload)

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
