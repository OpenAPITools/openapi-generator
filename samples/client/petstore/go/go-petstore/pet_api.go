package swagger

import (
    "strings"
    "fmt"
    "encoding/json"
    "errors"
    "io/ioutil"
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
func (a PetApi) AddPet (body Pet) (error, ApiResponse) {

  var httpMethod = "Post"
 // create path and map variables
  path := a.Configuration.BasePath + "/pet"

  // verify the required parameter 'body' is set
  if &body == nil {
      return errors.New("Missing required parameter 'body' when calling PetApi->AddPet"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}
  var fileName string
  var fileBytes []byte

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
  postBody = &body


  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams, fileName, fileBytes)


  if err != nil {
    return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Deletes a pet
 * 
 * @param petId Pet id to delete
 * @param apiKey 
 * @return void
 */
func (a PetApi) DeletePet (petId int64, apiKey string) (error, ApiResponse) {

  var httpMethod = "Delete"
 // create path and map variables
  path := a.Configuration.BasePath + "/pet/{petId}"
 path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%v", petId), -1)

  // verify the required parameter 'petId' is set
  if &petId == nil {
      return errors.New("Missing required parameter 'petId' when calling PetApi->DeletePet"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}
  var fileName string
  var fileBytes []byte

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



  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams, fileName, fileBytes)


  if err != nil {
    return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Finds Pets by status
 * Multiple status values can be provided with comma separated strings
 * @param status Status values that need to be considered for filter
 * @return []Pet
 */
func (a PetApi) FindPetsByStatus (status []string) ([]Pet, error, ApiResponse) {

  var httpMethod = "Get"
 // create path and map variables
  path := a.Configuration.BasePath + "/pet/findByStatus"

  // verify the required parameter 'status' is set
  if &status == nil {
      return *new([]Pet), errors.New("Missing required parameter 'status' when calling PetApi->FindPetsByStatus"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}
  var fileName string
  var fileBytes []byte

  // authentication (petstore_auth) required
      
  // oauth required
  if a.Configuration.AccessToken != ""{
      headerParams["Authorization"] =  "Bearer " + a.Configuration.AccessToken
  }

  // add default headers if any
  for key := range a.Configuration.DefaultHeader {
      headerParams[key] = a.Configuration.DefaultHeader[key]
  }
  
  queryParams["status"] = a.Configuration.ApiClient.ParameterToString(status)

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
  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams, fileName, fileBytes)


  if err != nil {
    return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  err = json.Unmarshal(httpResponse.Body(), &successPayload)

  return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Finds Pets by tags
 * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
 * @param tags Tags to filter by
 * @return []Pet
 */
func (a PetApi) FindPetsByTags (tags []string) ([]Pet, error, ApiResponse) {

  var httpMethod = "Get"
 // create path and map variables
  path := a.Configuration.BasePath + "/pet/findByTags"

  // verify the required parameter 'tags' is set
  if &tags == nil {
      return *new([]Pet), errors.New("Missing required parameter 'tags' when calling PetApi->FindPetsByTags"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}
  var fileName string
  var fileBytes []byte

  // authentication (petstore_auth) required
      
  // oauth required
  if a.Configuration.AccessToken != ""{
      headerParams["Authorization"] =  "Bearer " + a.Configuration.AccessToken
  }

  // add default headers if any
  for key := range a.Configuration.DefaultHeader {
      headerParams[key] = a.Configuration.DefaultHeader[key]
  }
  
  queryParams["tags"] = a.Configuration.ApiClient.ParameterToString(tags)

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
  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams, fileName, fileBytes)


  if err != nil {
    return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  err = json.Unmarshal(httpResponse.Body(), &successPayload)

  return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Find pet by ID
 * Returns a single pet
 * @param petId ID of pet to return
 * @return Pet
 */
func (a PetApi) GetPetById (petId int64) (Pet, error, ApiResponse) {

  var httpMethod = "Get"
 // create path and map variables
  path := a.Configuration.BasePath + "/pet/{petId}"
 path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%v", petId), -1)

  // verify the required parameter 'petId' is set
  if &petId == nil {
      return *new(Pet), errors.New("Missing required parameter 'petId' when calling PetApi->GetPetById"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}
  var fileName string
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
      "application/xml", 
      "application/json", 
  }
  //set Accept header
  localVarHttpHeaderAccept := a.Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts)
  if localVarHttpHeaderAccept != "" {  
      headerParams["Accept"] = localVarHttpHeaderAccept
  }


  var successPayload = new(Pet)
  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams, fileName, fileBytes)


  if err != nil {
    return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  err = json.Unmarshal(httpResponse.Body(), &successPayload)

  return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Update an existing pet
 * 
 * @param body Pet object that needs to be added to the store
 * @return void
 */
func (a PetApi) UpdatePet (body Pet) (error, ApiResponse) {

  var httpMethod = "Put"
 // create path and map variables
  path := a.Configuration.BasePath + "/pet"

  // verify the required parameter 'body' is set
  if &body == nil {
      return errors.New("Missing required parameter 'body' when calling PetApi->UpdatePet"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}
  var fileName string
  var fileBytes []byte

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
  postBody = &body


  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams, fileName, fileBytes)


  if err != nil {
    return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Updates a pet in the store with form data
 * 
 * @param petId ID of pet that needs to be updated
 * @param name Updated name of the pet
 * @param status Updated status of the pet
 * @return void
 */
func (a PetApi) UpdatePetWithForm (petId int64, name string, status string) (error, ApiResponse) {

  var httpMethod = "Post"
 // create path and map variables
  path := a.Configuration.BasePath + "/pet/{petId}"
 path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%v", petId), -1)

  // verify the required parameter 'petId' is set
  if &petId == nil {
      return errors.New("Missing required parameter 'petId' when calling PetApi->UpdatePetWithForm"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}
  var fileName string
  var fileBytes []byte

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

  formParams["name"] = name
  formParams["status"] = status


  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams, fileName, fileBytes)


  if err != nil {
    return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * uploads an image
 * 
 * @param petId ID of pet to update
 * @param additionalMetadata Additional data to pass to server
 * @param file file to upload
 * @return ApiResponse
 */
func (a PetApi) UploadFile (petId int64, additionalMetadata string, file *os.File) (ApiResponse, error, ApiResponse) {

  var httpMethod = "Post"
 // create path and map variables
  path := a.Configuration.BasePath + "/pet/{petId}/uploadImage"
 path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%v", petId), -1)

  // verify the required parameter 'petId' is set
  if &petId == nil {
      return *new(ApiResponse), errors.New("Missing required parameter 'petId' when calling PetApi->UploadFile"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}
  var fileName string
  var fileBytes []byte

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

  formParams["additionalMetadata"] = additionalMetadata
  fbs, _ := ioutil.ReadAll(file)
  fileBytes = fbs
  fileName = file.Name()

  var successPayload = new(ApiResponse)
  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams, fileName, fileBytes)


  if err != nil {
    return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  err = json.Unmarshal(httpResponse.Body(), &successPayload)

  return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
