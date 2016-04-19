package swagger

import (
    "strings"
    "fmt"
    "encoding/json"
    "errors"
)

type UserApi struct {
    Configuration Configuration
}

func NewUserApi() *UserApi{
    configuration := NewConfiguration()
    return &UserApi {
        Configuration: *configuration,
    }
}

func NewUserApiWithBasePath(basePath string) *UserApi{
    configuration := NewConfiguration()
    configuration.BasePath = basePath
    
    return &UserApi {
        Configuration: *configuration,
    }
}

/**
 * Create user
 * This can only be done by the logged in user.
 * @param body Created user object
 * @return void
 */
func (a UserApi) CreateUser (body User) (error, ApiResponse) {

  var httpMethod = "Post"
      // create path and map variables
  path := a.Configuration.BasePath + "/user"

  // verify the required parameter 'body' is set
  if &body == nil {
      return errors.New("Missing required parameter 'body' when calling UserApi->CreateUser"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}

  
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


  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams)


  if err != nil {
    return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Creates list of users with given input array
 * 
 * @param body List of user object
 * @return void
 */
func (a UserApi) CreateUsersWithArrayInput (body []User) (error, ApiResponse) {

  var httpMethod = "Post"
      // create path and map variables
  path := a.Configuration.BasePath + "/user/createWithArray"

  // verify the required parameter 'body' is set
  if &body == nil {
      return errors.New("Missing required parameter 'body' when calling UserApi->CreateUsersWithArrayInput"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}

  
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


  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams)


  if err != nil {
    return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Creates list of users with given input array
 * 
 * @param body List of user object
 * @return void
 */
func (a UserApi) CreateUsersWithListInput (body []User) (error, ApiResponse) {

  var httpMethod = "Post"
      // create path and map variables
  path := a.Configuration.BasePath + "/user/createWithList"

  // verify the required parameter 'body' is set
  if &body == nil {
      return errors.New("Missing required parameter 'body' when calling UserApi->CreateUsersWithListInput"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}

  
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


  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams)


  if err != nil {
    return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Delete user
 * This can only be done by the logged in user.
 * @param username The name that needs to be deleted
 * @return void
 */
func (a UserApi) DeleteUser (username string) (error, ApiResponse) {

  var httpMethod = "Delete"
      // create path and map variables
  path := a.Configuration.BasePath + "/user/{username}"
    path = strings.Replace(path, "{" + "username" + "}", fmt.Sprintf("%v", username), -1)

  // verify the required parameter 'username' is set
  if &username == nil {
      return errors.New("Missing required parameter 'username' when calling UserApi->DeleteUser"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}

  
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



  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams)


  if err != nil {
    return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Get user by user name
 * 
 * @param username The name that needs to be fetched. Use user1 for testing. 
 * @return User
 */
func (a UserApi) GetUserByName (username string) (User, error, ApiResponse) {

  var httpMethod = "Get"
      // create path and map variables
  path := a.Configuration.BasePath + "/user/{username}"
    path = strings.Replace(path, "{" + "username" + "}", fmt.Sprintf("%v", username), -1)

  // verify the required parameter 'username' is set
  if &username == nil {
      return *new(User), errors.New("Missing required parameter 'username' when calling UserApi->GetUserByName"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}

  
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


  var successPayload = new(User)
  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams)


  if err != nil {
    return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  err = json.Unmarshal(httpResponse.Body(), &successPayload)

  return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Logs user into the system
 * 
 * @param username The user name for login
 * @param password The password for login in clear text
 * @return string
 */
func (a UserApi) LoginUser (username string, password string) (string, error, ApiResponse) {

  var httpMethod = "Get"
      // create path and map variables
  path := a.Configuration.BasePath + "/user/login"

  // verify the required parameter 'username' is set
  if &username == nil {
      return *new(string), errors.New("Missing required parameter 'username' when calling UserApi->LoginUser"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }
  // verify the required parameter 'password' is set
  if &password == nil {
      return *new(string), errors.New("Missing required parameter 'password' when calling UserApi->LoginUser"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}

  
  // add default headers if any
  for key := range a.Configuration.DefaultHeader {
      headerParams[key] = a.Configuration.DefaultHeader[key]
  }
  
  queryParams["Username"] = a.Configuration.ApiClient.ParameterToString(username)
  queryParams["Password"] = a.Configuration.ApiClient.ParameterToString(password)

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


  var successPayload = new(string)
  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams)


  if err != nil {
    return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  err = json.Unmarshal(httpResponse.Body(), &successPayload)

  return *successPayload, err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Logs out current logged in user session
 * 
 * @return void
 */
func (a UserApi) LogoutUser () (error, ApiResponse) {

  var httpMethod = "Get"
      // create path and map variables
  path := a.Configuration.BasePath + "/user/logout"


  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}

  
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



  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams)


  if err != nil {
    return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
/**
 * Updated user
 * This can only be done by the logged in user.
 * @param username name that need to be deleted
 * @param body Updated user object
 * @return void
 */
func (a UserApi) UpdateUser (username string, body User) (error, ApiResponse) {

  var httpMethod = "Put"
      // create path and map variables
  path := a.Configuration.BasePath + "/user/{username}"
    path = strings.Replace(path, "{" + "username" + "}", fmt.Sprintf("%v", username), -1)

  // verify the required parameter 'username' is set
  if &username == nil {
      return errors.New("Missing required parameter 'username' when calling UserApi->UpdateUser"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }
  // verify the required parameter 'body' is set
  if &body == nil {
      return errors.New("Missing required parameter 'body' when calling UserApi->UpdateUser"), *a.Configuration.ApiClient.SetErrorApiResponse("400 - Bad Request")
  }

  headerParams := make(map[string]string)
  queryParams := make(map[string]string)
  formParams := make(map[string]string)
  var postBody interface{}

  
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


  httpResponse, err := a.Configuration.ApiClient.CallApi(path, httpMethod, postBody, headerParams, queryParams, formParams)


  if err != nil {
    return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
  }

  return err, *a.Configuration.ApiClient.GetApiResponse(httpResponse)
}
