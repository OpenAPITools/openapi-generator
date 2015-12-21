package swagger

import (
//    "encoding/json"
    "strings"
    "fmt"
//    "log"
    "github.com/dghubble/sling"
)

type UserApi struct {
    basePath  string
    apiClient ApiClient
    sling *sling.Sling
}

func NewUserApi() *UserApi{
    return &UserApi {
        basePath:   "http://petstore.swagger.io/v2",
    }
}

func NewUserApiWithBasePath(basePath string) *UserApi{
    return &UserApi {
        basePath:   basePath,
    }
}

/**
 * Create user
 * This can only be done by the logged in user.
 * @param body Created user object
 * @return void
 */
//func (a UserApi) createUser (body User) (error) {
func (a UserApi) createUser (body User) (error) {

    _sling := a.sling.Post(a.basePath)

    // create path and map variables
    path := "/user"
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string {  }

    

    // body params
    _sling = _sling.BodyJSON(body)
    //b, _ := json.Marshal(body)
    //bodyParams["body"] = string(b)
    

    
    resp, err := _sling.Request()
    fmt.Println("createUser response: void, ", resp, err)
    return err
    

    

    //response, err := a.apiClient.CallApi(a.basePath, path, "Post", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //}

    //

    //
    //return err
}
/**
 * Creates list of users with given input array
 * 
 * @param body List of user object
 * @return void
 */
//func (a UserApi) createUsersWithArrayInput (body []User) (error) {
func (a UserApi) createUsersWithArrayInput (body []User) (error) {

    _sling := a.sling.Post(a.basePath)

    // create path and map variables
    path := "/user/createWithArray"
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string {  }

    

    // body params
    _sling = _sling.BodyJSON(body)
    //b, _ := json.Marshal(body)
    //bodyParams["body"] = string(b)
    

    
    resp, err := _sling.Request()
    fmt.Println("createUsersWithArrayInput response: void, ", resp, err)
    return err
    

    

    //response, err := a.apiClient.CallApi(a.basePath, path, "Post", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //}

    //

    //
    //return err
}
/**
 * Creates list of users with given input array
 * 
 * @param body List of user object
 * @return void
 */
//func (a UserApi) createUsersWithListInput (body []User) (error) {
func (a UserApi) createUsersWithListInput (body []User) (error) {

    _sling := a.sling.Post(a.basePath)

    // create path and map variables
    path := "/user/createWithList"
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string {  }

    

    // body params
    _sling = _sling.BodyJSON(body)
    //b, _ := json.Marshal(body)
    //bodyParams["body"] = string(b)
    

    
    resp, err := _sling.Request()
    fmt.Println("createUsersWithListInput response: void, ", resp, err)
    return err
    

    

    //response, err := a.apiClient.CallApi(a.basePath, path, "Post", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //}

    //

    //
    //return err
}
/**
 * Logs user into the system
 * 
 * @param username The user name for login
 * @param password The password for login in clear text
 * @return string
 */
//func (a UserApi) loginUser (username string, password string) (string, error) {
func (a UserApi) loginUser (username string, password string) (string, error) {

    _sling := a.sling.Get(a.basePath)

    // create path and map variables
    path := "/user/login"
    

    _sling = _sling.Path(path)

    type QueryParams struct {
        username    string `url:"username,omitempty"`
        password    string `url:"password,omitempty"`
        
    }

    _sling = _sling.QueryStruct(&QueryParams{ username: username,password: password })
    

    

    //contentTypes := []string {  }

    

    

    
    var response string
    resp, err := _sling.ReceiveSuccess(response)
    fmt.Println("loginUser response: ", response, resp, err)
    return response, err
    

    

    //response, err := a.apiClient.CallApi(a.basePath, path, "Get", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //}

    //ApiClient.Deserialize(response, "", "string")

    //var response string
    //err = json.Unmarshal([]byte(req), &response)
    //return response, err
    //
}
/**
 * Logs out current logged in user session
 * 
 * @return void
 */
//func (a UserApi) logoutUser () (error) {
func (a UserApi) logoutUser () (error) {

    _sling := a.sling.Get(a.basePath)

    // create path and map variables
    path := "/user/logout"
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string {  }

    

    

    
    resp, err := _sling.Request()
    fmt.Println("logoutUser response: void, ", resp, err)
    return err
    

    

    //response, err := a.apiClient.CallApi(a.basePath, path, "Get", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //}

    //

    //
    //return err
}
/**
 * Get user by user name
 * 
 * @param username The name that needs to be fetched. Use user1 for testing.
 * @return User
 */
//func (a UserApi) getUserByName (username string) (User, error) {
func (a UserApi) getUserByName (username string) (User, error) {

    _sling := a.sling.Get(a.basePath)

    // create path and map variables
    path := "/user/{username}"
    //path = regexp.MustCompile("{" + "username" + "}").ReplaceAllString(path, "$1")
    //path = path.Replace("\\{" + "username" + "\\}", ApiClient.EscapeString(username))
    path = strings.Replace(path, "{" + "username" + "}", fmt.Sprintf("%b", username), -1)
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string {  }

    

    

    
    var response User
    resp, err := _sling.ReceiveSuccess(response)
    fmt.Println("getUserByName response: ", response, resp, err)
    return response, err
    

    

    //response, err := a.apiClient.CallApi(a.basePath, path, "Get", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //}

    //ApiClient.Deserialize(response, "", "User")

    //var response User
    //err = json.Unmarshal([]byte(req), &response)
    //return response, err
    //
}
/**
 * Updated user
 * This can only be done by the logged in user.
 * @param username name that need to be deleted
 * @param body Updated user object
 * @return void
 */
//func (a UserApi) updateUser (username string, body User) (error) {
func (a UserApi) updateUser (username string, body User) (error) {

    _sling := a.sling.Put(a.basePath)

    // create path and map variables
    path := "/user/{username}"
    //path = regexp.MustCompile("{" + "username" + "}").ReplaceAllString(path, "$1")
    //path = path.Replace("\\{" + "username" + "\\}", ApiClient.EscapeString(username))
    path = strings.Replace(path, "{" + "username" + "}", fmt.Sprintf("%b", username), -1)
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string {  }

    

    // body params
    _sling = _sling.BodyJSON(body)
    //b, _ := json.Marshal(body)
    //bodyParams["body"] = string(b)
    

    
    resp, err := _sling.Request()
    fmt.Println("updateUser response: void, ", resp, err)
    return err
    

    

    //response, err := a.apiClient.CallApi(a.basePath, path, "Put", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //}

    //

    //
    //return err
}
/**
 * Delete user
 * This can only be done by the logged in user.
 * @param username The name that needs to be deleted
 * @return void
 */
//func (a UserApi) deleteUser (username string) (error) {
func (a UserApi) deleteUser (username string) (error) {

    _sling := a.sling.Delete(a.basePath)

    // create path and map variables
    path := "/user/{username}"
    //path = regexp.MustCompile("{" + "username" + "}").ReplaceAllString(path, "$1")
    //path = path.Replace("\\{" + "username" + "\\}", ApiClient.EscapeString(username))
    path = strings.Replace(path, "{" + "username" + "}", fmt.Sprintf("%b", username), -1)
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string {  }

    

    

    
    resp, err := _sling.Request()
    fmt.Println("deleteUser response: void, ", resp, err)
    return err
    

    

    //response, err := a.apiClient.CallApi(a.basePath, path, "Delete", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //}

    //

    //
    //return err
}
