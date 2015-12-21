package swagger

import (
    "strings"
    "fmt"
    "github.com/dghubble/sling"
)

type UserApi struct {
    basePath  string
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
 * @param Body Created user object
 * @return void
 */
//func (a UserApi) CreateUser (Body User) (error) {
func (a UserApi) CreateUser (Body User) (error) {

    _sling := sling.New().Post(a.basePath)

    // create path and map variables
    path := "/v2/user"

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }

// body params
    _sling = _sling.BodyJSON(Body)



    _, err := _sling.ReceiveSuccess(nil)
    //fmt.Println("CreateUser response: void, ", resp, err)
    return err
}
/**
 * Creates list of users with given input array
 * 
 * @param Body List of user object
 * @return void
 */
//func (a UserApi) CreateUsersWithArrayInput (Body []User) (error) {
func (a UserApi) CreateUsersWithArrayInput (Body []User) (error) {

    _sling := sling.New().Post(a.basePath)

    // create path and map variables
    path := "/v2/user/createWithArray"

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }

// body params
    _sling = _sling.BodyJSON(Body)



    _, err := _sling.ReceiveSuccess(nil)
    //fmt.Println("CreateUsersWithArrayInput response: void, ", resp, err)
    return err
}
/**
 * Creates list of users with given input array
 * 
 * @param Body List of user object
 * @return void
 */
//func (a UserApi) CreateUsersWithListInput (Body []User) (error) {
func (a UserApi) CreateUsersWithListInput (Body []User) (error) {

    _sling := sling.New().Post(a.basePath)

    // create path and map variables
    path := "/v2/user/createWithList"

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }

// body params
    _sling = _sling.BodyJSON(Body)



    _, err := _sling.ReceiveSuccess(nil)
    //fmt.Println("CreateUsersWithListInput response: void, ", resp, err)
    return err
}
/**
 * Logs user into the system
 * 
 * @param Username The user name for login
 * @param Password The password for login in clear text
 * @return string
 */
//func (a UserApi) LoginUser (Username string, Password string) (string, error) {
func (a UserApi) LoginUser (Username string, Password string) (string, error) {

    _sling := sling.New().Get(a.basePath)

    // create path and map variables
    path := "/v2/user/login"

    _sling = _sling.Path(path)

    type QueryParams struct {
        Username    string `url:"username,omitempty"`
        Password    string `url:"password,omitempty"`
        
}
    _sling = _sling.QueryStruct(&QueryParams{ Username: Username,Password: Password })
    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }



    response := new(string)
    _, err := _sling.ReceiveSuccess(response)
    //fmt.Println("LoginUser response: ", response, resp, err)
    return *response, err
}
/**
 * Logs out current logged in user session
 * 
 * @return void
 */
//func (a UserApi) LogoutUser () (error) {
func (a UserApi) LogoutUser () (error) {

    _sling := sling.New().Get(a.basePath)

    // create path and map variables
    path := "/v2/user/logout"

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }




    _, err := _sling.ReceiveSuccess(nil)
    //fmt.Println("LogoutUser response: void, ", resp, err)
    return err
}
/**
 * Get user by user name
 * 
 * @param Username The name that needs to be fetched. Use user1 for testing.
 * @return User
 */
//func (a UserApi) GetUserByName (Username string) (User, error) {
func (a UserApi) GetUserByName (Username string) (User, error) {

    _sling := sling.New().Get(a.basePath)

    // create path and map variables
    path := "/v2/user/{username}"
    path = strings.Replace(path, "{" + "username" + "}", fmt.Sprintf("%v", Username), -1)

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }



    response := new(User)
    _, err := _sling.ReceiveSuccess(response)
    //fmt.Println("GetUserByName response: ", response, resp, err)
    return *response, err
}
/**
 * Updated user
 * This can only be done by the logged in user.
 * @param Username name that need to be deleted
 * @param Body Updated user object
 * @return void
 */
//func (a UserApi) UpdateUser (Username string, Body User) (error) {
func (a UserApi) UpdateUser (Username string, Body User) (error) {

    _sling := sling.New().Put(a.basePath)

    // create path and map variables
    path := "/v2/user/{username}"
    path = strings.Replace(path, "{" + "username" + "}", fmt.Sprintf("%v", Username), -1)

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }

// body params
    _sling = _sling.BodyJSON(Body)



    _, err := _sling.ReceiveSuccess(nil)
    //fmt.Println("UpdateUser response: void, ", resp, err)
    return err
}
/**
 * Delete user
 * This can only be done by the logged in user.
 * @param Username The name that needs to be deleted
 * @return void
 */
//func (a UserApi) DeleteUser (Username string) (error) {
func (a UserApi) DeleteUser (Username string) (error) {

    _sling := sling.New().Delete(a.basePath)

    // create path and map variables
    path := "/v2/user/{username}"
    path = strings.Replace(path, "{" + "username" + "}", fmt.Sprintf("%v", Username), -1)

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }




    _, err := _sling.ReceiveSuccess(nil)
    //fmt.Println("DeleteUser response: void, ", resp, err)
    return err
}
