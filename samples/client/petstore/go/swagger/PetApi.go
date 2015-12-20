package swagger

import (
    "strings"
    "fmt"
//    "log"
    "github.com/dghubble/sling"
    "os"
)

type PetApi struct {
    basePath  string
    apiClient ApiClient
    sling *sling.Sling
}

func NewPetApi() *PetApi{
    return &PetApi {
        basePath:   "http://petstore.swagger.io/v2",
    }
}

func NewPetApiWithBasePath(basePath string) *PetApi{
    return &PetApi {
        basePath:   basePath,
    }
}

/**
 * Update an existing pet
 * 
 * @param body Pet object that needs to be added to the store
 * @return void
 */
func (a PetApi) updatePet (body Pet) (error) {
    

    _sling := a.sling.Put(a.basePath)

    // create path and map variables
    path := "/pet"
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string { "application/json","application/xml" }

    

    // body params
    _sling = _sling.BodyJSON(body)
    //b, _ := json.Marshal(body)
    //bodyParams["body"] = string(b)
    

    req, err := _sling.Request()

    /*response, err := a.apiClient.CallApi(a.basePath, path, "Put", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //} */

    //
    
    return err
}
/**
 * Add a new pet to the store
 * 
 * @param body Pet object that needs to be added to the store
 * @return void
 */
func (a PetApi) addPet (body Pet) (error) {
    

    _sling := a.sling.Post(a.basePath)

    // create path and map variables
    path := "/pet"
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string { "application/json","application/xml" }

    

    // body params
    _sling = _sling.BodyJSON(body)
    //b, _ := json.Marshal(body)
    //bodyParams["body"] = string(b)
    

    req, err := _sling.Request()

    /*response, err := a.apiClient.CallApi(a.basePath, path, "Post", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //} */

    //
    
    return err
}
/**
 * Finds Pets by status
 * Multiple status values can be provided with comma seperated strings
 * @param status Status values that need to be considered for filter
 * @return []Pet
 */
func (a PetApi) findPetsByStatus (status []string) ([]Pet, error) {
    

    _sling := a.sling.Get(a.basePath)

    // create path and map variables
    path := "/pet/findByStatus"
    

    _sling = _sling.Path(path)

    
    type QueryParams struct {
        status    []string `url:"status,omitempty"`
        
    }

    _sling = _sling.QueryStruct(&QueryParams{ status: status })
    

    

    //contentTypes := []string {  }

    

    

    req, err := _sling.Request()

    /*response, err := a.apiClient.CallApi(a.basePath, path, "Get", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //} */

    //ApiClient.Deserialize(response, "array", "Pet")
    return req, err
    
}
/**
 * Finds Pets by tags
 * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
 * @param tags Tags to filter by
 * @return []Pet
 */
func (a PetApi) findPetsByTags (tags []string) ([]Pet, error) {
    

    _sling := a.sling.Get(a.basePath)

    // create path and map variables
    path := "/pet/findByTags"
    

    _sling = _sling.Path(path)

    
    type QueryParams struct {
        tags    []string `url:"tags,omitempty"`
        
    }

    _sling = _sling.QueryStruct(&QueryParams{ tags: tags })
    

    

    //contentTypes := []string {  }

    

    

    req, err := _sling.Request()

    /*response, err := a.apiClient.CallApi(a.basePath, path, "Get", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //} */

    //ApiClient.Deserialize(response, "array", "Pet")
    return req, err
    
}
/**
 * Find pet by ID
 * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
 * @param petId ID of pet that needs to be fetched
 * @return Pet
 */
func (a PetApi) getPetById (petId int64) (Pet, error) {
    
    // verify the required parameter 'petId' is set
    //if petId == nil {
    //    return 0, fmt.Error("Missing the required parameter 'petId' when calling getPetById")
    //}
    

    _sling := a.sling.Get(a.basePath)

    // create path and map variables
    path := "/pet/{petId}"
    //path = regexp.MustCompile("{" + "petId" + "}").ReplaceAllString(path, "$1")
    //path = path.Replace("\\{" + "petId" + "\\}", ApiClient.EscapeString(petId))
    path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%b", petId), -1)
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string {  }

    

    

    req, err := _sling.Request()

    /*response, err := a.apiClient.CallApi(a.basePath, path, "Get", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //} */

    //ApiClient.Deserialize(response, "", "Pet")
    return req, err
    
}
/**
 * Updates a pet in the store with form data
 * 
 * @param petId ID of pet that needs to be updated
 * @param name Updated name of the pet
 * @param status Updated status of the pet
 * @return void
 */
func (a PetApi) updatePetWithForm (petId string, name string, status string) (error) {
    
    // verify the required parameter 'petId' is set
    //if petId == nil {
    //    return 0, fmt.Error("Missing the required parameter 'petId' when calling updatePetWithForm")
    //}
    

    _sling := a.sling.Post(a.basePath)

    // create path and map variables
    path := "/pet/{petId}"
    //path = regexp.MustCompile("{" + "petId" + "}").ReplaceAllString(path, "$1")
    //path = path.Replace("\\{" + "petId" + "\\}", ApiClient.EscapeString(petId))
    path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%b", petId), -1)
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string { "application/x-www-form-urlencoded" }

    
    type FormParams struct {
        name    string `url:"name,omitempty"`
        status    string `url:"status,omitempty"`
        
    }
    _sling = _sling.BodyForm(&FormParams{ name: name,status: status })
    

    

    req, err := _sling.Request()

    /*response, err := a.apiClient.CallApi(a.basePath, path, "Post", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //} */

    //
    
    return err
}
/**
 * Deletes a pet
 * 
 * @param petId Pet id to delete
 * @param apiKey 
 * @return void
 */
func (a PetApi) deletePet (petId int64, apiKey string) (error) {
    
    // verify the required parameter 'petId' is set
    //if petId == nil {
    //    return 0, fmt.Error("Missing the required parameter 'petId' when calling deletePet")
    //}
    

    _sling := a.sling.Delete(a.basePath)

    // create path and map variables
    path := "/pet/{petId}"
    //path = regexp.MustCompile("{" + "petId" + "}").ReplaceAllString(path, "$1")
    //path = path.Replace("\\{" + "petId" + "\\}", ApiClient.EscapeString(petId))
    path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%b", petId), -1)
    

    _sling = _sling.Path(path)

    

    // header params "api_key"
    _sling = _sling.Set("api_key", apiKey)
    

    //contentTypes := []string {  }

    

    

    req, err := _sling.Request()

    /*response, err := a.apiClient.CallApi(a.basePath, path, "Delete", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //} */

    //
    
    return err
}
/**
 * uploads an image
 * 
 * @param petId ID of pet to update
 * @param additionalMetadata Additional data to pass to server
 * @param file file to upload
 * @return void
 */
func (a PetApi) uploadFile (petId int64, additionalMetadata string, file *os.File) (error) {
    
    // verify the required parameter 'petId' is set
    //if petId == nil {
    //    return 0, fmt.Error("Missing the required parameter 'petId' when calling uploadFile")
    //}
    

    _sling := a.sling.Post(a.basePath)

    // create path and map variables
    path := "/pet/{petId}/uploadImage"
    //path = regexp.MustCompile("{" + "petId" + "}").ReplaceAllString(path, "$1")
    //path = path.Replace("\\{" + "petId" + "\\}", ApiClient.EscapeString(petId))
    path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%b", petId), -1)
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string { "multipart/form-data" }

    
    type FormParams struct {
        additionalMetadata    string `url:"additionalMetadata,omitempty"`
        file    *os.File `url:"file,omitempty"`
        
    }
    _sling = _sling.BodyForm(&FormParams{ additionalMetadata: additionalMetadata,file: file })
    

    

    req, err := _sling.Request()

    /*response, err := a.apiClient.CallApi(a.basePath, path, "Post", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //} */

    //
    
    return err
}
