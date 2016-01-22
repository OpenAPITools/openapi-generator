package swagger

import (
    "strings"
    "fmt"
    "github.com/dghubble/sling"
    "os"
)

type PetApi struct {
    basePath  string
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
 * @param Body Pet object that needs to be added to the store
 * @return void
 */
//func (a PetApi) UpdatePet (Body Pet) (error) {
func (a PetApi) UpdatePet (Body Pet) (error) {

    _sling := sling.New().Put(a.basePath)

    // create path and map variables
    path := "/v2/pet"

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
    //fmt.Println("UpdatePet response: void, ", resp, err)
    return err
}
/**
 * Add a new pet to the store
 * 
 * @param Body Pet object that needs to be added to the store
 * @return void
 */
//func (a PetApi) AddPet (Body Pet) (error) {
func (a PetApi) AddPet (Body Pet) (error) {

    _sling := sling.New().Post(a.basePath)

    // create path and map variables
    path := "/v2/pet"

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
    //fmt.Println("AddPet response: void, ", resp, err)
    return err
}
/**
 * Finds Pets by status
 * Multiple status values can be provided with comma seperated strings
 * @param Status Status values that need to be considered for filter
 * @return []Pet
 */
//func (a PetApi) FindPetsByStatus (Status []string) ([]Pet, error) {
func (a PetApi) FindPetsByStatus (Status []string) ([]Pet, error) {

    _sling := sling.New().Get(a.basePath)

    // create path and map variables
    path := "/v2/pet/findByStatus"

    _sling = _sling.Path(path)

    type QueryParams struct {
        Status    []string `url:"status,omitempty"`
        
}
    _sling = _sling.QueryStruct(&QueryParams{ Status: Status })
    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }



    response := new([]Pet)
    _, err := _sling.ReceiveSuccess(response)
    //fmt.Println("FindPetsByStatus response: ", response, resp, err)
    return *response, err
}
/**
 * Finds Pets by tags
 * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
 * @param Tags Tags to filter by
 * @return []Pet
 */
//func (a PetApi) FindPetsByTags (Tags []string) ([]Pet, error) {
func (a PetApi) FindPetsByTags (Tags []string) ([]Pet, error) {

    _sling := sling.New().Get(a.basePath)

    // create path and map variables
    path := "/v2/pet/findByTags"

    _sling = _sling.Path(path)

    type QueryParams struct {
        Tags    []string `url:"tags,omitempty"`
        
}
    _sling = _sling.QueryStruct(&QueryParams{ Tags: Tags })
    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }



    response := new([]Pet)
    _, err := _sling.ReceiveSuccess(response)
    //fmt.Println("FindPetsByTags response: ", response, resp, err)
    return *response, err
}
/**
 * Find pet by ID
 * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
 * @param PetId ID of pet that needs to be fetched
 * @return Pet
 */
//func (a PetApi) GetPetById (PetId int64) (Pet, error) {
func (a PetApi) GetPetById (PetId int64) (Pet, error) {

    _sling := sling.New().Get(a.basePath)

    // create path and map variables
    path := "/v2/pet/{petId}"
    path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%v", PetId), -1)

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }



    response := new(Pet)
    _, err := _sling.ReceiveSuccess(response)
    //fmt.Println("GetPetById response: ", response, resp, err)
    return *response, err
}
/**
 * Updates a pet in the store with form data
 * 
 * @param PetId ID of pet that needs to be updated
 * @param Name Updated name of the pet
 * @param Status Updated status of the pet
 * @return void
 */
//func (a PetApi) UpdatePetWithForm (PetId string, Name string, Status string) (error) {
func (a PetApi) UpdatePetWithForm (PetId string, Name string, Status string) (error) {

    _sling := sling.New().Post(a.basePath)

    // create path and map variables
    path := "/v2/pet/{petId}"
    path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%v", PetId), -1)

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }

    type FormParams struct {
        Name    string `url:"name,omitempty"`
        Status    string `url:"status,omitempty"`
    }
    _sling = _sling.BodyForm(&FormParams{ Name: Name,Status: Status })



    _, err := _sling.ReceiveSuccess(nil)
    //fmt.Println("UpdatePetWithForm response: void, ", resp, err)
    return err
}
/**
 * Deletes a pet
 * 
 * @param PetId Pet id to delete
 * @param ApiKey 
 * @return void
 */
//func (a PetApi) DeletePet (PetId int64, ApiKey string) (error) {
func (a PetApi) DeletePet (PetId int64, ApiKey string) (error) {

    _sling := sling.New().Delete(a.basePath)

    // create path and map variables
    path := "/v2/pet/{petId}"
    path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%v", PetId), -1)

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }
    // header params "api_key"
    _sling = _sling.Set("api_key", ApiKey)




    _, err := _sling.ReceiveSuccess(nil)
    //fmt.Println("DeletePet response: void, ", resp, err)
    return err
}
/**
 * downloads an image
 * 
 * @return *os.File
 */
//func (a PetApi) DownloadFile () (*os.File, error) {
func (a PetApi) DownloadFile () (*os.File, error) {

    _sling := sling.New().Get(a.basePath)

    // create path and map variables
    path := "/v2/pet/{petId}/downloadImage"

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/octet-stream" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }



    response := new(*os.File)
    _, err := _sling.ReceiveSuccess(response)
    //fmt.Println("DownloadFile response: ", response, resp, err)
    return *response, err
}
/**
 * uploads an image
 * 
 * @param PetId ID of pet to update
 * @param AdditionalMetadata Additional data to pass to server
 * @param File file to upload
 * @return void
 */
//func (a PetApi) UploadFile (PetId int64, AdditionalMetadata string, File *os.File) (error) {
func (a PetApi) UploadFile (PetId int64, AdditionalMetadata string, File *os.File) (error) {

    _sling := sling.New().Post(a.basePath)

    // create path and map variables
    path := "/v2/pet/{petId}/uploadImage"
    path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%v", PetId), -1)

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }

    type FormParams struct {
        AdditionalMetadata    string `url:"additionalMetadata,omitempty"`
        File    *os.File `url:"file,omitempty"`
    }
    _sling = _sling.BodyForm(&FormParams{ AdditionalMetadata: AdditionalMetadata,File: File })



    _, err := _sling.ReceiveSuccess(nil)
    //fmt.Println("UploadFile response: void, ", resp, err)
    return err
}
