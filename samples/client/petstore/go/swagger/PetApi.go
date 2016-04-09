package swagger


import (
    "strings"
    "fmt"
    "encoding/json"
    "errors"
    "github.com/dghubble/sling"

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
//func (a PetApi) AddPet (body Pet) (error) {
func (a PetApi) AddPet (body Pet) (error) {

    _sling := sling.New().Post(a.Configuration.BasePath)

    // create path and map variables
    path := "/v2/pets"


    _sling = _sling.Path(path)


    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }


// body params
    _sling = _sling.BodyJSON(body)



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
 * Deletes a pet
 * 
 * @param apiKey 
 * @param petId Pet id to delete
 * @return void
 */
//func (a PetApi) DeletePet (apiKey string, petId int64) (error) {
func (a PetApi) DeletePet (apiKey string, petId int64) (error) {

    _sling := sling.New().Delete(a.Configuration.BasePath)

    // create path and map variables
    path := "/v2/pets/{petId}"
    path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%v", petId), -1)


    _sling = _sling.Path(path)


    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }
    // header params "api_key"
    _sling = _sling.Set("api_key", apiKey)





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
 * Finds Pets by status
 * Multiple status values can be provided with comma seperated strings
 * @param status Status values that need to be considered for filter
 * @return []Pet
 */
//func (a PetApi) FindPetsByStatus (status []string) ([]Pet, error) {
func (a PetApi) FindPetsByStatus (status []string) ([]Pet, error) {

    _sling := sling.New().Get(a.Configuration.BasePath)

    // create path and map variables
    path := "/v2/pets/findByStatus"


    _sling = _sling.Path(path)

    type QueryParams struct {
        status    []string `url:"status,omitempty"`
        
}
    _sling = _sling.QueryStruct(&QueryParams{ status: status })

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }



  var successPayload = new([]Pet)

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
 * Finds Pets by tags
 * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
 * @param tags Tags to filter by
 * @return []Pet
 */
//func (a PetApi) FindPetsByTags (tags []string) ([]Pet, error) {
func (a PetApi) FindPetsByTags (tags []string) ([]Pet, error) {

    _sling := sling.New().Get(a.Configuration.BasePath)

    // create path and map variables
    path := "/v2/pets/findByTags"


    _sling = _sling.Path(path)

    type QueryParams struct {
        tags    []string `url:"tags,omitempty"`
        
}
    _sling = _sling.QueryStruct(&QueryParams{ tags: tags })

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }



  var successPayload = new([]Pet)

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
 * Find pet by ID
 * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
 * @param petId ID of pet that needs to be fetched
 * @return Pet
 */
//func (a PetApi) GetPetById (petId int64) (Pet, error) {
func (a PetApi) GetPetById (petId int64) (Pet, error) {

    _sling := sling.New().Get(a.Configuration.BasePath)

    // create path and map variables
    path := "/v2/pets/{petId}"
    path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%v", petId), -1)


    _sling = _sling.Path(path)


    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }



  var successPayload = new(Pet)

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
 * Update an existing pet
 * 
 * @param body Pet object that needs to be added to the store
 * @return void
 */
//func (a PetApi) UpdatePet (body Pet) (error) {
func (a PetApi) UpdatePet (body Pet) (error) {

    _sling := sling.New().Put(a.Configuration.BasePath)

    // create path and map variables
    path := "/v2/pets"


    _sling = _sling.Path(path)


    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }


// body params
    _sling = _sling.BodyJSON(body)



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
 * Updates a pet in the store with form data
 * 
 * @param petId ID of pet that needs to be updated
 * @param name Updated name of the pet
 * @param status Updated status of the pet
 * @return void
 */
//func (a PetApi) UpdatePetWithForm (petId string, name string, status string) (error) {
func (a PetApi) UpdatePetWithForm (petId string, name string, status string) (error) {

    _sling := sling.New().Post(a.Configuration.BasePath)

    // create path and map variables
    path := "/v2/pets/{petId}"
    path = strings.Replace(path, "{" + "petId" + "}", fmt.Sprintf("%v", petId), -1)


    _sling = _sling.Path(path)


    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }

    type FormParams struct {
        name    string `url:"name,omitempty"`
        status    string `url:"status,omitempty"`

    }
    _sling = _sling.BodyForm(&FormParams{ name: name,status: status })




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


