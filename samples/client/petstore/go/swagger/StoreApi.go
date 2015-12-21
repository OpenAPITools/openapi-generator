package swagger

import (
//    "encoding/json"
    "strings"
    "fmt"
//    "log"
    "github.com/dghubble/sling"
)

type StoreApi struct {
    basePath  string
    apiClient ApiClient
    sling *sling.Sling
}

func NewStoreApi() *StoreApi{
    return &StoreApi {
        basePath:   "http://petstore.swagger.io/v2",
    }
}

func NewStoreApiWithBasePath(basePath string) *StoreApi{
    return &StoreApi {
        basePath:   basePath,
    }
}

/**
 * Returns pet inventories by status
 * Returns a map of status codes to quantities
 * @return map[string]int32
 */
//func (a StoreApi) getInventory () (map[string]int32, error) {
func (a StoreApi) getInventory () (map[string]int32, error) {

    _sling := a.sling.Get(a.basePath)

    // create path and map variables
    path := "/store/inventory"
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string {  }

    

    

    
    var response map[string]int32
    resp, err := _sling.ReceiveSuccess(response)
    fmt.Println("getInventory response: ", response, resp, err)
    return response, err
    

    

    //response, err := a.apiClient.CallApi(a.basePath, path, "Get", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //}

    //ApiClient.Deserialize(response, "map", "map")

    //var response map[string]int32
    //err = json.Unmarshal([]byte(req), &response)
    //return response, err
    //
}
/**
 * Place an order for a pet
 * 
 * @param body order placed for purchasing the pet
 * @return Order
 */
//func (a StoreApi) placeOrder (body Order) (Order, error) {
func (a StoreApi) placeOrder (body Order) (Order, error) {

    _sling := a.sling.Post(a.basePath)

    // create path and map variables
    path := "/store/order"
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string {  }

    

    // body params
    _sling = _sling.BodyJSON(body)
    //b, _ := json.Marshal(body)
    //bodyParams["body"] = string(b)
    

    
    var response Order
    resp, err := _sling.ReceiveSuccess(response)
    fmt.Println("placeOrder response: ", response, resp, err)
    return response, err
    

    

    //response, err := a.apiClient.CallApi(a.basePath, path, "Post", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //}

    //ApiClient.Deserialize(response, "", "Order")

    //var response Order
    //err = json.Unmarshal([]byte(req), &response)
    //return response, err
    //
}
/**
 * Find purchase order by ID
 * For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
 * @param orderId ID of pet that needs to be fetched
 * @return Order
 */
//func (a StoreApi) getOrderById (orderId string) (Order, error) {
func (a StoreApi) getOrderById (orderId string) (Order, error) {

    _sling := a.sling.Get(a.basePath)

    // create path and map variables
    path := "/store/order/{orderId}"
    //path = regexp.MustCompile("{" + "orderId" + "}").ReplaceAllString(path, "$1")
    //path = path.Replace("\\{" + "orderId" + "\\}", ApiClient.EscapeString(orderId))
    path = strings.Replace(path, "{" + "orderId" + "}", fmt.Sprintf("%b", orderId), -1)
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string {  }

    

    

    
    var response Order
    resp, err := _sling.ReceiveSuccess(response)
    fmt.Println("getOrderById response: ", response, resp, err)
    return response, err
    

    

    //response, err := a.apiClient.CallApi(a.basePath, path, "Get", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //}

    //ApiClient.Deserialize(response, "", "Order")

    //var response Order
    //err = json.Unmarshal([]byte(req), &response)
    //return response, err
    //
}
/**
 * Delete purchase order by ID
 * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
 * @param orderId ID of the order that needs to be deleted
 * @return void
 */
//func (a StoreApi) deleteOrder (orderId string) (error) {
func (a StoreApi) deleteOrder (orderId string) (error) {

    _sling := a.sling.Delete(a.basePath)

    // create path and map variables
    path := "/store/order/{orderId}"
    //path = regexp.MustCompile("{" + "orderId" + "}").ReplaceAllString(path, "$1")
    //path = path.Replace("\\{" + "orderId" + "\\}", ApiClient.EscapeString(orderId))
    path = strings.Replace(path, "{" + "orderId" + "}", fmt.Sprintf("%b", orderId), -1)
    

    _sling = _sling.Path(path)

    

    

    //contentTypes := []string {  }

    

    

    
    resp, err := _sling.Request()
    fmt.Println("deleteOrder response: void, ", resp, err)
    return err
    

    

    //response, err := a.apiClient.CallApi(a.basePath, path, "Delete", queryParams, headerParams, formParams, fileParams, bodyParams, contentTypes)
    //if err != nil {
    //    log.Fatal(err)
    //}

    //

    //
    //return err
}
