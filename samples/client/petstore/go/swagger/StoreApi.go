package swagger

import (
    "strings"
    "fmt"
    "github.com/dghubble/sling"
)

type StoreApi struct {
    basePath  string
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
//func (a StoreApi) GetInventory () (map[string]int32, error) {
func (a StoreApi) GetInventory () (map[string]int32, error) {

    _sling := sling.New().Get(a.basePath)

    // create path and map variables
    path := "/v2/store/inventory"

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }



    response := new(map[string]int32)
    _, err := _sling.ReceiveSuccess(response)
    //fmt.Println("GetInventory response: ", response, resp, err)
    return *response, err
}
/**
 * Place an order for a pet
 * 
 * @param Body order placed for purchasing the pet
 * @return Order
 */
//func (a StoreApi) PlaceOrder (Body Order) (Order, error) {
func (a StoreApi) PlaceOrder (Body Order) (Order, error) {

    _sling := sling.New().Post(a.basePath)

    // create path and map variables
    path := "/v2/store/order"

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }

// body params
    _sling = _sling.BodyJSON(Body)


    response := new(Order)
    _, err := _sling.ReceiveSuccess(response)
    //fmt.Println("PlaceOrder response: ", response, resp, err)
    return *response, err
}
/**
 * Find purchase order by ID
 * For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
 * @param OrderId ID of pet that needs to be fetched
 * @return Order
 */
//func (a StoreApi) GetOrderById (OrderId string) (Order, error) {
func (a StoreApi) GetOrderById (OrderId string) (Order, error) {

    _sling := sling.New().Get(a.basePath)

    // create path and map variables
    path := "/v2/store/order/{orderId}"
    path = strings.Replace(path, "{" + "orderId" + "}", fmt.Sprintf("%v", OrderId), -1)

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }



    response := new(Order)
    _, err := _sling.ReceiveSuccess(response)
    //fmt.Println("GetOrderById response: ", response, resp, err)
    return *response, err
}
/**
 * Delete purchase order by ID
 * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
 * @param OrderId ID of the order that needs to be deleted
 * @return void
 */
//func (a StoreApi) DeleteOrder (OrderId string) (error) {
func (a StoreApi) DeleteOrder (OrderId string) (error) {

    _sling := sling.New().Delete(a.basePath)

    // create path and map variables
    path := "/v2/store/order/{orderId}"
    path = strings.Replace(path, "{" + "orderId" + "}", fmt.Sprintf("%v", OrderId), -1)

    _sling = _sling.Path(path)

    // accept header
    accepts := []string { "application/json", "application/xml" }
    for key := range accepts {
        _sling = _sling.Set("Accept", accepts[key])
        break // only use the first Accept
    }




    _, err := _sling.ReceiveSuccess(nil)
    //fmt.Println("DeleteOrder response: void, ", resp, err)
    return err
}
