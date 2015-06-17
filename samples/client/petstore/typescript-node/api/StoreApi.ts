/* tslint:disable:no-unused-variable */

export class StoreApi {
    private basePath = 'http://petstore.swagger.io/v2';

    constructor(private url: string, private username: string, private password: string, basePath?: string) {
        if (basePath) {
            this.basePath = basePath;
        }
    }

  
  public getInventory ( ) : Promise<{ response: http.ClientResponse; body: map<String, number>;  }> {
    var path = this.url + this.basePath + '/store/inventory';

    

    var queryParameters: any = {};
    var headers: any = {};

    

    

    

    var deferred = promise.defer<{ response: http.ClientResponse; body: map<String, number>;  }>();

    request({
        method: 'GET',
        qs: queryParameters,
        uri: path,
        json: true,
        
        auth: {
            username: this.username, password: this.password
        }
    }, (error, response, body) => {
      if (error) {
        deferred.reject(error);
      } else {
        if (response.statusCode >= 200 && response.statusCode <= 299) {
          deferred.resolve({ response: response, body: body });
        } else {
          deferred.reject({ response: response, body: body });
        }
      }
    });

    return deferred.promise;
  }

  
  public placeOrder (body: Order ) : Promise<{ response: http.ClientResponse; body: Order;  }> {
    var path = this.url + this.basePath + '/store/order';

    

    var queryParameters: any = {};
    var headers: any = {};

    

    

    

    var deferred = promise.defer<{ response: http.ClientResponse; body: Order;  }>();

    request({
        method: 'POST',
        qs: queryParameters,
        uri: path,
        json: true,
        body: body,
        
        auth: {
            username: this.username, password: this.password
        }
    }, (error, response, body) => {
      if (error) {
        deferred.reject(error);
      } else {
        if (response.statusCode >= 200 && response.statusCode <= 299) {
          deferred.resolve({ response: response, body: body });
        } else {
          deferred.reject({ response: response, body: body });
        }
      }
    });

    return deferred.promise;
  }

  
  public getOrderById (orderId: string ) : Promise<{ response: http.ClientResponse; body: Order;  }> {
    var path = this.url + this.basePath + '/store/order/{orderId}';

    
    path = path.replace('{' + 'orderId' + '}', String(orderId));
    

    var queryParameters: any = {};
    var headers: any = {};

    

    

    

    var deferred = promise.defer<{ response: http.ClientResponse; body: Order;  }>();

    request({
        method: 'GET',
        qs: queryParameters,
        uri: path,
        json: true,
        
        auth: {
            username: this.username, password: this.password
        }
    }, (error, response, body) => {
      if (error) {
        deferred.reject(error);
      } else {
        if (response.statusCode >= 200 && response.statusCode <= 299) {
          deferred.resolve({ response: response, body: body });
        } else {
          deferred.reject({ response: response, body: body });
        }
      }
    });

    return deferred.promise;
  }

  
  public deleteOrder (orderId: string ) : Promise<{ response: http.ClientResponse;  }> {
    var path = this.url + this.basePath + '/store/order/{orderId}';

    
    path = path.replace('{' + 'orderId' + '}', String(orderId));
    

    var queryParameters: any = {};
    var headers: any = {};

    

    

    

    var deferred = promise.defer<{ response: http.ClientResponse;  }>();

    request({
        method: 'DELETE',
        qs: queryParameters,
        uri: path,
        json: true,
        
        auth: {
            username: this.username, password: this.password
        }
    }, (error, response, body) => {
      if (error) {
        deferred.reject(error);
      } else {
        if (response.statusCode >= 200 && response.statusCode <= 299) {
          deferred.resolve({ response: response, body: body });
        } else {
          deferred.reject({ response: response, body: body });
        }
      }
    });

    return deferred.promise;
  }

  
}
