/* tslint:disable:no-unused-variable */

export class UserApi {
    private basePath = 'http://petstore.swagger.io/v2';

    constructor(private url: string, private username: string, private password: string, basePath?: string) {
        if (basePath) {
            this.basePath = basePath;
        }
    }

  
  public createUser (body: User ) : Promise<{ response: http.ClientResponse;  }> {
    var path = this.url + this.basePath + '/user';

    

    var queryParameters: any = {};
    var headers: any = {};

    

    

    

    var deferred = promise.defer<{ response: http.ClientResponse;  }>();

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

  
  public createUsersWithArrayInput (body: Array<User> ) : Promise<{ response: http.ClientResponse;  }> {
    var path = this.url + this.basePath + '/user/createWithArray';

    

    var queryParameters: any = {};
    var headers: any = {};

    

    

    

    var deferred = promise.defer<{ response: http.ClientResponse;  }>();

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

  
  public createUsersWithListInput (body: Array<User> ) : Promise<{ response: http.ClientResponse;  }> {
    var path = this.url + this.basePath + '/user/createWithList';

    

    var queryParameters: any = {};
    var headers: any = {};

    

    

    

    var deferred = promise.defer<{ response: http.ClientResponse;  }>();

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

  
  public loginUser (username: string, password: string ) : Promise<{ response: http.ClientResponse; body: string;  }> {
    var path = this.url + this.basePath + '/user/login';

    

    var queryParameters: any = {};
    var headers: any = {};

    

    if (username !== undefined) {
        queryParameters['username'] = username;
    }
    if (password !== undefined) {
        queryParameters['password'] = password;
    }
    

    

    var deferred = promise.defer<{ response: http.ClientResponse; body: string;  }>();

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

  
  public logoutUser ( ) : Promise<{ response: http.ClientResponse;  }> {
    var path = this.url + this.basePath + '/user/logout';

    

    var queryParameters: any = {};
    var headers: any = {};

    

    

    

    var deferred = promise.defer<{ response: http.ClientResponse;  }>();

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

  
  public getUserByName (username: string ) : Promise<{ response: http.ClientResponse; body: User;  }> {
    var path = this.url + this.basePath + '/user/{username}';

    
    path = path.replace('{' + 'username' + '}', String(username));
    

    var queryParameters: any = {};
    var headers: any = {};

    

    

    

    var deferred = promise.defer<{ response: http.ClientResponse; body: User;  }>();

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

  
  public updateUser (username: string, body: User ) : Promise<{ response: http.ClientResponse;  }> {
    var path = this.url + this.basePath + '/user/{username}';

    
    path = path.replace('{' + 'username' + '}', String(username));
    

    var queryParameters: any = {};
    var headers: any = {};

    

    

    

    var deferred = promise.defer<{ response: http.ClientResponse;  }>();

    request({
        method: 'PUT',
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

  
  public deleteUser (username: string ) : Promise<{ response: http.ClientResponse;  }> {
    var path = this.url + this.basePath + '/user/{username}';

    
    path = path.replace('{' + 'username' + '}', String(username));
    

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
