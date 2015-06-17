/* tslint:disable:no-unused-variable */

export class PetApi {
    private basePath = 'http://petstore.swagger.io/v2';

    constructor(private url: string, private username: string, private password: string, basePath?: string) {
        if (basePath) {
            this.basePath = basePath;
        }
    }

  
  public updatePet (body: Pet ) : Promise<{ response: http.ClientResponse;  }> {
    var path = this.url + this.basePath + '/pet';

    

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

  
  public addPet (body: Pet ) : Promise<{ response: http.ClientResponse;  }> {
    var path = this.url + this.basePath + '/pet';

    

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

  
  public findPetsByStatus (status: Array<string> ) : Promise<{ response: http.ClientResponse; body: Array<Pet>;  }> {
    var path = this.url + this.basePath + '/pet/findByStatus';

    

    var queryParameters: any = {};
    var headers: any = {};

    

    if (status !== undefined) {
        queryParameters['status'] = status;
    }
    

    

    var deferred = promise.defer<{ response: http.ClientResponse; body: Array<Pet>;  }>();

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

  
  public findPetsByTags (tags: Array<string> ) : Promise<{ response: http.ClientResponse; body: Array<Pet>;  }> {
    var path = this.url + this.basePath + '/pet/findByTags';

    

    var queryParameters: any = {};
    var headers: any = {};

    

    if (tags !== undefined) {
        queryParameters['tags'] = tags;
    }
    

    

    var deferred = promise.defer<{ response: http.ClientResponse; body: Array<Pet>;  }>();

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

  
  public getPetById (petId: number ) : Promise<{ response: http.ClientResponse; body: Pet;  }> {
    var path = this.url + this.basePath + '/pet/{petId}';

    
    path = path.replace('{' + 'petId' + '}', String(petId));
    

    var queryParameters: any = {};
    var headers: any = {};

    

    

    

    var deferred = promise.defer<{ response: http.ClientResponse; body: Pet;  }>();

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

  
  public updatePetWithForm (petId: string, name: string, status: string ) : Promise<{ response: http.ClientResponse;  }> {
    var path = this.url + this.basePath + '/pet/{petId}';

    
    path = path.replace('{' + 'petId' + '}', String(petId));
    

    var queryParameters: any = {};
    var headers: any = {};

    

    

    

    var deferred = promise.defer<{ response: http.ClientResponse;  }>();

    request({
        method: 'POST',
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

  
  public deletePet (apiKey: string, petId: number ) : Promise<{ response: http.ClientResponse;  }> {
    var path = this.url + this.basePath + '/pet/{petId}';

    
    path = path.replace('{' + 'petId' + '}', String(petId));
    

    var queryParameters: any = {};
    var headers: any = {};

    

    

    headerParams['apiKey'] = apiKey;
    

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

  
  public uploadFile (petId: number, additionalMetadata: string, file: file ) : Promise<{ response: http.ClientResponse;  }> {
    var path = this.url + this.basePath + '/pet/{petId}/uploadImage';

    
    path = path.replace('{' + 'petId' + '}', String(petId));
    

    var queryParameters: any = {};
    var headers: any = {};

    

    

    

    var deferred = promise.defer<{ response: http.ClientResponse;  }>();

    request({
        method: 'POST',
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
