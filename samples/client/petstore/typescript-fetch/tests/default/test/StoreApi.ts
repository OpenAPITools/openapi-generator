import {expect} from 'chai';
import * as Swagger from 'typescript-fetch-api';

describe('StoreApi', function() {
  let api: Swagger.StoreApi;
  
  beforeEach(function() {
    api = new Swagger.StoreApi();
  });

  it('should get inventory', function() {
    return api.getInventory().then((result) => {
      expect(Object.keys(result)).to.not.be.empty;
    });
  });

});

