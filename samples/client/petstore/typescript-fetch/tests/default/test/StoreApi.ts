import {expect} from 'chai';
import {StoreApi} from 'typescript-fetch-api';

describe('StoreApi', function() {
  let api: StoreApi;
  
  beforeEach(function() {
    api = new StoreApi();
  });

  it('should get inventory', function() {
    return api.getInventory().then((result) => {
      expect(Object.keys(result)).to.not.be.empty;
    });
  });

});

