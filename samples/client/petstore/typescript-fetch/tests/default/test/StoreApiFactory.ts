import {expect} from 'chai';
import {StoreApiFactory} from 'typescript-fetch-api';

describe('StoreApiFactory', function() {
  it('should get inventory', function() {
    return StoreApiFactory().getInventory().then((result) => {
      expect(Object.keys(result)).to.not.be.empty;
    });
  });

});

