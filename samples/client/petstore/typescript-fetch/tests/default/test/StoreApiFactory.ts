import {expect} from 'chai';
import {StoreApiFactory} from 'typescript-fetch-api';
import {Configuration} from 'typescript-fetch-api/dist/configuration';

let config: Configuration;

before(function() {
  config = new Configuration();
  config.accessToken = "foobar";
  config.apiKey = {
    api_key: "foobar"
  };
  config.username = "foo";
  config.password = "bar";
});

describe('StoreApiFactory', function() {
  it('should get inventory', function() {
    return StoreApiFactory().getInventory(config).then((result) => {
      expect(Object.keys(result)).to.not.be.empty;
    });
  });

});

