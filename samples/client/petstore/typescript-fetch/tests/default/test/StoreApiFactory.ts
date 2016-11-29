import {expect} from 'chai';
import {StoreApiFactory} from 'typescript-fetch-api';

describe('StoreApiFactory', function() {

  function runSuite(description: string, requestOptions?: any): void {

    describe(description, () => {

      const requestOptions: any = {credentials: 'include', mode: 'cors'};

      it('should get inventory', function() {
        return StoreApiFactory().getInventory(requestOptions).then((result) => {
          expect(Object.keys(result)).to.not.be.empty;
        });
      });

    });
  }

  runSuite('without custom request options');

  runSuite('with custom request options', {
    credentials: 'include',
    mode: 'cors'
  });

});
