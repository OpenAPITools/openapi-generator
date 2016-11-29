import {expect} from 'chai';
import {PetApiFactory, Pet, Category} from 'typescript-fetch-api';

describe('PetApiFactory', () => {

  function runSuite(description: string, requestOptions?: any): void {

    describe(description, () => {

      const fixture: Pet = createTestFixture();

      it('should add and delete Pet', () => {
        return PetApiFactory().addPet({ body: fixture }, requestOptions).then(() => {
        });
      });

      it('should get Pet by ID', () => {
          return PetApiFactory().getPetById({ petId: fixture.id }, requestOptions).then((result) => {
              return expect(result).to.deep.equal(fixture);
          });
      });

      it('should update Pet by ID', () => {
        return PetApiFactory().getPetById({ petId: fixture.id }, requestOptions).then( (result) => {
          result.name = 'newname';
          return PetApiFactory().updatePet({ body: result }, requestOptions).then(() => {
            return PetApiFactory().getPetById({ petId: fixture.id }, requestOptions).then( (result) => {
              return expect(result.name).to.deep.equal('newname');
            });
          });
        });
      });

      it('should delete Pet', () => {
        return PetApiFactory().deletePet({ petId: fixture.id }, requestOptions);
      });

      it('should not contain deleted Pet', () => {
        return PetApiFactory().getPetById({ petId: fixture.id }, requestOptions).then((result) => {
          return expect(result).to.not.exist;
        }, (err) => {
          return expect(err).to.exist;
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

function createTestFixture(ts = Date.now()) {
  const category: Category = {
    'id': ts,
    'name': `category${ts}`,
  };

  const pet: Pet = {
    'id': ts,
    'name': `pet${ts}`,
    'category': category,
    'photoUrls': ['http://foo.bar.com/1', 'http://foo.bar.com/2'],
    'status': 'available',
    'tags': []
  };

  return pet;
};
