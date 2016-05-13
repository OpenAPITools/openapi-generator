import {expect} from 'chai';
import {PetApi, Pet, Category} from 'typescript-fetch-api';

describe('PetApi', () => {
  let api: PetApi;
  let fixture: Pet = createTestFixture();

  beforeEach(() => {
    api = new PetApi();
  });

  it('should add and delete Pet', () => {
    return api.addPet({ body: fixture }).then(() => {
    });
  });

  it('should get Pet by ID', () => {
      return api.getPetById({ petId: fixture.id }).then((result) => {
          return expect(result).to.deep.equal(fixture);
      });
  });

  it('should update Pet by ID', () => {
    return api.getPetById({ petId: fixture.id }).then( (result) => {
      result.name = 'newname';
      return api.updatePet({ body: result }).then(() => {
        return api.getPetById({ petId: fixture.id }).then( (result) => {
          return expect(result.name).to.deep.equal('newname');
        });
      });
    });
  });
  
  it('should delete Pet', () => {
    return api.deletePet({ petId: fixture.id });
  });

  it('should not contain deleted Pet', () => {
      return api.getPetById({ petId: fixture.id }).then((result) => {
          return expect(result).to.not.exist;
      }, (err) => {
        return expect(err).to.exist;
      });
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
