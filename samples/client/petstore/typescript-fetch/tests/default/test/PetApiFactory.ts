import {expect} from 'chai';
import {PetApiFactory, Pet, Category} from 'typescript-fetch-api';
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

describe('PetApiFactory', () => {
  let fixture: Pet = createTestFixture();

  it('should add and delete Pet', () => {
    return PetApiFactory().addPet({ body: fixture }, config).then(() => {
    });
  });

  it('should get Pet by ID', () => {
      return PetApiFactory().getPetById({ petId: fixture.id }, config).then((result) => {
          return expect(result).to.deep.equal(fixture);
      });
  });

  it('should update Pet by ID', () => {
    return PetApiFactory().getPetById({ petId: fixture.id }, config).then( (result) => {
      result.name = 'newname';
      return PetApiFactory().updatePet({ body: result }, config).then(() => {
        return PetApiFactory().getPetById({ petId: fixture.id }, config).then( (result) => {
          return expect(result.name).to.deep.equal('newname');
        });
      });
    });
  });
  
  it('should delete Pet', () => {
    return PetApiFactory().deletePet({ petId: fixture.id }, config);
  });

  it('should not contain deleted Pet', () => {
      return PetApiFactory().getPetById({ petId: fixture.id }, config).then((result) => {
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
