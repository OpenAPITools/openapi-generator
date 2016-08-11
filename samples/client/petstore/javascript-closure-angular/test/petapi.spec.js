goog.require('API.Client.PetApi');

angular.module('PetApi', [])
    .service('petApi', API.Client.PetApi)
    .value('PetApiBasePath', 'https://example.com');

describe('API.Client.PetAPI', function() {
  beforeEach(module('ng', 'ngMock', 'PetApi'));

  /** @type {!Object} */
  var $httpBackend;

  /** @type {!API.Client.PetAPI} */
  var api;

  /** @type {!API.Client.Category} */
  var sampleCategory = {
    id: 345,
    name: 'categoryname',
  };

  /** @type {!API.Client.Pet} */
  var samplePet = {
    id: 123,
    category: sampleCategory,
    name: 'petname',
    photoUrls: [],
    tags: [sampleTag],
    status: API.Client.Pet.StatusEnum.available,
  };

  /** @type {!API.Client.Tag} */
  var sampleTag = {
    id: 345,
    name: 'categoryname',
  };

  beforeEach(function() {
    inject(function($injector) {
      $httpBackend = $injector.get('$httpBackend');
      api = $injector.get('petApi');
    })
  });

  it('should update pets', function() {
    $httpBackend.expectPUT('https://example.com/pet', samplePet)
        .respond(200, '');
    api.updatePet(samplePet);
    $httpBackend.flush();
  });

  it('should add a pet', function() {
    $httpBackend.expectPOST('https://example.com/pet', samplePet)
        .respond(200, '');
    api.addPet(samplePet);
    $httpBackend.flush();
  });

  it('should find pets by status', function() {
    $httpBackend.expectGET('https://example.com/pet/findByStatus?status=sold')
        .respond(200, '');
    api.findPetsByStatus(API.Client.Pet.StatusEnum.sold);
    $httpBackend.flush();
  });

  it('should find a pet by tag', function() {
    $httpBackend.expectGET('https://example.com/pet/findByTags?tags=%7B%22id%22:345,%22name%22:%22categoryname%22%7D')
        .respond(200, '');
    api.findPetsByTags(sampleTag);
    $httpBackend.flush();
  });

  it('should get pet by id', function() {
    $httpBackend.expectGET('https://example.com/pet/789')
        .respond(200, '');
    api.getPetById(789);
    $httpBackend.flush();
  });

  it('should update pet with form', function() {
    $httpBackend.expectPOST('https://example.com/pet/890', "name=newname&status=pending")
        .respond(200, '');
    api.updatePetWithForm(890, 'newname', API.Client.Pet.StatusEnum.pending);
    $httpBackend.flush();
  });

  it('should delete a pet', function() {
    $httpBackend.expectDELETE('https://example.com/pet/234')
        .respond(200, '');
    api.deletePet(234);
    $httpBackend.flush();
  });

  afterEach(function() {
    $httpBackend.verifyNoOutstandingExpectation();
    $httpBackend.verifyNoOutstandingRequest();
  });
});
