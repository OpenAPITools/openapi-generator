goog.require('API.Client.UserApi');

angular.module('UserApi', [])
    .service('UserApi', API.Client.UserApi)
    .value('UserApiBasePath', 'https://example.com');

describe('API.Client.PetAPI', function() {
  beforeEach(module('ng', 'ngMock', 'UserApi'));

  /** @type {!Object} */
  var $httpBackend;

  /** @type {!API.Client.UserApi} */
  var api;

  /** @type {!API.Client.User} */
  var sampleUser = {
    id: 123,
    username: 'username',
    firstName: 'first',
    lastName: 'last',
    email: 'email@example.com',
    password: 'password',
    userStatus: 0,
  };

  beforeEach(function() {
    inject(function($injector) {
      $httpBackend = $injector.get('$httpBackend');
      api = $injector.get('UserApi');
    })
  });

  it('should create a user', function() {
    $httpBackend.expectPOST('https://example.com/user', sampleUser)
        .respond(200, '');
    api.createUser(sampleUser);
    $httpBackend.flush();
  });

  it('should create an array of users', function() {
    $httpBackend.expectPOST('https://example.com/user/createWithArray', [sampleUser])
        .respond(200, '');
    api.createUsersWithArrayInput([sampleUser]);
    $httpBackend.flush();
  });

  it('should create a list of users', function() {
    $httpBackend.expectPOST('https://example.com/user/createWithList', [sampleUser])
        .respond(200, '');
    api.createUsersWithListInput([sampleUser]);
    $httpBackend.flush();
  });

  it('should login a user', function() {
    $httpBackend.expectGET('https://example.com/user/login?password=password&username=username')
        .respond(200, '');
    api.loginUser('username', 'password');
    $httpBackend.flush();
  });

  it('should logout a user', function() {
    $httpBackend.expectGET('https://example.com/user/logout')
        .respond(200, '');
    api.logoutUser();
    $httpBackend.flush();
  });

  it('should get a user by username', function() {
    $httpBackend.expectGET('https://example.com/user/username')
        .respond(200, '');
    api.getUserByName('username');
    $httpBackend.flush();
  });

  afterEach(function() {
    $httpBackend.verifyNoOutstandingExpectation();
    $httpBackend.verifyNoOutstandingRequest();
  });
});

