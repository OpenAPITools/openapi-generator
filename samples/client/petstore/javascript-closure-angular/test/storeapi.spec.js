goog.require('API.Client.StoreApi');

angular.module('StoreApi', [])
    .service('storeApi', API.Client.StoreApi)
    .value('StoreApiBasePath', 'https://example.com');

describe('API.Client.StoreApi', function() {
  beforeEach(module('ng', 'ngMock', 'StoreApi'));

  /** @type {!Object} */
  var $httpBackend;

  /** @type {!API.Client.PetAPI} */
  var api;

  /** @type {!Date} */
  fixedDate = new Date();

  /** @type {!API.Client.Order} */
  var sampleOrder = {
    id: 123,
    petId: 234,
    quantity: 1,
    shipDate: fixedDate,
    status: API.Client.Order.StatusEnum.placed,
    complete: false,
  };

  beforeEach(function() {
    inject(function($injector) {
      $httpBackend = $injector.get('$httpBackend');
      api = $injector.get('storeApi');
    })
  });

  it('should get the inventory', function() {
    $httpBackend.expectGET('https://example.com/store/inventory')
        .respond(200, 'ok');
    api.getInventory();
    $httpBackend.flush();
  });

  it('should place an order', function() {
    $httpBackend.expectPOST('https://example.com/store/order', sampleOrder)
        .respond(200, 'ok');
    api.placeOrder(sampleOrder);
    $httpBackend.flush();
  });

  it('should get an order by id', function() {
    $httpBackend.expectGET('https://example.com/store/order/345')
        .respond(200, 'ok');
    api.getOrderById(345);
    $httpBackend.flush();
  });

  it('should delete an order', function() {
    $httpBackend.expectDELETE('https://example.com/store/order/456')
        .respond(200, 'ok');
    api.deleteOrder(456);
    $httpBackend.flush();
  });

  afterEach(function() {
    $httpBackend.verifyNoOutstandingExpectation();
    $httpBackend.verifyNoOutstandingRequest();
  });
});
