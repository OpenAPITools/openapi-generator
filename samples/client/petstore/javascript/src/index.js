(function(factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['./ApiClient', './model/Category', './model/InlineResponse200', './model/Model200Response', './model/ModelReturn', './model/Name', './model/Order', './model/Pet', './model/SpecialModelName', './model/Tag', './model/User', './api/PetApi', './api/StoreApi', './api/UserApi'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('./ApiClient'), require('./model/Category'), require('./model/InlineResponse200'), require('./model/Model200Response'), require('./model/ModelReturn'), require('./model/Name'), require('./model/Order'), require('./model/Pet'), require('./model/SpecialModelName'), require('./model/Tag'), require('./model/User'), require('./api/PetApi'), require('./api/StoreApi'), require('./api/UserApi'));
  }
}(function(ApiClient, Category, InlineResponse200, Model200Response, ModelReturn, Name, Order, Pet, SpecialModelName, Tag, User, PetApi, StoreApi, UserApi) {
  'use strict';

  return {
    ApiClient: ApiClient,
    Category: Category,
    InlineResponse200: InlineResponse200,
    Model200Response: Model200Response,
    ModelReturn: ModelReturn,
    Name: Name,
    Order: Order,
    Pet: Pet,
    SpecialModelName: SpecialModelName,
    Tag: Tag,
    User: User,
    PetApi: PetApi,
    StoreApi: StoreApi,
    UserApi: UserApi
  };
}));
