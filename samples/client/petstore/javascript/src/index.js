(function(factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['./ApiClient', './model/Order', './model/SpecialModelName', './model/User', './model/Category', './model/ObjectReturn', './model/InlineResponse200', './model/Tag', './model/Pet', './api/UserApi', './api/StoreApi', './api/PetApi'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('./ApiClient'), require('./model/Order'), require('./model/SpecialModelName'), require('./model/User'), require('./model/Category'), require('./model/ObjectReturn'), require('./model/InlineResponse200'), require('./model/Tag'), require('./model/Pet'), require('./api/UserApi'), require('./api/StoreApi'), require('./api/PetApi'));
  }
}(function(ApiClient, Order, SpecialModelName, User, Category, ObjectReturn, InlineResponse200, Tag, Pet, UserApi, StoreApi, PetApi) {
  'use strict';

  return {
    ApiClient: ApiClient,
    Order: Order,
    SpecialModelName: SpecialModelName,
    User: User,
    Category: Category,
    ObjectReturn: ObjectReturn,
    InlineResponse200: InlineResponse200,
    Tag: Tag,
    Pet: Pet,
    UserApi: UserApi,
    StoreApi: StoreApi,
    PetApi: PetApi
  };
}));
