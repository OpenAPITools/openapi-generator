(function(factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['./ApiClient', './model/Order', './model/User', './model/Category', './model/Tag', './model/Pet', './api/UserApi', './api/StoreApi', './api/PetApi'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('./ApiClient'), require('./model/Order'), require('./model/User'), require('./model/Category'), require('./model/Tag'), require('./model/Pet'), require('./api/UserApi'), require('./api/StoreApi'), require('./api/PetApi'));
  }
}(function(ApiClient, Order, User, Category, Tag, Pet, UserApi, StoreApi, PetApi) {
  'use strict';

  return {
    ApiClient: ApiClient,
    Order: Order,
    User: User,
    Category: Category,
    Tag: Tag,
    Pet: Pet,
    UserApi: UserApi,
    StoreApi: StoreApi,
    PetApi: PetApi
  };
}));
