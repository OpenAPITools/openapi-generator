(function(factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['./ApiClient', './model/User', './model/Category', './model/Pet', './model/Tag', './model/Order', './api/UserApi', './api/PetApi', './api/StoreApi'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('./ApiClient'), require('./model/User'), require('./model/Category'), require('./model/Pet'), require('./model/Tag'), require('./model/Order'), require('./api/UserApi'), require('./api/PetApi'), require('./api/StoreApi'));
  }
}(function(ApiClient, User, Category, Pet, Tag, Order, UserApi, PetApi, StoreApi) {
  'use strict';

  return {
    ApiClient: ApiClient,
    User: User,
    Category: Category,
    Pet: Pet,
    Tag: Tag,
    Order: Order,
    UserApi: UserApi,
    PetApi: PetApi,
    StoreApi: StoreApi
  };
}));
