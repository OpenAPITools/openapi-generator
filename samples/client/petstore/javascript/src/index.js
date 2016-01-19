(function(factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['./ApiClient', './model/User', './model/Category', './model/Pet', './model/Tag', './model/Order', './api/UserApi', './api/StoreApi', './api/PetApi'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('./ApiClient.js'), require('./model/User.js'), require('./model/Category.js'), require('./model/Pet.js'), require('./model/Tag.js'), require('./model/Order.js'), require('./api/UserApi.js'), require('./api/StoreApi.js'), require('./api/PetApi.js'));
  }
}(function(ApiClient, User, Category, Pet, Tag, Order, UserApi, StoreApi, PetApi) {
  'use strict';

  return {
    ApiClient: ApiClient,
    User: User,
    Category: Category,
    Pet: Pet,
    Tag: Tag,
    Order: Order,
    UserApi: UserApi,
    StoreApi: StoreApi,
    PetApi: PetApi
  };
}));
