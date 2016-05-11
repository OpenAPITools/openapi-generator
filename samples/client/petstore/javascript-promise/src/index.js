(function(factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['ApiClient', 'model/Animal', 'model/AnimalFarm', 'model/ApiResponse', 'model/Cat', 'model/Category', 'model/Dog', 'model/EnumClass', 'model/EnumTest', 'model/FormatTest', 'model/Model200Response', 'model/ModelReturn', 'model/Name', 'model/Order', 'model/Pet', 'model/SpecialModelName', 'model/Tag', 'model/User', 'api/FakeApi', 'api/PetApi', 'api/StoreApi', 'api/UserApi'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('./ApiClient'), require('./model/Animal'), require('./model/AnimalFarm'), require('./model/ApiResponse'), require('./model/Cat'), require('./model/Category'), require('./model/Dog'), require('./model/EnumClass'), require('./model/EnumTest'), require('./model/FormatTest'), require('./model/Model200Response'), require('./model/ModelReturn'), require('./model/Name'), require('./model/Order'), require('./model/Pet'), require('./model/SpecialModelName'), require('./model/Tag'), require('./model/User'), require('./api/FakeApi'), require('./api/PetApi'), require('./api/StoreApi'), require('./api/UserApi'));
  }
}(function(ApiClient, Animal, AnimalFarm, ApiResponse, Cat, Category, Dog, EnumClass, EnumTest, FormatTest, Model200Response, ModelReturn, Name, Order, Pet, SpecialModelName, Tag, User, FakeApi, PetApi, StoreApi, UserApi) {
  'use strict';

  /**
   * This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose..<br>
   * The <code>index</code> module provides access to constructors for all the classes which comprise the public API.
   * <p>
   * An AMD (recommended!) or CommonJS application will generally do something equivalent to the following:
   * <pre>
   * var SwaggerPetstore = require('index'); // See note below*.
   * var xxxSvc = new SwaggerPetstore.XxxApi(); // Allocate the API class we're going to use.
   * var yyyModel = new SwaggerPetstore.Yyy(); // Construct a model instance.
   * yyyModel.someProperty = 'someValue';
   * ...
   * var zzz = xxxSvc.doSomething(yyyModel); // Invoke the service.
   * ...
   * </pre>
   * <em>*NOTE: For a top-level AMD script, use require(['index'], function(){...})
   * and put the application logic within the callback function.</em>
   * </p>
   * <p>
   * A non-AMD browser application (discouraged) might do something like this:
   * <pre>
   * var xxxSvc = new SwaggerPetstore.XxxApi(); // Allocate the API class we're going to use.
   * var yyy = new SwaggerPetstore.Yyy(); // Construct a model instance.
   * yyyModel.someProperty = 'someValue';
   * ...
   * var zzz = xxxSvc.doSomething(yyyModel); // Invoke the service.
   * ...
   * </pre>
   * </p>
   * @module index
   * @version 1.0.0
   */
  var exports = {
    /**
     * The ApiClient constructor.
     * @property {module:ApiClient}
     */
    ApiClient: ApiClient,
    /**
     * The Animal model constructor.
     * @property {module:model/Animal}
     */
    Animal: Animal,
    /**
     * The AnimalFarm model constructor.
     * @property {module:model/AnimalFarm}
     */
    AnimalFarm: AnimalFarm,
    /**
     * The ApiResponse model constructor.
     * @property {module:model/ApiResponse}
     */
    ApiResponse: ApiResponse,
    /**
     * The Cat model constructor.
     * @property {module:model/Cat}
     */
    Cat: Cat,
    /**
     * The Category model constructor.
     * @property {module:model/Category}
     */
    Category: Category,
    /**
     * The Dog model constructor.
     * @property {module:model/Dog}
     */
    Dog: Dog,
    /**
     * The EnumClass model constructor.
     * @property {module:model/EnumClass}
     */
    EnumClass: EnumClass,
    /**
     * The EnumTest model constructor.
     * @property {module:model/EnumTest}
     */
    EnumTest: EnumTest,
    /**
     * The FormatTest model constructor.
     * @property {module:model/FormatTest}
     */
    FormatTest: FormatTest,
    /**
     * The Model200Response model constructor.
     * @property {module:model/Model200Response}
     */
    Model200Response: Model200Response,
    /**
     * The ModelReturn model constructor.
     * @property {module:model/ModelReturn}
     */
    ModelReturn: ModelReturn,
    /**
     * The Name model constructor.
     * @property {module:model/Name}
     */
    Name: Name,
    /**
     * The Order model constructor.
     * @property {module:model/Order}
     */
    Order: Order,
    /**
     * The Pet model constructor.
     * @property {module:model/Pet}
     */
    Pet: Pet,
    /**
     * The SpecialModelName model constructor.
     * @property {module:model/SpecialModelName}
     */
    SpecialModelName: SpecialModelName,
    /**
     * The Tag model constructor.
     * @property {module:model/Tag}
     */
    Tag: Tag,
    /**
     * The User model constructor.
     * @property {module:model/User}
     */
    User: User,
    /**
     * The FakeApi service constructor.
     * @property {module:api/FakeApi}
     */
    FakeApi: FakeApi,
    /**
     * The PetApi service constructor.
     * @property {module:api/PetApi}
     */
    PetApi: PetApi,
    /**
     * The StoreApi service constructor.
     * @property {module:api/StoreApi}
     */
    StoreApi: StoreApi,
    /**
     * The UserApi service constructor.
     * @property {module:api/UserApi}
     */
    UserApi: UserApi
  };

  return exports;
}));
