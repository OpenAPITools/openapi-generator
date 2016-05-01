(function(factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
<<<<<<< HEAD
    define(['./ApiClient', './model/Category', './model/Order', './model/Pet', './model/Tag', './model/User', './api/PetApi', './api/StoreApi', './api/UserApi'], factory);
=======
    define(['ApiClient', 'model/Category', 'model/InlineResponse200', 'model/Model200Response', 'model/ModelReturn', 'model/Name', 'model/Order', 'model/Pet', 'model/SpecialModelName', 'model/Tag', 'model/User', 'api/PetApi', 'api/StoreApi', 'api/UserApi'], factory);
>>>>>>> 7dfddd449ddc2ae8e7e35b6d5ab7fc10e52bc93d
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('./ApiClient'), require('./model/Category'), require('./model/Order'), require('./model/Pet'), require('./model/Tag'), require('./model/User'), require('./api/PetApi'), require('./api/StoreApi'), require('./api/UserApi'));
  }
}(function(ApiClient, Category, Order, Pet, Tag, User, PetApi, StoreApi, UserApi) {
  'use strict';

  /**
   * This is a sample server Petstore server.  You can find out more about Swagger at &lt;a href&#x3D;\&quot;http://swagger.io\&quot;&gt;http://swagger.io&lt;/a&gt; or on irc.freenode.net, #swagger.  For this sample, you can use the api key \&quot;special-key\&quot; to test the authorization filters.<br>
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
     * The Category model constructor.
     * @property {module:model/Category}
     */
    Category: Category,
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
