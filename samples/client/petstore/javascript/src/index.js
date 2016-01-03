if (typeof module === 'object' && module.exports) {
  var SwaggerPetstore = {};
  
  SwaggerPetstore.User = require('./model/User.js');
  
  SwaggerPetstore.Category = require('./model/Category.js');
  
  SwaggerPetstore.Pet = require('./model/Pet.js');
  
  SwaggerPetstore.Tag = require('./model/Tag.js');
  
  SwaggerPetstore.Order = require('./model/Order.js');
  
  
  SwaggerPetstore.UserApi = require('./api/UserApi.js');
  
  SwaggerPetstore.StoreApi = require('./api/StoreApi.js');
  
  SwaggerPetstore.PetApi = require('./api/PetApi.js');
  
  module.exports = SwaggerPetstore;
}