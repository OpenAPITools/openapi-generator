if (typeof module === 'object' && module.exports) {
  var SwaggerPetstore = {};
  
  SwaggerPetstore.User = require('./model/User.js');
  
  SwaggerPetstore.Category = require('./model/Category.js');
  
  SwaggerPetstore.Pet = require('./model/Pet.js');
  
  SwaggerPetstore.Tag = require('./model/Tag.js');
  
  SwaggerPetstore.Order = require('./model/Order.js');
  
  
  SwaggerPetstore.User = require('./api/User.js');
  
  SwaggerPetstore.Store = require('./api/Store.js');
  
  SwaggerPetstore.Pet = require('./api/Pet.js');
  
  module.exports = SwaggerPetstore;
}