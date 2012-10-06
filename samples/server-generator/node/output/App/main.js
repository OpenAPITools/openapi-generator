var express = require("express")
 , url = require("url")
 , swagger = require("./Common/node/swagger.js")
 , db = false

var app = express.createServer(
  function(req, res, next) { if (req.db === undefined) { req.db = db; } next(); });
app.use(express.bodyParser());
swagger.setAppHandler(app);  

// resources for the demo
var storeApi = require("./apis/StoreApi.js");
var petApi = require("./apis/PetApi.js");
var userApi = require("./apis/UserApi.js");
swagger.addModels(models)
  .addGET(storeApi.getOrderById)
  .addDELETE(storeApi.deleteOrder)
  .addPOST(storeApi.placeOrder)
  .addGET(petApi.getPetById)
  .addPOST(petApi.addPet)
  .addPUT(petApi.updatePet)
  .addGET(petApi.findPetsByStatus)
  .addGET(petApi.findPetsByTags)
  .addPOST(userApi.createUsersWithArrayInput)
  .addPOST(userApi.createUser)
  .addPOST(userApi.createUsersWithListInput)
  .addPUT(userApi.updateUser)
  .addDELETE(userApi.deleteUser)
  .addGET(userApi.getUserByName)
  .addGET(userApi.loginUser)
  .addGET(userApi.logoutUser)
  ;

// configures the app
swagger.configure("http://localhost:8002", "0.1");

//  start the server
app.listen(8002);

