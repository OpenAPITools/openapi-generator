var express = require("express")
 , url = require("url")
 , swagger = require("./Common/node/swagger.js")
 , db = false

var app = express.createServer(
  function(req, res, next) { if (req.db === undefined) { req.db = db; } next(); });
app.use(express.bodyParser());
swagger.setAppHandler(app);  

// resources for the demo
var UserApi = require("./apis/UserApi.js");
var StoreApi = require("./apis/StoreApi.js");
var PetApi = require("./apis/PetApi.js");
swagger.addModels(models)
  .addPOST(UserApi.createUsersWithArrayInput)
  .addPOST(UserApi.createUser)
  .addPOST(UserApi.createUsersWithListInput)
  .addPUT(UserApi.updateUser)
  .addDELETE(UserApi.deleteUser)
  .addGET(UserApi.getUserByName)
  .addGET(UserApi.loginUser)
  .addGET(UserApi.logoutUser)
  .addGET(StoreApi.getOrderById)
  .addDELETE(StoreApi.deleteOrder)
  .addPOST(StoreApi.placeOrder)
  .addGET(PetApi.getPetById)
  .addPOST(PetApi.addPet)
  .addPUT(PetApi.updatePet)
  .addGET(PetApi.findPetsByStatus)
  .addGET(PetApi.findPetsByTags)
  ;

// configures the app
swagger.configure("http://localhost:8002", "0.1");

//  start the server
app.listen(8002);

