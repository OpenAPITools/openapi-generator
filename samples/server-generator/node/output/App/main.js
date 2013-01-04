var express = require("express")
 , url = require("url")
 , swagger = require("swagger-node-express")
 , db = false

var app = express();
app.use(express.bodyParser());

swagger.setAppHandler(app);  

// resources for the demo
var petApi = require("./apis/PetApi.js");
var storeApi = require("./apis/StoreApi.js");
var userApi = require("./apis/UserApi.js");
swagger.addModels(models)
  .addGET(petApi.getPetById)
  .addPOST(petApi.addPet)
  .addPUT(petApi.updatePet)
  .addGET(petApi.findPetsByStatus)
  .addGET(petApi.findPetsByTags)
  .addGET(storeApi.getOrderById)
  .addDELETE(storeApi.deleteOrder)
  .addPOST(storeApi.placeOrder)
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

