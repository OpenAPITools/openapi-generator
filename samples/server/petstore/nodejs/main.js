var express = require("express")
 , url = require("url")
 , cors = require("cors")
 , app = express()
 , swagger = require("swagger-node-express")
 , db = false


var corsOptions = {
  credentials: true,
  origin: function(origin,callback) {
    if(origin===undefined) {
      callback(null,false);
    } else {
      callback(null,true);
    }
  }
};

app.use(express.json());
app.use(express.urlencoded());
app.use(cors(corsOptions));

var subpath = express();

app.use("/v2", subpath);

swagger.setAppHandler(subpath);

swagger.configureSwaggerPaths("", "api-docs", "")

var models = require("./app/models.js");

var UserApi = require("./app/apis/UserApi.js");
var StoreApi = require("./app/apis/StoreApi.js");
var PetApi = require("./app/apis/PetApi.js");

swagger.addModels(models)
  .addPOST(UserApi.createUser)
  .addPOST(UserApi.createUsersWithArrayInput)
  .addPOST(UserApi.createUsersWithListInput)
  .addGET(UserApi.loginUser)
  .addGET(UserApi.logoutUser)
  .addGET(UserApi.getUserByName)
  .addPUT(UserApi.updateUser)
  .addDELETE(UserApi.deleteUser)
  .addGET(StoreApi.getInventory)
  .addPOST(StoreApi.placeOrder)
  .addGET(StoreApi.getOrderById)
  .addDELETE(StoreApi.deleteOrder)
  .addPUT(PetApi.updatePet)
  .addPOST(PetApi.addPet)
  .addGET(PetApi.findPetsByStatus)
  .addGET(PetApi.findPetsByTags)
  .addGET(PetApi.getPetById)
  .addPOST(PetApi.updatePetWithForm)
  .addDELETE(PetApi.deletePet)
  .addPOST(PetApi.uploadFile)
  ;

// configures the app
swagger.configure("http://localhost:8002/v2", "0.1");

//  start the server
app.listen(8002);
