const ExpressServer = require("./expressServer");
const config = require("./config");
let expressServer;
launchServer = async() =>{
  try{
    expressServer = new ExpressServer(config.URL_PORT, config.OPENAPI_YAML);
    await expressServer.launch();
    console.log("Express server running");

  }
  catch(error){
    if(expressServer !== undefined){
      expressServer.close();
    }
    console.log("Error occured. Server closed", error);
  }
};

launchServer();