const path = require("path");
const describe = require("mocha").describe;
const chai = require("chai");
const chaiAsPromised = require("chai-as-promised");
const axios = require("axios");
const ExpressServer = require("../expressServer");

chai.use(chaiAsPromised);
const should = chai.should();

const config = {
    ROOT_DIR: path.join(__dirname, "../"),
    URL_PORT: 3009
    };
config.OPENAPI_YAML = path.join(config.ROOT_DIR, "api", "openapi.yaml");

describe("Server tests, checking launch, terminate, and various error messages", () =>{
   let expressServer;
   before(async ()=>{
       try{
           expressServer = new ExpressServer(config.URL_PORT, config.OPENAPI_YAML);
           await expressServer.launch();
           console.log("express server launched\n");
       }
       catch(error){
           console.log(error);
           throw(error);
       }
   });
   after(async() =>{
       if(expressServer !== undefined){
           await expressServer.close();
           console.log("express server closed");
       }
   });
   it("should launch express server successfully", async() =>{
      const indexResponse = await axios.get(`http://localhost:${config.URL_PORT}/`);
      indexResponse.status.should.equal(200, "Expecting a call to root directory of server to return 200 code");
   });
    it("should fail with a 404 on non-existing page", async() =>{
        try{
            const response = await axios.get(`http://localhost:${config.URL_PORT}/someRandomPage`);
            response.status.should.not.equal(200, "Expecting a 404, got 200");
        }
        catch(error){
            if(error.response !== undefined){
                error.response.status.should.equal(404, "expecting to receive a 404 on requesting a non-existing page");
            }
            console.log(error);
        }
    });
    it("should load api-doc", async() =>{
        const response = await axios.get(`http://localhost:${config.URL_PORT}/api-docs`);
        response.status.should.equal(200, "Expecting 200");
    });
});
