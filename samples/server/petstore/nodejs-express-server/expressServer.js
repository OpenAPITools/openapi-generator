'use strict';
const Middleware= require("swagger-express-middleware").Middleware;
const swaggerUI = require("swagger-ui-express");
const yamljs=  require('yamljs');
const express = require("express");

class ExpressServer{
  constructor(port, openApiYaml){
    this.port = port;
    this.app = express();
    this.middleware = new Middleware(this.app);
    this.swaggerJson = yamljs.load(openApiYaml);
    this.setupMiddleware();
  }
  setupMiddleware(){
    this.middleware.init(this.swaggerJson, (err) => {
      this.app.use(
          this.middleware.metadata(),
          this.middleware.CORS(),
          this.middleware.files(
              {
                apiPath: "api",
                rawFilesPath: false
              }
          ),
          this.middleware.parseRequest(),
          this.middleware.validateRequest()
      );
    });
      this.app.use("/api-docs", swaggerUI.serve, swaggerUI.setup(this.swaggerJson));
      this.app.get("/", (req, res, next) => {
        res.status(200);
        res.end("Hello World");
      });
  }
  addErrorHandler(){
    this.app.use((error, req, res, next) => {
      const status = error.status || 500;
      res.status(status);
      res.type("json");
      res.end({error});
    });
  }
  async launch(){
    return new Promise(
        async (resolve, reject) => {
          try {
            this.addErrorHandler();
            await this.app.listen(this.port, () => {
              console.log(`server running on port ${this.port}`);
              resolve(this.server);
            });
          } catch (error) {
            reject(error);
          }
        });
    }
    async close(){
      if(this.server !== undefined){
        await this.server.close();
        console.log(`Server on port ${this.port} shut down`);
      }
  }
}
 module.exports = ExpressServer;