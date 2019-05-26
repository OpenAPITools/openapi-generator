const { Middleware } = require('swagger-express-middleware');
const swaggerUI = require('swagger-ui-express');
const yamljs = require('yamljs');
const express = require('express');

class ExpressServer {
  constructor(port, openApiYaml) {
    this.port = port;
    this.app = express();
    this.middleware = new Middleware(this.app);
    this.schema = openApiYaml;
    this.setupMiddleware();
  }

  setupMiddleware() {
    this.middleware.init(this.schema, (err) => {
      if (err) {
        console.error(err);
      }
    });
    this.app.use(
      this.middleware.metadata(),
      this.middleware.CORS(),
      this.middleware.parseRequest(),
      this.middleware.validateRequest(),
      // ('/api-docs', swaggerUI.serve, swaggerUI.setup(this.swaggerJson)),
    );
    this.app.use('/api-docs', swaggerUI.serve, swaggerUI.setup(yamljs.load(this.schema)));
    this.app.get('/', (req, res) => {
      res.status(200);
      res.end('Hello World');
    });
  }

  addErrorHandler() {
    this.app.use('*', (req, res) => {
      res.status(404);
      res.send(JSON.stringify({ error: `path ${req.baseUrl} doesn't exist` }));
    });
    this.app.use((error, req, res) => {
      res.status(error.status || 500);
      res.type('json');
      res.json({ error: error.message });
    });
  }

  async launch() {
    return new Promise(
      async (resolve, reject) => {
        try {
          this.addErrorHandler();
          this.server = await this.app.listen(this.port, () => {
            console.log(`server running on port ${this.port}`);
            resolve(this.server);
          });
        } catch (error) {
          reject(error);
        }
      },
    );
  }

  async close() {
    if (this.server !== undefined) {
      await this.server.close();
      console.log(`Server on port ${this.port} shut down`);
    }
  }
}

module.exports = ExpressServer;
