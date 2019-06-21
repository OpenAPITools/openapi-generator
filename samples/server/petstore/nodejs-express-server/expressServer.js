// const { Middleware } = require('swagger-express-middleware');
const swaggerUI = require('swagger-ui-express');
const yamljs = require('yamljs');
const express = require('express');
const cors = require('cors');
const cookieParser = require('cookie-parser');
const bodyParser = require('body-parser');
const { OpenApiValidator } = require('express-openapi-validator');
const swaggerRouter = require('./utils/swaggerRouter');


class ExpressServer {
  constructor(port, openApiYaml) {
    this.port = port;
    this.app = express();
    this.schema = openApiYaml;
    // this.middleware = new Middleware(this.app);
    this.setupMiddleware();
  }

  setupMiddleware() {
    this.app.use(cors());
    this.app.use(bodyParser.json());
    this.app.use(express.json());
    this.app.use(express.urlencoded({ extended: false }));
    this.app.use(cookieParser());
    this.app.get('/spec', express.static(this.schema));
    this.app.use('/api-docs', swaggerUI.serve, swaggerUI.setup(yamljs.load(this.schema)));
    this.app.get('/login-redirect', (req, res) => {
      res.status(200);
      res.json(req.query);
    });
    this.app.get('/oauth2-redirect.html', (req, res) => {
      res.status(200);
      res.json(req.query);
    });
    new OpenApiValidator({
      apiSpecPath: this.schema,
    }).install(this.app);
    this.app.use(swaggerRouter());
    // this.middleware.init(this.schema, (err) => {
    //   if (err) {
    //     console.error(err);
    //   }
    // });
    // this.app.use(
    //   // this.middleware.metadata(),
    //   // this.middleware.CORS(),
    //   // this.middleware.parseRequest(),
    //   // this.middleware.validateRequest(),
    //   // ('/api-docs', swaggerUI.serve, swaggerUI.setup(this.swaggerJson)),
    // );
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
    /**
     * suppressed eslint rule: The next variable is required here, even though it's not used.
      *
     ** */
    // eslint-disable-next-line no-unused-vars
    this.app.use((error, req, res, next) => {
      const errorResponse = error.error || error.errors || error.message || 'Unknown error';
      res.status(error.status || 500);
      res.type('json');
      res.json({ error: errorResponse });
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
