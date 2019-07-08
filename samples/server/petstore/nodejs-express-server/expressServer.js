// const { Middleware } = require('swagger-express-middleware');
const swaggerUI = require('swagger-ui-express');
const yamljs = require('yamljs');
const express = require('express');
const cors = require('cors');
const cookieParser = require('cookie-parser');
const bodyParser = require('body-parser');
const { OpenApiValidator } = require('express-openapi-validator');
// const pathToRegexp = require('path-to-regexp');
const openapiRouter = require('./utils/openapiRouter');
const logger = require('./logger');

class ExpressServer {
  constructor(port, openApiYaml) {
    this.port = port;
    this.app = express();
    this.openApiPath = openApiYaml;
    this.schema = yamljs.load(openApiYaml);
    // this.middleware = new Middleware(this.app);
    this.setupMiddleware();
  }

  // handleIncomingMedia(request) {
  //   console.log(this.apiValidator);
  //   const incomingMedia = request.headers['content-type'].split(';')[0];
  //   logger.info(`checking access for media type ${incomingMedia}`);
  //   const currentServer = this.schema.servers.find(
  //     server => server.url.indexOf(request.headers.host) > -1,
  //   );
  //   const currentServerUrl = currentServer.url.substr(currentServer.url.indexOf('://') + 3);
  //   // const path = `${request.headers.host}${request.originalUrl}`;
  //   const requestPath = `${request.headers.host}${request.originalUrl}`.substr(currentServerUrl.length);
  //   this.allowedMedia.forEach((permissions, path) => {
  //     console.log(path, permissions);
  //     const keys = [];
  //     const regex = pathToRegexp(path, keys);
  //     const matches = regex.exec(requestPath);
  //     console.log(matches);
  //   });
  //   // this.allowedMedia.find((instance) => {
  //   //   const keys = [];
  //   //   const regex = pathToRegexp(Object.keys(instance)[0], keys);
  //   //   return instance.path === regex;
  //   // });
  //
  //   logger.info(`analysing incoming media. content type: ${request.headers['content-type']}`); '';
  //   logger.info(this.schema.paths[request.url]);
  //   logger.info(this.schema);
  // }

  setupMiddleware() {
    // this.setupAllowedMedia();
    this.app.use(cors());
    this.app.use(bodyParser.json());
    this.app.use(express.json());
    this.app.use(express.urlencoded({ extended: false }));
    this.app.use(cookieParser());
    // this.app.use(bodyParser.raw({
    //   type: (req => this.handleIncomingMedia(req)),
    // }));
    this.app.get('/spec', express.static(this.openApiPath));
    this.app.use('/api-docs', swaggerUI.serve, swaggerUI.setup(this.schema));
    this.app.get('/login-redirect', (req, res) => {
      res.status(200);
      res.json(req.query);
    });
    this.app.get('/oauth2-redirect.html', (req, res) => {
      res.status(200);
      res.json(req.query);
    });
    new OpenApiValidator({
      apiSpecPath: this.openApiPath,
    }).install(this.app);
    this.app.use(openapiRouter());
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

  setupAllowedMedia() {
    this.allowedMedia = new Map();
    logger.info('Setting up allowed media types according to schema deifnition');
    Object.entries(this.schema.paths).forEach(([pathName, pathOperation]) => {
      const pathMedia = {};
      ['post', 'put', 'patch'].forEach((method) => {
        if (pathOperation[method] !== undefined
            && pathOperation[method].requestBody !== undefined) {
          pathMedia[method] = [];
          Object.keys(pathOperation[method].requestBody.content).forEach((mediaType) => {
            pathMedia[method].push(mediaType);
          });
        }
      });
      if (Object.keys(pathMedia).length > 0) {
        this.allowedMedia.set(pathName, pathMedia);
      }
    });
  }
}
module.exports = ExpressServer;
