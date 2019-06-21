const config = require('./config');
const logger = require('./logger');
const App = require('./app');

const app = new App(config);
app.launch()
  .then(() => {
    logger.info('Server launched');
  })
  .catch((error) => {
    logger.error('found error, shutting down server');
    app.close()
      .catch(closeError => logger.error(closeError))
      .finally(() => logger.error(error));
  });
