import * as path from 'path'
import * as process from 'process'

import { runQunitPuppeteer, printResultSummary, printFailedTests } from 'node-qunit-puppeteer';

const qunitArgs = {
  // Path to qunit tests suite
  targetUrl: `file://${path.join(__dirname, '../index.html')}`,
  // (optional, 30000 by default) global timeout for the tests suite
  timeout: 10000,
  // (optional, false by default) should the browser console be redirected or not
  redirectConsole: true,
  puppeteerArgs: [ '--disable-web-security']
};
 
runQunitPuppeteer(qunitArgs)
  .then((result: any) => {
    printResultSummary(result, console);
 
    if (result.stats.failed > 0) {
      printFailedTests(result, console);
      // other action(s) on failed tests
      process.exit(1)
    }
  })
  .catch((ex: any) => {
    console.error(ex);
    process.exit(2)
  });