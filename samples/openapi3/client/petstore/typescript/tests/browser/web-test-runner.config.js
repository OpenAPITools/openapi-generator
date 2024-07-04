import { puppeteerLauncher } from '@web/test-runner-puppeteer';

export default {
  files: "./dist/*.test.js",
  nodeResolve: true,
  manual: false,
  port: 8080,
  browsers: [
    puppeteerLauncher(),
  ],
};
