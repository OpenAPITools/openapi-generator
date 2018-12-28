const fs = require('fs');
const path = require('path');
const parseYaml = require("js-yaml").safeLoad;

const loadYaml = relativePath => parseYaml(fs.readFileSync(path.join(__dirname, relativePath), "utf8"));

// See https://docusaurus.io/docs/site-config for all the possible
// site configuration options.

const team = loadYaml("dynamic/team.yml");

const users = [
  // {
  //   caption: 'User1',
  //   // You will need to prepend the image path with your baseUrl
  //   // if it is not '/', like: '/test-site/img/docusaurus.svg'.
  //   image: '/img/docusaurus.svg',
  //   infoLink: 'https://www.facebook.com',
  //   pinned: true,
  // },

  {
    caption: 'Angular.Schule',
    image: '/img/companies/angular-schule.svg',
    infoLink: 'https://angular.schule/',
    pinned: true,
  },
  {
    caption: 'ASKUL',
    image: '/img/companies/logo-askul-01.gif',
    infoLink: 'https://www.askul.co.jp/',
    pinned: true,
  },
  {
    caption: 'b<>com',
    image: '/img/companies/b-com.png',
    infoLink: 'https://b-com.com/en',
    pinned: true,
  },
  {
    caption: 'Bithost GmbH',
    image: '/img/companies/bithost.svg',
    infoLink: 'https://www.bithost.ch/',
    pinned: true,
  },
  {
    caption: 'Boxever',
    image: '/img/companies/boxever.svg',
    infoLink: 'https://www.boxever.com/',
    pinned: true,
  },
  {
    caption: 'GMO Pepabo',
    image: '/img/companies/pepabo.png',
    infoLink: 'https://pepabo.com/en/',
    pinned: true,
  },
  {
    caption: 'JustStar',
    image: '/img/companies/juststar.png',
    infoLink: 'https://www.juststarinfo.com/',
    pinned: true,
  },
  {
    caption: 'Klarna',
    image: '/img/companies/klarna.svg',
    infoLink: 'https://www.klarna.com/us/',
    pinned: true,
  },
  {
    caption: 'Metaswitch',
    image: '/img/companies/metaswitch.svg',
    infoLink: 'https://www.metaswitch.com/',
    pinned: false,
  },
  {
    caption: 'Myworkout',
    image: '/img/companies/myworkout.png',
    infoLink: 'https://myworkout.com/',
    pinned: false,
  },
  {
    caption: 'Raiffeisen Schweiz Genossenschaft',
    image: '/img/companies/raiffeisen.png',
    infoLink: 'https://www.raiffeisen.ch/',
    pinned: false,
  },
  {
    caption: 'RepreZen API Studio',
    image: '/img/companies/reprezen.png',
    infoLink: 'https://www.reprezen.com/swagger-openapi-code-generation-api-first-microservices-enterprise-development',
    pinned: false,
  },
  {
    caption: 'REST United',
    image: '/img/companies/rest-united.png',
    infoLink: 'https://restunited.com/',
    pinned: false,
  },
  {
    caption: 'Stingray',
    image: '/img/companies/stingray.png',
    infoLink: 'http://www.stingray.com/',
    pinned: false,
  },
  {
    caption: 'Suva',
    image: '/img/companies/suva.svg',
    infoLink: 'https://www.suva.ch/',
    pinned: false,
  },
  {
    caption: 'Telstra',
    image: '/img/companies/telstra.svg',
    infoLink: 'https://dev.telstra.com/',
    pinned: false,
  },

  {
    caption: 'TUI InfoTec GmbH',
    image: '/img/companies/infotec.png',
    infoLink: 'http://www.tui-infotec.com/',
    pinned: false,
  },
  {
    caption: 'unblu inc.',
    image: '/img/companies/unblu.svg',
    infoLink: 'https://www.unblu.com/',
    pinned: false,
  },
  {
    caption: 'Zalando',
    image: '/img/companies/zalando.jpg',
    infoLink: 'https://www.zalando.com/',
    pinned: false,
  },
];

const siteConfig = {
  title: 'OpenAPI Generator', // Title for your website.
  tagline: 'Generate clients, servers, and documentation from OpenAPI 2.0/3.x documents',
  url: 'https://your-docusaurus-test-site.com', // Your website URL
  baseUrl: '/', // Base URL for your project */
  // For github.io type URLs, you would set the url and baseUrl like:
  //   url: 'https://facebook.github.io',
  //   baseUrl: '/test-site/',

  // Used for publishing and more
  projectName: 'openapi-generator',
  organizationName: 'openapitools',
  // For top-level user or org sites, the organization is still the same.
  // e.g., for the https://JoelMarcey.github.io site, it would be set like...
  //   organizationName: 'JoelMarcey'

  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    {doc: 'roadmap', label: 'Roadmap'},
    {doc: 'generators', label: 'Generators'},
    // {doc: 'doc4', label: 'API'},
    { page: "team", label: "Team" },
    // {page: 'help', label: 'Help'},
    {blog: true, label: 'Blog'},
  ],

  // If you have users set above, you add it here:
  users,

  /* path to images for header/footer */
  headerIcon: 'img/mono-logo.svg',
  footerIcon: 'img/mono-logo.svg',
  favicon: 'img/favicon.png',

  /* Colors for website */
  colors: {
    primaryColor: '#76c513',
    secondaryColor: '#464446',
  },

  /* Custom fonts for website */
  /*
  fonts: {
    myFont: [
      "Times New Roman",
      "Serif"
    ],
    myOtherFont: [
      "-apple-system",
      "system-ui"
    ]
  },
  */

  // This copyright info is used in /core/Footer.js and blog RSS/Atom feeds.
  copyright: `Copyright © ${new Date().getFullYear()} OpenAPI-Generator Contributors (https://openapi-generator.tech)`,

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks.
    theme: 'default',
  },

  // Add custom scripts here that would be placed in <script> tags.
  scripts: ['https://buttons.github.io/buttons.js'],

  // On page navigation for the current documentation page.
  onPageNav: 'separate',
  // No .html extensions for paths.
  cleanUrl: true,

  // Open Graph and Twitter card images.
  ogImage: 'img/docusaurus.png',
  twitterImage: 'img/icons/twitter.svg',

  // Show documentation's last contributor's name.
  // enableUpdateBy: true,

  // Show documentation's last update time.
  enableUpdateTime: true,

  // You may provide arbitrary config keys to be used as needed by your
  // template. For example, if you need your repo's URL...
  repoUrl: 'https://https://github.com/OpenAPITools/openapi-generator',

  team: team,

};

module.exports = siteConfig;
