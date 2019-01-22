const fs = require('fs');
const path = require('path');
const parseYaml = require("js-yaml").safeLoad;

const loadYaml = relativePath => parseYaml(fs.readFileSync(path.join(__dirname, relativePath), "utf8"));

// See https://docusaurus.io/docs/site-config for all the possible
// site configuration options.

const team = loadYaml("dynamic/team.yml");
const users = loadYaml("dynamic/users.yml");
const baseUrl = '/';
const siteConfig = {
  title: 'OpenAPI Generator', // Title for your website.
  tagline: 'Generate clients, servers, and documentation from OpenAPI 2.0/3.x documents',
  url: 'https://openapi-generator.tech', // Your website URL
  baseUrl: baseUrl, // Base URL for your project */
  // For github.io type URLs, you would set the url and baseUrl like:
  //   url: 'https://facebook.github.io',
  //   baseUrl: '/test-site/',

  // Used for publishing and more
  projectName: 'openapi-generator',
  organizationName: 'OpenAPITools',
  // For top-level user or org sites, the organization is still the same.
  // e.g., for the https://JoelMarcey.github.io site, it would be set like...
  //   organizationName: 'JoelMarcey'

  cname: 'openapi-generator.tech',

  gaTrackingId: 'UA-132927057-1',

  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    {doc: 'installation', label: 'Get Started'},
    {doc: 'generators', label: 'Generators'},
    {doc: 'roadmap', label: 'Roadmap'},
    // {doc: 'doc4', label: 'API'},
    { page: "team", label: "Team" },
    { doc: "faq", label: "FAQ" },
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
  copyright: `Copyright © ${new Date().getFullYear()} OpenAPI-Generator Contributors (https://openapi-generator.tech). (Both "OpenAPI Tools" (https://OpenAPITools.org) and "OpenAPI Generator" are not affiliated with OpenAPI Initiative (OAI))`,

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks.
    theme: 'default',
  },

  // Add custom scripts here that would be placed in <script> tags.
  scripts: [
      'https://buttons.github.io/buttons.js',
      'https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.0/clipboard.min.js',
      `${baseUrl}js/code-block-buttons.js`,
  ],

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
  repoUrl: 'https://github.com/OpenAPITools/openapi-generator',

  team: team,
};

module.exports = siteConfig;
