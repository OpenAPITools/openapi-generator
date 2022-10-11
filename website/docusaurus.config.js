const fs = require('fs');
const path = require('path');
const parseYaml = require("js-yaml").safeLoad;

const loadYaml = relativePath => parseYaml(fs.readFileSync(path.join(__dirname, relativePath), "utf8"));

// See https://docusaurus.io/docs/site-config for all the possible
// site configuration options.

const team = loadYaml("src/dynamic/team.yml");
const users = loadYaml("src/dynamic/users.yml");
const sponsors = loadYaml("src/dynamic/sponsors.yml");
const baseUrl = '/';

const docusaurusConfig = {
  title: 'OpenAPI Generator',
  tagline: 'Generate clients, servers, and documentation from OpenAPI 2.0/3.x documents',
  url: 'https://openapi-generator.tech', // Your website URL
  baseUrl: baseUrl, // Base URL for your project */
  favicon: 'img/favicon.png',
  organizationName: 'OpenAPITools',
  projectName: 'openapi-generator',

  // // You may provide arbitrary config keys to be used as needed by your
  // // template. For example, if you need your repo's URL...
  // repoUrl: 'https://github.com/OpenAPITools/openapi-generator',

  plugins: ['@docusaurus/plugin-google-analytics'],

  themeConfig: {
    // Open Graph and Twitter card images.
    image: 'img/docusaurus.png',

    sidebarCollapsible: true,

    prism: {
      theme: require('prism-react-renderer/themes/dracula'),
      defaultLanguage: 'bash',
    },

    navbar: {
      title: 'OpenAPI Generator',
      logo: {
        src: 'img/mono-logo.svg',
        alt: 'OpenAPI Tools logo',
      },

      links: [
        {to: 'docs/installation', label: 'Getting Started'},
        {to: 'docs/generators', label: 'Generators'},
        {to: 'docs/roadmap', label: 'Roadmap'},
        {to: "docs/faq", label: "FAQ" },
        {to: "team", label: "Team" },
        {to: "blog", label: 'Blog'},
        {to: 'https://api.openapi-generator.tech', label: 'API'},
      ],
    },

    googleAnalytics: {
      trackingID: 'UA-132927057-1',
    },

    algolia: {
      apiKey: '28e55aff9bab37236baa1c5f0f84f4bb',
      indexName: 'openapi-generator',
      algoliaOptions: { advancedSyntax: true, hitsPerPage: 5},
    },


    footer: {
      style: 'dark',

      logo: {
        alt: 'OpenAPI Tools',
        src: 'img/mono-logo.svg',
        href: 'https://openapi-generator.tech/',
      },

      copyright:  `Copyright © ${new Date().getFullYear()} OpenAPI-Generator Contributors (https://openapi-generator.tech)`,
      links: [
        {
          title: 'Docs',
          items: [
            {
              label: 'Customizing Generators',
              to: 'docs/customization',
            },
            {
              label: 'Installation',
              to: 'docs/installation',
            },
            {
              label: 'Workflow Integrations',
              to: 'docs/integrations',
            },
          ],
        },
        {
          title: 'Community',
          items: [
            {
              label: 'User Showcase',
              to: 'users',
            },
            {
              label: 'Stack Overflow',
              href: 'https://stackoverflow.com/questions/tagged/openapi-generator',
            },
            {
              label: 'Chat Room',
              href: 'https://join.slack.com/t/openapi-generator/shared_invite/zt-12jxxd7p2-XUeQM~4pzsU9x~eGLQqX2g',
            },
            {
              label: 'Twitter',
              href: 'https://twitter.com/oas_generator',
            },
          ],
        },
        {
          title: 'More',
          items: [
            {
              label: 'Blog',
              to: 'blog',
            },
            {
              label: 'GitHub',
              href: 'https://github.com/OpenAPITools/openapi-generator',
            },
          ],
        },
      ]
    },
  },
  presets: [
    [
      '@docusaurus/preset-classic',
      {
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },

        docs: {
          // docs folder path relative to website dir.
          path: '../docs',
          include: ['**/*.md', '**/*.mdx'],

          // sidebars file relative to website dir.
          sidebarPath: require.resolve('./sidebars.js'),

          /**
           * Theme components used by the docs pages
           */
          docLayoutComponent: '@theme/DocPage',
          docItemComponent: '@theme/DocItem',

          editUrl: 'https://github.com/OpenAPITools/openapi-generator/edit/master/website',

          // Equivalent to `docsUrl`.
          routeBasePath: 'docs',
          // Remark and Rehype plugins passed to MDX. Replaces `markdownOptions` and `markdownPlugins`.
          remarkPlugins: [],
          rehypePlugins: [],
          // Equivalent to `enableUpdateBy`.
          showLastUpdateAuthor: true,
          // Equivalent to `enableUpdateTime`.
          showLastUpdateTime: true,
        },
      },
    ],
  ],

  // Add custom scripts here that would be placed in <script> tags.
  scripts: [
      'https://buttons.github.io/buttons.js',
      'https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.0/clipboard.min.js',
      `${baseUrl}js/code-block-buttons.js`,
  ],
  customFields: {
    users: users,
    sponsors: sponsors,
    team: team
  },
};

module.exports = docusaurusConfig;
