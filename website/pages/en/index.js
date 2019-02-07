/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require('react');

const CompLibrary = require('../../core/CompLibrary.js');

const MarkdownBlock = CompLibrary.MarkdownBlock; /* Used to read markdown */
const Container = CompLibrary.Container;
const GridBlock = CompLibrary.GridBlock;
const translate = require('../../server/translate.js').translate;

function stripMargin (template, ...expressions) {
    let result = template.reduce((accumulator, part, i) => {
        return accumulator + expressions[i - 1] + part
    });

    return result.replace(/\r?(\n)\s*\|/g, '$1');
}

class HomeSplash extends React.Component {
  render() {
    const {siteConfig, language = ''} = this.props;
    const {baseUrl, docsUrl} = siteConfig;
    const docsPart = `${docsUrl ? `${docsUrl}/` : ''}`;
    const langPart = `${language ? `${language}/` : ''}`;
    const docUrl = doc => `${baseUrl}${docsPart}${langPart}${doc}`;

    const SplashContainer = props => (
      <div className="homeContainer">
        <div className="homeSplashFade">
          <div className="wrapper homeWrapper">{props.children}</div>
        </div>
      </div>
    );

    const Logo = props => (
      <div className="splashLogo">
        <img src={props.img_src} alt="Project Logo" />
      </div>
    );

    const ProjectTitle = () => (
      <h2 className="projectTitle">
        {siteConfig.title}
        <small>{siteConfig.tagline}</small>
      </h2>
    );

    const PromoSection = props => (
      <div className="section promoSection">
        <div className="promoRow">
          <div className="pluginRowBlock">{props.children}</div>
        </div>
      </div>
    );

    const Button = props => (
      <div className="pluginWrapper buttonWrapper">
        <a className="button" href={props.href} target={props.target}>
          {props.children}
        </a>
      </div>
    );

    return (
      <SplashContainer>
        <div className="inner">
          <ProjectTitle siteConfig={siteConfig} />
          <Logo img_src={`${baseUrl}img/color-logo.svg`} />
          <PromoSection>
            <Button href="#try">Try It Out</Button>
              <Button href={docUrl('installation.html')}>Install</Button>
              <Button href={docUrl('generators.html')}>Generators</Button>
            <Button href={docUrl('customization.html')}>Customization</Button>
            <Button href={docUrl('integrations.html')}>Integrations</Button>
          </PromoSection>
        </div>
      </SplashContainer>
    );
  }
}

class Index extends React.Component {
    docUrl(doc, language) {
        const baseUrl = this.props.config.baseUrl;
        const docsUrl = this.props.config.docsUrl;
        const docsPart = `${docsUrl ? `${docsUrl}/` : ''}`;
        const langPart = `${language ? `${language}/` : ''}`;
        return `${baseUrl}${docsPart}${langPart}${doc}`;
    }

    pageUrl(doc, language) {
        const baseUrl = this.props.config.baseUrl;
        return baseUrl + (language ? `${language}/` : '') + doc;
    }

  render() {
    const {config: siteConfig, language = ''} = this.props;
    const {baseUrl} = siteConfig;

    const Block = props => (
      <Container
        padding={['bottom', 'top']}
        id={props.id}
        background={props.background}>
        <GridBlock
          align="left"
          contents={props.children}
          layout={props.layout}
        />
      </Container>
    );

    const FeatureCallout = () => (
      <div
        className="productShowcaseSection paddingBottom"
        style={{textAlign: 'center'}}>
        <h2>Feature Callout</h2>
        <MarkdownBlock>Feature Matrix *Coming Soon*!</MarkdownBlock>
      </div>
    );

    const tryHomebrewContents = stripMargin`
        | **Install** via [homebrew](https://brew.sh/):
        |
        | \`\`\`bash
        | brew install openapi-generator
        | \`\`\`
        | 
        | Then, **generate** a ruby client from a valid [petstore.yaml](https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml) doc:
        |
        | \`\`\`bash
        | openapi-generator generate -i petstore.yaml -g ruby -o /tmp/test/
        | \`\`\`
        `;
    const TryOutHomebrew = () => (
      <Block id="tryHomebrew">
        {[
          {
            content: `${tryHomebrewContents}`,
            image: `${baseUrl}img/tools/homebrew-256x256.png`,
            imageAlign: 'left',
            title: 'Try via Homebrew',
          },
        ]}
      </Block>
    );

  const tryDockerContents = stripMargin`
    | The OpenAPI Generator image acts as a standalone executable. It can be used as an alternative to installing via homebrew, or for developers who are unable to install Java or upgrade the installed version.
    | 
    | To generate code from a valid [petstore.yaml](https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml) doc with this image, you'll need to mount a local location as a volume.
    | \`\`\`bash
    | docker run --rm \\
    | -v \${PWD}:/local openapitools/openapi-generator-cli generate \\
    | -i petstore.yaml \\
    | -g go \\
    | -o /local/out/go
    | \`\`\`
    `;
   const TryOutDocker = () => (
      <Block id="tryDocker">
          {[
              {
                  content: `${tryDockerContents}`,
                  image: `${baseUrl}img/tools/docker.png`,
                  imageAlign: 'left',
                  title: 'Try via Docker',
              },
          ]}
      </Block>
  );

   const tryNpmContents = stripMargin`
      | The [NPM package wrapper](https://github.com/openapitools/openapi-generator-cli) is cross-platform wrapper around the .jar artifact.
      | **Install** globally, exposing the CLI on the command line:
      | 
      | \`\`\`bash
      | # install the latest version of "openapi-generator-cli"
      | npm install @openapitools/openapi-generator-cli -g
      | 
      | # install a specific version of "openapi-generator-cli"
      | npm install @openapitools/openapi-generator-cli@cli-3.0.0 -g
      |
      | # Or install it as dev-dependency in your node.js projects
      | npm install @openapitools/openapi-generator-cli -D
      | \`\`\`
      |
      | Then, **generate** a ruby client from a valid [petstore.yaml](https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml) doc:
      | \`\`\`bash
      | openapi-generator generate -i petstore.yaml -g ruby -o /tmp/test/
      | \`\`\`
    `;
   const TryNpm = () => (
       // note: id here is the topmost anchor linked via the "Try it out" button above.
       <Block id="try">
           {[
               {
                   content: `${tryNpmContents}`,
                   image: `${baseUrl}img/tools/npm.svg`,
                   imageAlign: 'left',
                   title: 'Try via NPM',
               }
           ]}
       </Block>
   );

    // const Description = () => (
    //   <Block background="dark">
    //     {[
    //       {
    //         content:
    //           'This is another description of how this project is useful',
    //         image: `${baseUrl}img/docusaurus.svg`,
    //         imageAlign: 'right',
    //         title: 'Description',
    //       },
    //     ]}
    //   </Block>
    // );

    const LearnHow = () => (
      <Block background="light">
        {[
          {
            content: 'OpenAPI Generator supports many different integrations and use cases, including (but not limited to):\n\n' +
              '* Maven Plugin\n' +
              '* Gradle Plugin\n' +
              '* CLI via Homebrew\n' +
              '* CLI via Docker\n' +
              '* Generator SaaS\n\n' +
              'For details, see  [Workflow Integrations](' + this.docUrl('integrations.html', this.props.language) + ')\n\n' +
              'Generation also allows for easy customization via options, custom templates, or even custom generators on your classpath. ' +
               'See [Customization](' + this.docUrl('customization.html', this.props.language) + ') for details.\n\n' +
              'As a very community-oriented project, the core team is also active on the project\'s [Gitter channel](https://gitter.im/OpenAPITools/openapi-generator).',
            image: `${baseUrl}img/color-logo.svg`,
            imageAlign: 'right',
            title: 'Learn How',
          },
        ]}
      </Block>
    );

    const Features = () => (
        <div className="features">
          <Block layout="fourColumn">
            {[
              {
                title: 'Clients',
                content: 'With *50+* client generators, you can easily generate code to interact with any server which exposes an OpenAPI document.\n\n' +
                  'Maintainers of APIs may also automatically generate and distribute clients as part of official SDKs.\n\n' +
                  'Each client supports different options and features, but all templates can be replaced with your own Mustache-based templates.\n\n' +
                  'See [Customization](' + this.docUrl('customization.html', this.props.language) + ') for details.',
                image: `${baseUrl}img/icons/plug.svg`,
                imageAlign: 'top',
                classNames: 'feature-client'
              },
                {
                    content: 'Getting started with server development can be tough, especially if you\'re evaluating technologies. We can reduce the burden when you bring your own OpenAPI document.\n\n' +
                    'Generate server stubs for 40+ different languages and technologies, including Java, Kotlin, Go, and PHP.\n\n' +
                    'Some generators support *Inversion of Control*, allowing you to iterate on design via your OpenAPI document without worrying about blowing away your entire domain layer when you regenerate code.',
                    image: `${baseUrl}img/icons/exchange.svg`,
                    imageAlign: 'top',
                    title: 'Servers',
                },
            ]}
          </Block>
            <Block layout="fourColumn">
                {[

                    {
                        content: 'Ever wanted to iteratively design a MySQL database, but writing table declarations was too tedious?\n\n' +
                            'OpenAPI Generator offers some special generators such as Apache2 Configuration, MySQL and GraphQL schema generators.\n\n' +
                            'You can easily extend these generators and their templates to create derivative generators!',
                        image: `${baseUrl}img/icons/pencil.svg`,
                        imageAlign: 'top',
                        title: 'Schemas/Configs',
                    },
                    {
                        content: 'OpenAPI documents allow you to convert the metadata about your API into some other format.\n\n' +
                            'We include documentation formats such as HTML and Cwiki, which allow you to distribute static documentation to your consumers.\n\n' +
                            'We also support generating from OpenAPI 2.0 to newer JSON/YAML OpenAPI 3.x documents.',
                        image: `${baseUrl}img/icons/newspaper-o.svg`,
                        imageAlign: 'top',
                        title: 'Documentation',
                    },
                ]}
            </Block>
        </div>
    );

    const Showcase = () => {
      if ((siteConfig.users || []).length === 0) {
        return null;
      }

      const showcase = siteConfig.users
        .filter(user => user.pinned)
        .map(user => (
          <a href={user.infoLink} key={user.infoLink}>
            <img src={baseUrl + user.image} alt={user.caption} title={user.caption} />
          </a>
        ));

      const pageUrl = page => baseUrl + (language ? `${language}/` : '') + page;

      return (
        <div className="productShowcaseSection paddingBottom">
          <h2>Who is Using This?</h2>
          <p>Here are some users of OpenAPI Generator</p>
          <div className="logos">{showcase}</div>
          <div className="more-users">
            <a className="button" href={pageUrl('users.html')}>
              More {siteConfig.title} Users
            </a>
          </div>
        </div>
      );
    };

    return (
      <div>
        <HomeSplash siteConfig={siteConfig} language={language} />
        <div className="mainContainer">
          <Features />
          {/*<FeatureCallout />*/}
          <LearnHow />
          <TryNpm />
          <TryOutHomebrew />
          <TryOutDocker />
          {/*<Description />*/}
          <Showcase />
        </div>
      </div>
    );
  }
}

module.exports = Index;
