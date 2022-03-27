import React from 'react';
import classnames from 'classnames';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import styles from './styles.module.css';
import CodeBlock from '@theme/CodeBlock'

const features = [
    {
        title: <>Easy to Use</>,
        imageUrl: `img/icons/plug.svg`,
        description: (
            <>
                <p>
                    With <em>50+</em> client generators, you can easily generate code to interact with any server which
                    exposes an OpenAPI document.
                </p>
                <p>
                    Maintainers of APIs may also automatically generate and distribute clients as part of official SDKs.
                </p>
                <p>
                    Each client supports different options and features, but all templates can be replaced with your own
                    Mustache-based templates.
                </p>
                See <a href="./docs/customization">Customization</a> for details.
            </>
        ),
    },
    {
        title: <>Servers</>,
        imageUrl: 'img/icons/exchange.svg',
        description: (
            <>
                <p>Getting started with server development can be tough, especially if you're evaluating technologies.
                    We can reduce the burden when you bring your own OpenAPI document.</p>
                <p>Generate server stubs for 40+ different languages and technologies, including Java, Kotlin, Go, and
                    PHP.</p>
                <p>Some generators support <em>Inversion of Control</em>, allowing you to iterate on design via your
                    OpenAPI document without worrying about blowing away your entire domain layer when you regenerate
                    code.</p>
            </>
        ),
    },
    {
        title: <>Schemas/Configs</>,
        imageUrl: 'img/icons/pencil.svg',
        description: (
            <>
                <p>Ever wanted to iteratively design a MySQL database, but writing table declarations was too
                    tedious?</p>
                <p>OpenAPI Generator offers some special generators such as Apache2 Configuration, MySQL and GraphQL
                    schema generators.</p>
                <p>You can easily extend these generators and their templates to create derivative generators!</p>
            </>
        ),
    },
    {
        title: <>Documentation</>,
        imageUrl: 'img/icons/newspaper-o.svg',
        description: (
            <>
                <p>OpenAPI documents allow you to convert the metadata about your API into some other format.</p>
                <p>We include documentation formats such as HTML and Cwiki, which allow you to distribute static
                    documentation to your consumers.</p>
                <p>We also support generating from OpenAPI 2.0 to newer JSON/YAML OpenAPI 3.x documents.</p>
            </>
        ),
    },
];


function stripMargin(template, ...expressions) {
    let result = template.reduce((accumulator, part, i) => {
        return accumulator + expressions[i - 1] + part
    });

    return result.replace(/\r?(\n)\s*\|/g, '$1');
}


const callouts = [
    {
        id: 'learn',
        imageUrl: 'img/color-logo.svg',
        title: <>Learn How</>,
        content: (
            <div><span><p>OpenAPI Generator supports many different integrations and use cases, including (but not limited to):</p>
            <ul>
            <li>Maven Plugin</li>
            <li>Gradle Plugin</li>
            <li>Bazel Plugin</li>
            <li>SBT Plugin</li>
            <li>Cake Plugin</li>
            <li>CLI via Homebrew</li>
            <li>CLI via Docker</li>
            <li>CLI via npm</li>
            <li>Generator SaaS</li>
            </ul>
            <p>For details, see  <a href="/docs/integrations">Workflow Integrations</a></p>
            <p>Generation also allows for easy customization via options, custom templates, or even custom generators on your classpath. See <a
                href="/docs/customization">Customization</a> for details.</p>
            </span></div>
        ),
    },
    {
        id: 'connectOnSlack',
        imageUrl: 'img/tools/Slack_Mark-256x256-3a29a6b.png',
        title: <>Active Community</>,
        content: (
            <>
                <p><strong>Connect</strong> with us on Slack!</p>
                <p>
                    We're a very community-oriented project. We have an active community of users, contributors, and
                    core team members on Slack. Slack is often a good
                    place to start if you're looking for guidance about where to begin contributing, if you have an idea
                    you're
                    not sure fits the project, or if you just want to ask a question or say hello.
                </p>
                <p>Slack is free to <a href="https://slack.com/downloads" className="href">download</a>, and our
                    workspace is free to <a
                        href="https://join.slack.com/t/openapi-generator/shared_invite/zt-12jxxd7p2-XUeQM~4pzsU9x~eGLQqX2g"
                        className="href">sign up</a>.
                </p>
            </>
        ),
    },
    {
        id: 'try',
        imageUrl: 'img/tools/npm.svg',
        title: <>Try via npm</>,
        content: (
            <>
                <p>
                    The <a href="https://github.com/openapitools/openapi-generator-cli" className="href">npm package
                    wrapper</a> is cross-platform wrapper around the .jar artifact.
                </p>
                <p>
                    <strong>Install</strong> globally, exposing the CLI on the command line:
                </p>

                {/* <!-- RELEASE_VERSION --> */}

                <p><CodeBlock className="bash">{stripMargin`
                   |# install the latest version of "openapi-generator-cli"
                   |npm install @openapitools/openapi-generator-cli -g
                   |
                   |# use a specific version of "openapi-generator-cli"
                   |openapi-generator-cli version-manager set 5.3.0
                   |
                   |# Or install it as dev-dependency in your node.js projects
                   |npm install @openapitools/openapi-generator-cli -D
                `}</CodeBlock></p>

                {/*  <!-- /RELEASE_VERSION --> */}

                <p>Then, <strong>generate</strong> a ruby client from a valid <a
                    href="https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml"
                    className="href">petstore.yaml</a> doc:</p>

                <p><CodeBlock className="bash">{`
                openapi-generator-cli generate -i petstore.yaml -g ruby -o /tmp/test/
                `}</CodeBlock></p>
            </>
        ),
    },
    {
        id: 'tryHomebrew',
        imageUrl: 'img/tools/homebrew-256x256.png',
        title: <>Try via Homebrew</>,
        content: (
            <>
                <p><strong>Install</strong> via <a href="https://brew.sh/" className="href">homebrew</a>:</p>

                <p><CodeBlock className="bash">{'brew install openapi-generator'}</CodeBlock></p>

                <p>
                    Then, <strong>generate</strong> a ruby client from a valid <a
                    href="https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml"
                    className="href">petstore.yaml</a> doc:
                </p>

                <p><CodeBlock className="bash">{'openapi-generator generate -i petstore.yaml -g ruby -o /tmp/test/'}</CodeBlock></p>
            </>
        ),
    },
    {
        id: 'tryDocker',
        imageUrl: 'img/tools/docker.png',
        title: <>Try via Docker</>,
        content: (
            <>
                <p>The OpenAPI Generator image acts as a standalone executable. It can be used as an alternative to
                    installing via homebrew, or for developers who are unable to install Java or upgrade the installed
                    version.</p>
                <p>To generate code from a valid <a
                    href="https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml">petstore.yaml</a> doc
                    with this image, you'll need to mount a local location as a volume.
                </p>
                <p>
                    <CodeBlock className="bash">
                        {stripMargin`
                |docker run --rm \\
                |    -v $PWD:/local openapitools/openapi-generator-cli generate \\
                |    -i /local/petstore.yaml \\
                |    -g go \\
                |    -o /local/out/go
                `}
                    </CodeBlock>
                </p>
                <p>For a full list of our docker images, check out <a
                    href="https://hub.docker.com/u/openapitools">u/openapitools</a> on Docker Hub.</p>
            </>
        ),
    }
];

function Callout({id, title, imageUrl, content}) {
    const imgUrl = useBaseUrl(imageUrl);
    let alt = `${id} logo`;

    return (
        <>
            <div id={id} className={classnames('row', styles.calloutRow)}>
                <div className={classnames('col col--3 blockImage')}>
                    <img className={'image'} src={imgUrl} alt={alt}/>
                </div>
                <div className={'col col--9'}>
                    <h2>{title}</h2>
                    {content}
                </div>
            </div>
        </>
    )
}

function Feature({imageUrl, title, description}) {
    const imgUrl = useBaseUrl(imageUrl);
    return (
        <div className={classnames('col col--6', styles.feature)}>
            {imgUrl && (
                <div className="text--center">
                    <img className={styles.featureImage} src={imgUrl} alt="logo"/>
                </div>
            )}
            <h3>{title}</h3>
            <div>{description}</div>
        </div>
    );
}

function ShowcaseUser({image, infoLink, caption}) {
    const imgUrl = useBaseUrl(image);
    return (
        <a href={infoLink} key={infoLink}>
            <img src={imgUrl} alt={caption} title={caption} className={styles.productShowcaseSectionLogo}/>
        </a>
    );
}
function Showcase({users}) {
    const context = useDocusaurusContext();
    const {siteConfig = {}} = context;
    return (
        <>
            <h2>Who is Using This?</h2>
            <p>Here are some users of OpenAPI Generator</p>
            <div className="logos">
                {users.filter(user => user.pinned).map((user, idx) => (
                    <ShowcaseUser key={idx} {...user} />
                ))}
            </div>
            <div className="more-users">
                <Link
                    className={classnames(
                        'button button--outline button--primary button--lg',
                        styles.productShowcaseSectionButton
                    )}
                    to={useBaseUrl('users')}>
                    More {siteConfig.title} Users
                </Link>
            </div>
        </>
    );
}

function Home() {
    const context = useDocusaurusContext();
    const {siteConfig = {}} = context;
    const {sponsors = {}, users = {}} = siteConfig.customFields;

    return (
        <Layout
            title={`Hello from ${siteConfig.title}`}
            description="Description will go into a meta tag in <head />">
            <header className={classnames('hero hero--primary', styles.heroBanner)}>
                <div className="container">
                    <h1 className="hero__title">{siteConfig.title}</h1>
                    <p className="hero__subtitle">{siteConfig.tagline}</p>
                    <div className={styles.buttons}>
                        <Link
                            className={classnames(
                                'button button--outline button--secondary button--lg',
                                styles.getStarted,
                            )}
                            to={useBaseUrl('#try')}>
                            Try It Out
                        </Link>
                        <Link
                            className={classnames(
                                'button button--outline button--secondary button--lg',
                                styles.getStarted,
                            )}
                            to={useBaseUrl('docs/installation')}>
                            Install
                        </Link>
                    </div>

                    <div className={styles.buttons}>
                        <Link
                            className={classnames(
                                'button button--outline button--secondary button--md',
                                styles.getStarted,
                            )}
                            to={useBaseUrl('docs/generators')}>
                            Generators
                        </Link>
                        <Link
                            className={classnames(
                                'button button--outline button--secondary button--md',
                                styles.getStarted,
                            )}
                            to={useBaseUrl('docs/customization')}>
                            Customization
                        </Link>
                        <Link
                            className={classnames(
                                'button button--outline button--secondary button--md',
                                styles.getStarted,
                            )}
                            to={useBaseUrl('docs/integrations')}>
                            Integrations
                        </Link>
                    </div>
                </div>
            </header>
            <div className={classnames(styles.announcement, styles.announcementDark)}>
                <div className={styles.announcementInner}>
                    <h2><b>Sponsors</b></h2>
                    <p>If you find OpenAPI Generator useful, please consider asking your company to <a
                        href="https://opencollective.com/openapi_generator">become a sponsor</a>.</p>
                    <p>You can also individually sponsor the project by <a
                        href="https://opencollective.com/openapi_generator">becoming a backer</a>.</p>
                    <h3>Thank you to our bronze sponsors!</h3>
                    <div className={classnames('avatar', styles.bronzeSponsorAvatars)}>
                        {sponsors
                            .filter(user => user.bronze)
                            .map((user) => (
                                <a className={classnames('avatar__photo-link bronze-sponsor')} href={user.infoLink}
                                   key={user.infoLink}>
                                    <img src={useBaseUrl(user.image)} alt={user.caption} title={user.caption}
                                         className={classnames('avatar__photo avatar__photo--lg')}/>
                                </a>
                            ))}
                    </div>
                </div>
            </div>
            <main>
                {features && features.length && (
                    <section className={styles.features}>
                        <div className="features container">
                            <div className="row">
                                {features.map((props, idx) => (
                                    <Feature key={idx} {...props} />
                                ))}
                            </div>
                        </div>
                    </section>
                )}

                {callouts && callouts.length && (
                    <section className={styles.callout}>
                        <div className="callout container">
                            {callouts.map((props, idx) => (
                                <Callout key={idx} {...props} />
                            ))}
                        </div>
                    </section>
                )}
            </main>

            {users && users.length && (
                <div className={classnames(styles.announcement, styles.announcementLight)}>
                    <div className={classnames(styles.productShowcaseSection, styles.announcementInner)}>
                        <Showcase users={users}/>
                    </div>
                </div>
            )}

            <div className={classnames(styles.announcement, styles.announcementDark)}>
                "OpenAPI Tools" and "OpenAPI Generator" are not affiliated with OpenAPI Initiative (OAI)
            </div>
        </Layout>
    );
}

export default Home;
