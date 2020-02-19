import React from 'react';
import Layout from '@theme/Layout';

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import styles from "./styles.module.css";
import classnames from "classnames";

function Users() {
    const context = useDocusaurusContext();
    const {siteConfig = {}} = context;
    const {users = {}} = siteConfig.customFields;

    let showcase = users.map((user, idx) => {
            // avoid joining base/ with /img/path as double-slash breaks image load.
            let trimmedImg = user.image.startsWith("/") ? user.image.slice(1, user.image.length) : user.image;
            let imgUrl = useBaseUrl(trimmedImg);
            return (
                <a href={user.infoLink} key={user.infoLink}>
                    <img src={imgUrl} alt={user.caption} title={user.caption}
                         className={styles.productShowcaseSectionLogo}/>
                </a>
            );
        }
    );

    return (
        <Layout>
            {/*<EditThisPage title="Who is Using This?" url={editUrl} />*/}
            <main>
                <div className={'container margin-vert--lg'}>
                    <h1>Who is Using This?</h1>
                    <div className={classnames(styles.announcement, styles.announcementLight)}>
                        <p>Here are some of our users. To add your company, click "Edit this page" below.</p>
                        <div className={classnames(styles.productShowcaseSection, styles.announcementInner)}>
                            {showcase}
                        </div>
                        <Link
                            className={classnames(
                                'button button--outline button--primary button--md',
                                styles.productShowcaseSectionButton
                            )}
                            to={'https://github.com/OpenAPITools/openapi-generator/edit/master/website/src/dynamic/users.yml'}>
                            Edit this page
                        </Link>
                    </div>
                </div>
            </main>
        </Layout>
    );
}

export default Users;