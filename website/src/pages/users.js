import React from 'react';
import Layout from '@theme/Layout';

import useBaseUrl from '@docusaurus/useBaseUrl';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';

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
                    <img src={imgUrl} alt={user.caption} title={user.caption}/>
                </a>
            );
        }
    );

    return (
        <Layout padding={['bottom']}>
            {/*<EditThisPage title="Who is Using This?" url={editUrl} />*/}
            <div className={'showcaseSection'}>
                <div className={'prose'}>
                    <p>Here are some of the users. To add yours to the list below, please click on "Edit this page"</p>
                </div>
                <div className={'logos'}>{showcase}</div>
            </div>
        </Layout>
    );
}

export default Users;