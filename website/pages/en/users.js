/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require('react');
const path = require("path");

const CompLibrary = require('../../core/CompLibrary.js');

const EditThisPage = require(path.resolve(process.cwd(), "core/EditThisPage.js"));

const Container = CompLibrary.Container;

class Users extends React.Component {
  render() {
    const {config: siteConfig} = this.props;
    const {baseUrl, repoUrl} = siteConfig;
    if ((siteConfig.users || []).length === 0) {
      return null;
    }

    const editUrl = `${repoUrl}/edit/master/website/dynamic/team.yml`;
    const showcase = siteConfig.users.map(user => {
          let imgUrl = `${baseUrl}${user.infoLink.startsWith("/") ? user.image : "/" + user.image}`;
          return (
              <a href={user.infoLink} key={user.infoLink}>
                <img src={imgUrl} alt={user.caption} title={user.caption}/>
              </a>
          );
        }
    );

    return (
      <Container padding={['bottom']}>
        <EditThisPage title="Who is Using This?" url={editUrl} />
        <div className="showcaseSection">
          <div className="prose">
            <p>This project is used by many folks</p>
          </div>
          <div className="logos">{showcase}</div>
        </div>
      </Container>
    );
  }
}

module.exports = Users;
