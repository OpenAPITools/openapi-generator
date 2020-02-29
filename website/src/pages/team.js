import React from 'react';
import Layout from '@theme/Layout';
import useBaseUrl from '@docusaurus/useBaseUrl';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import styles from './styles.module.css';

// This is based mostly off of Babel's website configuration for Members/orgs/etc.
function MediaLink(props) {
    if (props.text) {
        return (
            <div>
                <img
                    style={{marginRight: 4}}
                    height={props.size}
                    width={props.size}
                    src={props.iconSource}
                    alt={props.iconAlt}
                    className={styles.mediaLink}
                />
                <a href={props.url} target="_blank" rel="noreferrer noopener">
                    <span className="anchor_text">{props.text}</span>
                </a>
            </div>
        );
    } else return null;
}

function Member({member}) {
    const {github, twitter, name, joined} = member;
    const avatarUrl = `https://avatars.githubusercontent.com/${github}`;
    const twitterUrl = `https://twitter.com/${twitter}`;
    const githubUrl = `https://github.com/${github}`;
    const ghIcon = useBaseUrl('img/icons/github.svg');
    const twitterIcon = useBaseUrl('img/icons/twitter.svg');

    return (

        <div className="member avatar avatar--vertical">
            <img
                className="avatar__photo avatar__photo--xl"
                src={avatarUrl}
                height="80" width="80" alt={name}
            />
            <div className="avatar__intro">
                <h4 className="avatar__name">{name}</h4>
                <small className="avatar__subtitle">
                    <MediaLink
                        iconAlt="github"
                        iconSource={ghIcon}
                        size="16"
                        url={githubUrl}
                        text={github}
                    />
                    <MediaLink
                        iconAlt="twitter"
                        iconSource={twitterIcon}
                        size="16"
                        url={twitterUrl}
                        text={twitter}
                    />
                </small>
                <div><em>({joined})</em></div>
            </div>
        </div>
    );
}

const MemberGroup = props => {
    return (
        <>
            <h2 className="member-group">{props.title}</h2>
            <div className="member-container">
                {props.members.map(member => (
                    <Member key={member.github} member={member}/>
                ))}
            </div>
        </>
    );
};

function Team() {
    const context = useDocusaurusContext();
    const {siteConfig = {}} = context;
    const {team = {}} = siteConfig.customFields;
    return (
        <>
            <Layout>
                <main>
                    <div className={'container margin-vert--lg'}>
                        <MemberGroup title="Core Team" members={team.core}/>
                    </div>
                </main>
            </Layout>
        </>
    );
}

export default Team;
