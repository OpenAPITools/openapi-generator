const React = require("react");
const path = require("path");

const CompLibrary = require("../../core/CompLibrary.js");
const Container = CompLibrary.Container;

const EditThisPage = require(path.resolve(process.cwd(), "core/EditThisPage.js"));

const siteConfig = require(process.cwd() + "/siteConfig.js");

// This is based mostly off of Babel's website configuration for Members/orgs/etc.
const MediaLink = props => {
    if (props.text) {
        return (
            <div>
                <img
                    style={{ marginRight: 4 }}
                    height={props.size}
                    width={props.size}
                    src={props.iconSource}
                    alt={props.iconAlt}
                />
                <a href={props.url} target="_blank" rel="noreferrer noopener">
                    <span className="anchor_text">{props.text}</span>
                </a>
            </div>
        );
    } else return null;
};

const Member = ({member}) => {
    const { baseUrl } = siteConfig;
    const { github, twitter, name, joined } = member;
    const avatarUrl = `https://avatars.githubusercontent.com/${github}`;
    const twitterUrl = `https://twitter.com/${twitter}`;
    const githubUrl = `https://github.com/${github}`;
    const ghIcon = `${baseUrl}img/icons/github.svg`;
    const twitterIcon = `${baseUrl}img/icons/twitter.svg`;

    return (
        <div className="member">
            <div className="avatar">
                <img src={avatarUrl} height="80" width="80" alt="{{name}}" />
            </div>
            <div className="member-info">
                <div style={{ fontWeight: 600 }}>{name}</div>
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
                <div><em>({joined})</em></div>
            </div>
        </div>
    );
};

const MemberGroup = props => {
    return (
        <div>
            <h2 className="member-group">{props.title}</h2>
            <div className="member-container">
                {props.members.map(member => {
                    return <Member key={member.github} member={member} />;
                })}
            </div>
        </div>
    );
};

class Team extends React.Component {
    render() {
        const {team, repoUrl} = siteConfig;
        const editUrl = `${repoUrl}/edit/master/website/dynamic/team.yml`;
        return (
            <div>
                <EditThisPage title="The Team" url={editUrl} />
                <Container padding={["bottom"]}>
                    <div>
                        <MemberGroup title="Core Team" members={team.core} />
                    </div>
                </Container>
            </div>
        );
    }
}

module.exports = Team;
