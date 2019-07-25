const React = require("react");
const path = require("path");

const CompLibrary = require(path.join(__dirname, "../node_modules/docusaurus/lib/core/CompLibrary.js"));
const Container = CompLibrary.Container;
const MarkdownBlock = CompLibrary.MarkdownBlock;

class EditThisPage extends React.Component {
    renderHeader(title) {
        if (!title) {
            return null;
        }
        return (
            <h1>
                <MarkdownBlock>{title}</MarkdownBlock>
            </h1>
        )
    }

    render() {
        return (
            <Container padding={[]}>
                <div className="edit-this-page text-center">
                    {this.renderHeader(this.props.title)}
                    <p className="link">
                        <a
                            href={this.props.url}
                            target="_blank"
                            rel="noreferrer noopener"
                        >
                            Edit this page
                        </a>
                    </p>
                </div>
            </Container>
        );
    }
}

module.exports = EditThisPage;
