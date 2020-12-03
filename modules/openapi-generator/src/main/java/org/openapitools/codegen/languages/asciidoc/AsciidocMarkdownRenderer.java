package org.openapitools.codegen.languages.asciidoc;

import org.commonmark.internal.renderer.NodeRendererMap;
import org.commonmark.node.Node;
import org.commonmark.renderer.Renderer;

public class AsciidocMarkdownRenderer implements Renderer {

  @Override
  public void render(Node node, Appendable appendable) {

    NodeRendererMap nrm = new NodeRendererMap();
    nrm.add(new CoreAsciidocNodeRenderer(appendable));
    nrm.render(node);
  }

  @Override
  public String render(Node node) {
    StringBuilder b = new StringBuilder();
    render(node, b);
    return b.toString();
  }
}
