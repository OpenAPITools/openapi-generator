package org.openapitools.codegen.languages.asciidoc;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.commonmark.node.AbstractVisitor;
import org.commonmark.node.Block;
import org.commonmark.node.BlockQuote;
import org.commonmark.node.BulletList;
import org.commonmark.node.Code;
import org.commonmark.node.Document;
import org.commonmark.node.Emphasis;
import org.commonmark.node.FencedCodeBlock;
import org.commonmark.node.HardLineBreak;
import org.commonmark.node.Heading;
import org.commonmark.node.HtmlBlock;
import org.commonmark.node.HtmlInline;
import org.commonmark.node.Image;
import org.commonmark.node.IndentedCodeBlock;
import org.commonmark.node.Link;
import org.commonmark.node.ListBlock;
import org.commonmark.node.ListItem;
import org.commonmark.node.Node;
import org.commonmark.node.OrderedList;
import org.commonmark.node.Paragraph;
import org.commonmark.node.SoftLineBreak;
import org.commonmark.node.StrongEmphasis;
import org.commonmark.node.Text;
import org.commonmark.node.ThematicBreak;
import org.commonmark.renderer.NodeRenderer;

public class CoreAsciidocNodeRenderer extends AbstractVisitor implements NodeRenderer {
  static final List<Class<? extends Node>> NODE_TYPES =
      Arrays.asList(
          Document.class,
          Heading.class,
          Paragraph.class,
          BlockQuote.class,
          BulletList.class,
          FencedCodeBlock.class,
          HtmlBlock.class,
          ThematicBreak.class,
          IndentedCodeBlock.class,
          Link.class,
          ListItem.class,
          OrderedList.class,
          Image.class,
          Emphasis.class,
          StrongEmphasis.class,
          Text.class,
          Code.class,
          HtmlInline.class,
          SoftLineBreak.class,
          HardLineBreak.class);
  private static final String CODE_BLOCK = "-----------------";
  Appendable appendable;

  public CoreAsciidocNodeRenderer(Appendable appendable) {

    this.appendable = appendable;
  }

  @Override
  public Set<Class<? extends Node>> getNodeTypes() {

    return new HashSet<>(NODE_TYPES);
  }

  @Override
  public void render(Node node) {
    node.accept(this);
  }

  @Override
  public void visit(BlockQuote blockQuote) {
    visitChildren(blockQuote);
  }

  @Override
  public void visit(BulletList bulletList) {
    append("--\n");
    visitChildren(bulletList);
    append("--\n");
  }

  @Override
  public void visit(Code code) {
    append("`").append(code.getLiteral());
    visitChildren(code);
    append("`");
  }

  @Override
  public void visit(Document document) {
    visitChildren(document);
  }

  @Override
  public void visit(Emphasis emphasis) {
    append("_");
    visitChildren(emphasis);
    append("_");
  }

  @Override
  public void visit(FencedCodeBlock fencedCodeBlock) {
    appendSourceCode(fencedCodeBlock.getLiteral());
  }

  @Override
  public void visit(HardLineBreak hardLineBreak) {
    append("\n\n");
  }

  @Override
  public void visit(Heading heading) {
    newLine();
    // ascii doc max 5 levels
    for (int i = 0; i <= Math.min(heading.getLevel(), 5); i++) {
      append("=");
    }
    append(" ");
    visitChildren(heading);
    newLine();
  }

  @Override
  public void visit(ThematicBreak thematicBreak) {
    newLine().append("''''").newLine();
  }

  @Override
  public void visit(HtmlInline htmlInline) {
    append(htmlInline.getLiteral());
  }

  @Override
  public void visit(HtmlBlock htmlBlock) {
    newLine().append(htmlBlock.getLiteral()).append("\n");
  }

  @Override
  public void visit(Image image) {
    append("image:");
    append(image.getDestination());
    append("\"");
    append(image.getTitle());
    append("\"");
  }

  @Override
  public void visit(IndentedCodeBlock indentedCodeBlock) {
    append(CODE_BLOCK)
        .newLine()
        .append(indentedCodeBlock.getLiteral())
        .newLine()
        .append(CODE_BLOCK)
        .newLine();
  }

  @Override
  public void visit(Link link) {
    // link:asciidoc[This document]
    append("link:").append(link.getDestination()).append("[");
    visitChildren(link);
    append("]");
  }

  @Override
  public void visit(ListItem listItem) {
    Block parent = listItem.getParent();
    String item = parent instanceof BulletList ? "* " : ". ";
    append(item);
    visitChildren(listItem);
    newLine();
  }

  @Override
  public void visit(OrderedList orderedList) {
    append("--").newLine();
    visitChildren(orderedList);
    append("--").newLine();
  }

  @Override
  public void visit(Paragraph paragraph) {
    if (!skipLeadingBreak(paragraph)) {
      newLine();
    }
    visitChildren(paragraph);
    if (paragraph.getNext() != null) {
      newLine();
    }
  }

  private boolean skipLeadingBreak(Paragraph paragraph) {
    Node parent = paragraph.getParent();
    return parent != null
        && ((parent instanceof Document && paragraph.getPrevious() == null)
            || parent.getParent() instanceof ListBlock);
  }

  @Override
  public void visit(SoftLineBreak softLineBreak) {
    append(" +\n");
  }

  @Override
  public void visit(StrongEmphasis strongEmphasis) {
    append("*");
    visitChildren(strongEmphasis);
    append("*");
  }

  @Override
  public void visit(Text text) {
    append(text.getLiteral());
  }

  private void appendSourceCode(String code) {
    append("[source]\n----").newLine().append(code).newLine().append("----").newLine();
  }

  private CoreAsciidocNodeRenderer newLine() {
    return append("\n");
  }

  private CoreAsciidocNodeRenderer append(CharSequence s) {
    if (s == null) {
      return this;
    }
    try {
      appendable.append(s);
    } catch (IOException ex) {
      throw new RuntimeException("Failed to write output");
    }
    return this;
  }
}
