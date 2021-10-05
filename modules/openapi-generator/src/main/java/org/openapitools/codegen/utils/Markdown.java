/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.utils;

import org.commonmark.node.Node;
import org.commonmark.parser.Parser;
import org.commonmark.renderer.html.HtmlRenderer;


/**
 * Utility class to convert Markdown (CommonMark) to HTML.
 * <a href='https://github.com/atlassian/commonmark-java/issues/83'>This class is threadsafe.</a>
 */
public class Markdown {

    // see https://github.com/atlassian/commonmark-java
    private final Parser parser = Parser.builder().build();
    private final HtmlRenderer renderer = HtmlRenderer.builder().build();

    /**
     * Convert input markdown text to HTML.
     * Simple text is not wrapped in <p>...</p>.
     * @param markdown text with Markdown styles. If <code>null</code>, <code>""</code> is returned.
     * @return HTML rendering from the Markdown
     */
    public String toHtml(String markdown) {
        if (markdown == null)
            return "";
        Node document = parser.parse(markdown);
        String html = renderer.render(document);
        html = unwrapped(html);
        return html;
    }

    // The CommonMark library wraps the HTML with
    //  <p> ... html ... </p>\n
    // This method removes that markup wrapper if there are no other <p> elements,
    // do that Markdown can be used in non-block contexts such as operation summary etc.
    private static final String P_END = "</p>\n";
    private static final String P_START = "<p>";
    private String unwrapped(String html) {
        if (html.startsWith(P_START) && html.endsWith(P_END)
                && html.lastIndexOf(P_START) == 0)
            return html.substring(P_START.length(),
                    html.length() - P_END.length());
        else
            return html;
    }
}
