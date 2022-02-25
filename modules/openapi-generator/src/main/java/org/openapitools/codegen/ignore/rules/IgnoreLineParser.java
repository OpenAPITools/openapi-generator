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

package org.openapitools.codegen.ignore.rules;

import java.util.ArrayList;
import java.util.List;

public class IgnoreLineParser {
    enum Token {
        MATCH_ALL("**"),
        MATCH_ANY("*"),
        ESCAPED_EXCLAMATION("\\!"),
        ESCAPED_SPACE("\\ "),
        PATH_DELIM("/"),
        NEGATE("!"),
        TEXT(null),
        DIRECTORY_MARKER("/"),
        ROOTED_MARKER("/"),
        COMMENT(null);

        private String pattern;

        Token(String pattern) {
            this.pattern = pattern;
        }

        public String getPattern() {
            return pattern;
        }
    }

    // NOTE: Comments that start with a : (e.g. //:) are pulled from git documentation for .gitignore
    // see: https://github.com/git/git/blob/90f7b16b3adc78d4bbabbd426fb69aa78c714f71/Documentation/gitignore.txt
    static List<Part> parse(String text) throws ParserException {
        List<Part> parts = new ArrayList<>();
        StringBuilder sb = new StringBuilder();
        String current = null;
        String next = null;

        char[] characters = text.toCharArray();
        for (int i = 0, totalLength = characters.length; i < totalLength; i++) {
            char character = characters[i];
            current = String.valueOf(character);
            next = i < totalLength - 1 ? String.valueOf(characters[i + 1]) : null;

            if (i == 0) {
                if ("#".equals(current)) {
                    //: A line starting with # serves as a comment.
                    parts.add(new Part(Token.COMMENT, text));
                    i = totalLength; // rather than early return
                    continue;
                } else if ("!".equals(current)) {
                    if (i == totalLength - 1) {
                        throw new ParserException("Negation with no negated pattern.");
                    } else {
                        parts.add(new Part(Token.NEGATE));
                        continue;
                    }
                } else if ("\\".equals(current) && "#".equals(next)) {
                    //: Put a backslash ("`\`") in front of the first hash for patterns
                    //: that begin with a hash.
                    // NOTE: Just push forward and drop the escape character. Falls through to TEXT token.
                    current = next;
                    next = null;
                    i++;
                }
            }

            if (Token.MATCH_ANY.pattern.equals(current)) {

                if (Token.MATCH_ANY.pattern.equals(next)) {
                    // peek ahead for invalid pattern. Slightly inefficient, but acceptable.
                    if ((i+2 < totalLength - 1) &&
                            String.valueOf(characters[i+2]).equals(Token.MATCH_ANY.pattern)) {
                        // It doesn't matter where we are in the pattern, *** is invalid.
                        throw new ParserException("The pattern *** is invalid.");
                    }

                    parts.add(new Part(Token.MATCH_ALL));
                    i++;
                    continue;
                } else {

                    if (sb.length() > 0) {
                        // A MATCH_ANY may commonly follow a filename or some other character. Dump that to results before the MATCH_ANY.
                        parts.add(new Part(Token.TEXT, sb.toString()));
                        sb.delete(0, sb.length());
                    }

                    parts.add(new Part(Token.MATCH_ANY));
                    continue;
                }
            }

            if (i == 0 && Token.ROOTED_MARKER.pattern.equals(current)) {
                parts.add(new Part(Token.ROOTED_MARKER));
                continue;
            }

            if ("\\".equals(current) && " ".equals(next)) {
                parts.add(new Part(Token.ESCAPED_SPACE));
                i++;
                continue;
            } else if ("\\".equals(current) && "!".equals(next)) {
                parts.add(new Part(Token.ESCAPED_EXCLAMATION));
                i++;
                continue;
            }

            if (Token.PATH_DELIM.pattern.equals(current)) {
                if (i != totalLength - 1) {
                    if (sb.length() > 0) {
                        parts.add(new Part(Token.TEXT, sb.toString()));
                        sb.delete(0, sb.length());
                    }

                    parts.add(new Part(Token.PATH_DELIM));
                    if(Token.PATH_DELIM.pattern.equals(next)) {
                        // ignore doubled path delims. NOTE: doesn't do full lookahead, so /// will result in //
                        i++;
                    }
                    continue;
                } else if (i == totalLength - 1) {
                    parts.add(new Part(Token.TEXT, sb.toString()));
                    sb.delete(0, sb.length());

                    parts.add(new Part(Token.DIRECTORY_MARKER));
                    continue;
                }
            }

            sb.append(current);
        }

        if (sb.length() > 0) {
            // NOTE: All spaces escaped spaces are a special token, ESCAPED_SPACE
            //: Trailing spaces are ignored unless they are quoted with backslash ("`\`")
            parts.add(new Part(Token.TEXT, sb.toString().trim()));
        }

        return parts;
    }
}
