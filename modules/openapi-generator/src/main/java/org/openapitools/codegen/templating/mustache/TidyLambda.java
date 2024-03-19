package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.io.Writer;

/**
 * Creates a text into a tidy comment converting special tags
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("tidy", new TidyLambda());
 * additionalProperties.put("tidyForDocs", new TidyLambda(true));
 * </pre>
 * <p>
 * Use:
 * <pre>{@code
 *     {{#tidy}}{{{text}}}{{/tidy}}
 * }</pre>
 * <pre>{@code
 *     {{#tidyForDocs}}{{{text}}}{{/tidyForDocs}}
 * }</pre>
 */
public class TidyLambda implements Mustache.Lambda 
{
    private static final String SINGLE_SPACE = " ";
    private static final String HTML_BREAK_TAG = "<br>";
    private static final String LIST_DELIMITER = "((\\* )|(- )|(\\+ )|([0-9]. )|([0-9][0-9]. ))";
    private static final String HORIZONTAL_RULE = " ((\\*){3,}|(-){3,}|(_){3,})";
    private static final String ANYTHING = "\\r|\\n|\\S|.|\\*|\\+";
    private static final String START_OF_FIRST_LINE = "(?<!(" + ANYTHING + "))"; //The first line starts before anything else
    private static final String END_OF_LAST_LINE = "(?!(" + ANYTHING + "))"; //The last line ends and after that there is nothing else

    private final String openingBracketRegex;
    private final String openingBracketReplacement;
    private final String closingBracketRegex;
    private final String closingBracketReplacement;

    private final String multipleNewlineRegex;
    private final String multipleNewlineReplacement;

    private final String apostrophesRegex;

    public TidyLambda(boolean forDocs)
    {
        if(forDocs) {
            //use maximum 2 consecutive line breaks for docs
            multipleNewlineRegex = "((\\r)?\\n){3,}";
            multipleNewlineReplacement = "\r\n\r\n";
            //default behavior for opening/closing brackets
            openingBracketRegex = "\\<";
            openingBracketReplacement = "&lt;";
            closingBracketRegex = "\\>";
            closingBracketReplacement = "&gt;";
            //all non-standard apostrophes excluding ``` used for code blocks
            apostrophesRegex = "((?<!`)``(?!`))|((`){4,})|((?<!`)`(?!`))"; 
        } else {
            //use only 1 consecutive line breaks for comments
            multipleNewlineRegex = "((\\r)?\\n){2,}";
            multipleNewlineReplacement = "\r\n";
            //escape opening/closing brackets
            openingBracketRegex = "<";
            openingBracketReplacement = "\\<";
            closingBracketRegex = ">";
            closingBracketReplacement = "\\>";
            //all non-standard apostrophes
            apostrophesRegex = "(`)+";
        }
    }

    public TidyLambda()
    {
        this(false);
    }

    @Override
    public void execute(Template.Fragment i_fragment, Writer i_writer) throws IOException 
    {
        String text = 
            i_fragment.execute()
                // Replace and normalize all non-standard apostrophes
                .replaceAll(apostrophesRegex, "'")
                // Replace and normalize all non-standard quotes
                .replaceAll("[“”]+", "\"")
                // Replace all HTML break tags by new lines
                .replaceAll(HTML_BREAK_TAG, "\r\n")
                //Replace hard tabs with space (https://github.com/DavidAnson/markdownlint/blob/v0.33.0/doc/Rules.md#md010---hard-tabs)
                .replaceAll("\\t"," ")
                // Escape all opening/closing brackets if escapeBrackets is enabled, otherwise use the default replacements
                .replaceAll(openingBracketRegex, openingBracketReplacement)
                .replaceAll(closingBracketRegex, closingBracketReplacement)
                // Normalize spaces (https://github.com/DavidAnson/markdownlint/blob/v0.33.0/doc/Rules.md#md012---multiple-consecutive-blank-lines)
                // (list indentation and list style are not covered by this Lambda class)
                .replaceAll("(?<=(\\S))( ){2,}", SINGLE_SPACE)
                .replaceAll("( ){2,}(?!( |" + LIST_DELIMITER + "))", SINGLE_SPACE)
                //Make sure that a line only with ' ***' or ' ---' or ' ___' is considered an horizontal rule so it should have \r\n\r\n before and after it (Horizontal rule definition: https://www.markdownguide.org/basic-syntax/#horizontal-rules and horizontal rule style: https://github.com/DavidAnson/markdownlint/blob/v0.33.0/doc/Rules.md#md035---horizontal-rule-style)
                .replaceAll("(((\\r)?\\n)|<br>)*( |\\t)*" + HORIZONTAL_RULE + "( |\\t)*(((\\r)?\\n)|<br>)*", multipleNewlineReplacement + " ***" + multipleNewlineReplacement)
                // Normalize new-lines (maximum 2 consecutive line-breaks)
                .replaceAll(multipleNewlineRegex, multipleNewlineReplacement)
                // Remove new-lines at the beginning
                .replaceAll(START_OF_FIRST_LINE + "((\\r)?\\n)+(?=(.))", "")
                // Remove new-lines, spaces and tabs at the end
                .replaceAll("(?<=(.))(((\\r)?\\n)| |\\t)+" + END_OF_LAST_LINE, "")
                // Remove trailing spaces at the end of each line
                .replaceAll("( |\\t)+(?=(\\r|\\n|$))", "");
        i_writer.write(text);
    }
}
