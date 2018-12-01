
# Markdown for WDP documentation cheatsheet

Here's how to format Markdown in your WDP topics.

**Headings:** Use #s followed by a blank space for notebook titles and section headings:

`#` title<br>
`##` major headings<br>
`###` subheadings<br>
`####` 4th level subheadings

**Emphasis:** Use this code: Bold: `__string__` or `**string**` Italic: `_string_` or `*string*`

**Attributes:** 
Commonly used attributes:
```
cloud
beta blue
codeblock python r scala
```
In a ditamap, include this code in the topicref: `audience="attribute1 attribute2 attribute3"`. See any ditamap for an example.

Within a topic, you have to tag each element separately. Use this syntax on a line by itself right after the text you want to tag:
`{: .attribute1 attribute2}`
For bullets, put after the list to tag them all, or put in front of a single bullet to tag only that line.

**Monospace font:** Surround text with a back quotes (to the left of the 1 key). Don't use for code blocks.
Use monospace for file path and file names and for text users enter or message text users see.

**Code blocks:** Type 3 back quotes on a line. On the next line, start the code. After the code, put 3 back quotes on a line. To do it right, also include codeblock and language attributes: see MarkdownResource.md.

**Line breaks:** Sometimes markdown doesn’t make line breaks when you want them.
Use this code for a manual line break: `<br>`

**Indenting:** Use a greater than sign (`> `) and then a space, then type the text.
Everything is indented until the next carriage return.

**Bullets:** Use the dash sign (`-  `) with two spaces after it or a space, a dash, and a space (` - `),
to create a circular bullet. To create a sub bullet, use a tab followed a dash and two spaces.
You can also use an asterisk instead of a dash, and it works the same.

**Numbered lists:** Start with `1.` followed by a space.
Start each line with some number and a period, then a space. Indent 3 spaces to get subnumbering. You can include bullets and other elements in a step, but don't include blank lines in between or the numbering will discontinue. 

**Graphics:** Use this syntax:
``` markdown
![Alt text](../images/stormtroopocat.jpg "The Stormtroopocat")
```

**Tables:** Use this syntax:
``` markdown
| Heading | Heading |
| ----| ----|
| text   | text |
| text | text |
{: caption="Table 1. Table title" caption-side="top"}
```

**Internal links:** To link to another file in our doc set

```markdown
[Link Text](../relative/linkto/filename.html)
```
**Sub-section links:**

``` markdown
[Section Title](#section-title)
```
For the text in the parentheses, replace spaces and special characters with a hyphen. Make sure to test all the links!

Alternatively, you can add an ID for a section right below the section title. Use this code: `{: #section_ID}`
Make sure that the section_ID is unique within the notebook.

**Link to sub-section in another internal file:**

```markdown
[Link text you want](relative/linkto/filename.html#subheading-anchor)
```

**External links:** Use this code and test all links!
``` markdown
[Link text](https://url/to/topic.html){: new_window}
```
or
```html
<a href="external-url" target="_blank" rel="noopener noreferrer">link text</a>
```

**Videos:** Use this syntax:

```html
<figure class="fignone" id="video01"><figcaption>Figure 1. <span class="ph"><a href="VIDEO_URLrel=0" rel="external" target="_blank" title="If you cannot access the video that is embedded in this page, you can access the video from the YouTube website. (Opens in a new tab or window)">    <img src="images/video.png" alt="Video icon"></a>VIDEO_CAPTION</span></figcaption>

<object height="315" data="VIDOE_URLrel=0" width="560">
<span>ADD AN EXPLANATION OF WHAT THE VIDEO SHOWS SOMEONE HOW TO DO.</span>
<param name="movie" value="VIDEO_URLrel=0">
<param name="allowFullScreen" value="true">
<param name="allowscriptaccess" value="always">
<param name="scale" value="noScale">
</object>
</figure>
```

For accessibility, videos need closed captioning.

**Conrefs:** Use the conrefs in the `dsxconrefs.yml` file. Use this syntax, replacing `conref` with the conref name:
``` markdown
{{site.data.keyword.conref}}
```

**Comments:** Use this syntax:
``` markdown
<!-- text to hide in all builds -->
```