See [jgm/gitit](https://github.com/jgm/gitit) for info on `gitit`

In this fork, I've added code to compile certain plugins, rather than loading them at compile time.
These are placed in `src/Network/Gitit/Plugin/`, and put into the `compiledPlugins` list in `Plugins.hs`
This structure was shamelessly stolen from [Jeff Johnson's fork](https://github.com/jefdaj/gitit).
See [this issue](https://github.com/jgm/gitit/issues/599).

If you're using this approach, note that you need to add your plugins to exposed-modules in `gitit.cabal` (see mine for example).
In my `.cabal` file, I've disabled the ordinary plugin support by default as well.

- `TikzCD` - similar to the `ImgTex` plugin in `gitit`, but for the `tikz-cd` LaTeX package. Uses `dvisvgm`.
  - This produces a metric fuckton of debug output. If running locally, use this to tell TeX to move on when your diagrams are broken.
- The normal gitit `Interwiki` plugin (but fixed to carry around the attrs of the links - I think this is due to a new version of pandoc-types)
- `DeadlinkDetection` - add the `deadlink` class to dead internal links (experimental, kind of a hack)
  - Note that if using the cache, the page needs to be rebuilt to update dead links - it's not a dynamic thing.
  - Also, your external links should start with "http" (and they won't be flagged as dead links)
- `Backlinks` - end each page with a list of backlinks, i.e other pages that link to it
  - This is using the same hack for detecting internal links as `DeadlinkDetection`. May cause errors.
  - Relies on a list of backlinks, saved in `blfile.json`. This is updated whenever a page is rendered (and starts tracking all the links found on the page). If you add this to a wiki with existing pages, or edit your wiki without opening the pages (from the filesystem using `git`, for instance), this may be out of date.
  - Similar to deadlinks, if you use the cache, the list of backlinks is only updated when the page is rebuilt.
  - Also tracks dead internal links ("magic links") - this shouldn't cause issues.

### Other features

- When creating a new page, displays search results for that page.
- I add an empty `<div>` to the edit pages so you can use Ace Editor.

## Planned features

- A spaced-repetition memory system, where one uses commands embedded into pages to generate cards.
- Add debug output, or at least more graceful failures, to tikzcd.

## Development notes to self
  
- To avoid interfering with dead link tagger, any plugins that interpret weird links should have them start with `!`, like `[Text](!cmd ...)`
- To expose more functionality, add "Handlers" to `wikiHandlers` in `Gitit.hs`.
    - These can be found in `Gitit/Handlers.hs`, for examples (and it would be natural to add your own here).
- To add memo functionality, figure out a minimal example of these things:
    - Iterating over pages
    - Updating a list in the filestore
    - Extracting info from this list and displaying it in a handler.
- When you get around to adding Memo pages, make sure to filter them out in index (or sort them? Unclear).
- `updatePage` seems to have the code for adding pages.

## Process to duplicate my setup

- Use this
- Use [gitit-bootstrap-theme](https://github.com/t13a/gitit-bootstrap-theme)
- Put TeX commands into the `content.st` template, in a `display: none` div right over `$content$`.
- Put `.deadlink {color: red;}` into `static/custom.css` (if you want to tag dead links).
- Install ace editor into `/static/js/ace-builds/`
  - Put this code into `page.st` at the top with the other includes:
    `<script type="text/javascript" src="$base$/js/ace-builds/src-noconflict/ace.js"></script>`
- And this at the bottom below the actual content.
```
<script>
\$(document).ready(function() {
        var editor = ace.edit("editor", {
            theme: "ace/theme/tomorrow_night_blue",
            mode: "ace/mode/markdown",
            autoScrollEditorIntoView: true,
            maxLines: 30,
            minLines: 2
        });
        editor.setReadOnly(false);
        var textarea = \$('textarea#editedText').hide();
        editor.getSession().setValue(textarea.val());
        editor.getSession().on('change', function(){
            textarea.val(editor.getSession().getValue());
        });
    });
</script>
```