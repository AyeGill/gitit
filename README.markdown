See [jgm/gitit](https://github.com/jgm/gitit) for info on `gitit`

In this fork, I've added code to compile certain plugins, rather than loading them at compile time.
These are placed in `src/Network/Gitit/Plugin/`, and put into the `compiledPlugins` list in `Plugins.hs`
This structure was shamelessly stolen from [Jeff Johnson's fork](https://github.com/jefdaj/gitit).
See [this issue](https://github.com/jgm/gitit/issues/599).

If you're using this approach, note that you need to add your plugins to exposed-modules in `gitit.cabal` (see mine for example).
In my `.cabal` file, I've disabled the ordinary plugin support by default as well.

- `TikzCD` - similar to the `ImgTex` plugin in `gitit`, but for the `tikz-cd` LaTeX package. Uses `dvisvgm`.
- The normal gitit `Interwiki` plugin (but fixed to carry around the attrs of the links - I think this is due to a new version of pandoc-types)
- `DeadlinkDetection` - add the `deadlink` class to dead internal links (experimental, kind of a hack)

### Other features

- When creating a new page, displays search results for that page.

## Planned features

- A spaced-repetition memory system, where one uses commands embedded into pages to generate cards.

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
- Use gitit bootstrap(find link)
- Put TeX commands into the `content.st` template, in a `display: none` div right over `$content$`.
- Put `.deadlink {color: red;}` into `static/custom.css` (if you want to tag dead links).