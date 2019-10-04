See [jgm/gitit](https://github.com/jgm/gitit) for info on `gitit`

In this fork, I've added code to compile certain plugins, rather than loading them at compile time.
These are placed in `src/Network/Gitit/Plugin/`, and put into the `compiledPlugins` list in `Plugins.hs`
This structure was shamelessly stolen from [Jeff Johnson's fork](https://github.com/jefdaj/gitit).
See [this issue](https://github.com/jgm/gitit/issues/599).

If you're using this approach, note that you need to add your plugins to exposed-modules in `gitit.cabal` (see mine for example).
In my `.cabal` file, I've disabled the ordinary plugin support by default as well.

- `TikzCD` - similar to the `ImgTex` plugin in `gitit`, but for the `tikz-cd` LaTeX package. Uses `dvisvgm`.
- The normal gitit `Interwiki` plugin (but fixed to carry around the attrs of the links - I think this is due to a new version of pandoc-types)
