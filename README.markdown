See [jgm/gitit](https://github.com/jgm/gitit) for info on `gitit`

In this fork, I've added code to compile certain plugins, rather than loading them at compile time.
These are placed in `src/Network/Gitit/Plugin/`, and put into the `compiledPlugins` list in `Plugins.hs`
This structure was shamelessly stolen from [Jeff Johnson's fork](https://github.com/jefdaj/gitit).

The actual plugins are
- `TikzCD` - similar to the `ImgTex` plugin in `gitit`, but for the `tikz-cd` LaTeX package. Uses `dvisvgm`.
- The normal gitit `Interwiki` plugin.