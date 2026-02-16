# Rollen S. D'Souza's Personal Website
Hi there, and welcome to the source code of my website.
The website is statically generated using [Hakyll](https://jaspervdj.be/hakyll/) which is a Haskell-based static website generator that leverages [Pandoc](https://pandoc.org/index.html).
Development of both the website and its content is done in a VS Code Dev Container that uses a Ubuntu Noble image as its base.

## Project Organization
The Hakyll entry point resides in `site.hs`.
All the generic, static-content rules exist there.
The other modules manage more complicated logic:
 - `Compiler.hs`: Implements a lot of the shared boiler-plate for managing compiler context across the website.
 - `Navigation.hs`: Implements data and logic for managing the navigation bar on all the pages.
 - `Posts.hs`: Implements all the post rules and includes logic like draft/publish mode.


