# TODO

- **Search index** for the generated HTML tree. Pandoc has no built-in
  search; options to evaluate: lunr.js index baked at generate time, or
  an Elasticlunr-compatible JSON sidecar consumed by a small static
  page.
- **Filter sugar in `query`**: glob / regex modes for `--match`;
  `--since FILE` filtering by source file.
