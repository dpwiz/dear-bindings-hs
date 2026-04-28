# TODO

- **Search index** for the generated HTML tree. Pandoc has no built-in
  search; options to evaluate: lunr.js index baked at generate time, or
  an Elasticlunr-compatible JSON sidecar consumed by a small static
  page.
- **Cross-link substitution**: in any signature/declaration, turn
  user-defined type names (struct / enum / typedef) into relative links
  to their entity pages. Symbol table comes for free from the merged
  catalog. Anchor scheme is already in place
  (`<category-prefix>-<name>`).
- **Filter sugar in `query`**: glob / regex modes for `--match`;
  `--since FILE` filtering by source file.
