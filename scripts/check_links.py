#!/usr/bin/env python3
"""Static link / anchor check for a generated docs tree.

Walks every .html file under the given directory, parses anchor hrefs,
and verifies each internal link points at an existing file and (if the
href has a fragment) an existing id on that file. External links
(http://, https://, mailto:) are skipped.

Exits non-zero if anything is broken; prints a summary either way.

    scripts/check_links.py                        # default: output/vanilla
    scripts/check_links.py output/docking
    scripts/check_links.py output/vanilla --max 50
"""

import argparse
import os
import sys
from html.parser import HTMLParser
from urllib.parse import unquote, urldefrag


class _Collector(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.hrefs: list[str] = []
        self.ids: list[str] = []

    def handle_starttag(self, tag, attrs):
        a = dict(attrs)
        href = a.get("href")
        if href is not None:
            self.hrefs.append(href)
        ident = a.get("id")
        if ident is not None:
            self.ids.append(ident)


def _parse(path: str) -> _Collector:
    with open(path, encoding="utf-8") as fh:
        c = _Collector()
        c.feed(fh.read())
    return c


def check(root: str) -> tuple[int, int, list[str]]:
    """Return (pages, links_checked, broken_messages)."""
    pages: list[str] = []
    ids_in: dict[str, set[str]] = {}
    cache: dict[str, _Collector] = {}

    for dirpath, _, files in os.walk(root):
        for f in files:
            if f.endswith(".html"):
                full = os.path.join(dirpath, f)
                pages.append(full)
                c = _parse(full)
                cache[full] = c
                ids_in[full] = set(c.ids)

    broken: list[str] = []
    total = 0
    for page in pages:
        c = cache[page]
        for href in c.hrefs:
            if href.startswith(("http://", "https://", "mailto:")):
                continue
            total += 1
            if href.startswith("#"):
                anchor = unquote(href[1:])
                if anchor and anchor not in ids_in[page]:
                    broken.append(f"{page}: same-page anchor #{anchor} missing")
                continue
            url, frag = urldefrag(href)
            target = os.path.normpath(os.path.join(os.path.dirname(page), unquote(url)))
            if os.path.isdir(target):
                target = os.path.join(target, "index.html")
            if not os.path.exists(target):
                broken.append(f"{page}: link to missing file {url} (resolved {target})")
                continue
            if frag:
                frag = unquote(frag)
                if frag not in ids_in.get(target, set()):
                    broken.append(f"{page}: anchor #{frag} missing in {target}")

    return len(pages), total, broken


def main() -> int:
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    p.add_argument(
        "root",
        nargs="?",
        default="output/vanilla",
        help="directory to crawl (default: output/vanilla)",
    )
    p.add_argument(
        "--max",
        type=int,
        default=20,
        metavar="N",
        help="max number of broken-link messages to print (default: 20)",
    )
    args = p.parse_args()

    if not os.path.isdir(args.root):
        print(f"error: {args.root} is not a directory", file=sys.stderr)
        return 2

    pages, total, broken = check(args.root)
    print(f"pages: {pages}, links checked: {total}, broken: {len(broken)}")
    for b in broken[: args.max]:
        print(b)
    if len(broken) > args.max:
        print(f"... ({len(broken) - args.max} more)")
    return 0 if not broken else 1


if __name__ == "__main__":
    sys.exit(main())
