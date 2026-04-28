#!/usr/bin/env python3
"""Download latest dear_bindings releases (vanilla + docking) into a target directory."""

raise Exception("XXX: Do not use, the releases are missing critical files (?)")
please_dont()

import argparse
import json
import os
import sys
import urllib.request
from pathlib import Path

REPO = "dearimgui/dear_bindings"
API = f"https://api.github.com/repos/{REPO}/releases"


def http_get(url: str, accept: str = "application/vnd.github+json") -> bytes:
    req = urllib.request.Request(url, headers={
        "Accept": accept,
        "User-Agent": "pull-dear-bindings",
        "X-GitHub-Api-Version": "2022-11-28",
    })
    token = os.environ.get("GITHUB_TOKEN") or os.environ.get("GH_TOKEN")
    if token:
        req.add_header("Authorization", f"Bearer {token}")
    with urllib.request.urlopen(req) as resp:
        return resp.read()


def latest_releases() -> tuple[dict, dict]:
    """Return (vanilla, docking) — the most recent release of each kind."""
    releases = json.loads(http_get(API))
    vanilla = docking = None
    for rel in releases:
        if rel.get("draft") or rel.get("prerelease"):
            continue
        is_docking = rel["tag_name"].endswith("-docking")
        if is_docking and docking is None:
            docking = rel
        elif not is_docking and vanilla is None:
            vanilla = rel
        if vanilla and docking:
            break
    if not vanilla or not docking:
        raise RuntimeError("Could not find both vanilla and docking releases")
    return vanilla, docking


def download_release(release: dict, dest: Path) -> None:
    dest.mkdir(parents=True, exist_ok=True)
    print(f"-> {release['tag_name']}  ->  {dest}")
    (dest / "RELEASE_TAG").write_text(release["tag_name"] + "\n")
    for asset in release["assets"]:
        out = dest / asset["name"]
        print(f"   {asset['name']} ({asset['size']} bytes)")
        data = http_get(asset["browser_download_url"], accept="application/octet-stream")
        out.write_bytes(data)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument(
        "target", nargs="?", default="dear_bindings",
        help="Target directory (default: ./dear_bindings)",
    )
    ap.add_argument(
        "--clean", action="store_true",
        help="Remove target subdirectories before downloading",
    )
    args = ap.parse_args()

    target = Path(args.target)
    vanilla, docking = latest_releases()

    for sub, rel in [("vanilla", vanilla), ("docking", docking)]:
        dest = target / sub
        if args.clean and dest.exists():
            for p in dest.iterdir():
                p.unlink()
        download_release(rel, dest)

    print("Done.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
