#!/usr/bin/env python3
"""Check that every internal link of the exported site resolves.

    check-links.py [SITE]        (SITE defaults to html-content)

A link is a page and maybe an anchor: the page must exist, and the
anchor must be on it -- a link to a heading since renamed is broken too.
Left alone: links with a scheme (https:, javascript:, which drives the
menu), and #top, which HTML defines as the top of any page.  Exits 1 if
anything is broken.

One script for every workflow that publishes or checks the site: the
check was copied into two, and one copy was fixed while the other,
the one a release runs, kept failing.
"""
import glob
import os
import re
import sys


def main(site):
    pages = glob.glob(os.path.join(site, '*.html'))
    bad, anchors = [], 0
    for f in pages:
        for href in re.findall(r'(?:href|src)="([^"]*)"', open(f, encoding='utf-8').read()):
            if not href or ':' in href.split('/')[0]:
                continue
            path, _, anchor = href.partition('#')
            target = os.path.normpath(os.path.join(site, path)) if path else f
            if not os.path.exists(target):
                bad.append((os.path.basename(f), href))
            elif anchor and anchor != 'top' and target.endswith('.html'):
                anchors += 1
                if f'id="{anchor}"' not in open(target, encoding='utf-8').read():
                    bad.append((os.path.basename(f), href + ' (no such anchor)'))
    for f, h in bad:
        print(f"broken: {f} -> {h}")
    print(f"{len(pages)} pages and {anchors} anchors checked, {len(bad)} broken")
    return 1 if bad or not pages else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else 'html-content'))
