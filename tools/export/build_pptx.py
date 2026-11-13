"""Assemble the talk as a .pptx from tools/export/capture.mjs output.

One pptx slide per build (a click on the site is a slide advance in PowerPoint), the
captured PNG or GIF full-bleed, the build's narration as speaker notes, the slide title as
the picture's alt text and as a hidden title (for the outline and screen readers).
GIFs play in PowerPoint's slideshow.

Usage: python3 tools/export/build_pptx.py [--out export] [--pptx export/talk.pptx]
"""
import argparse
import json
from pathlib import Path

from pptx import Presentation
from pptx.util import Emu

ap = argparse.ArgumentParser()
ap.add_argument("--out", default="export")
ap.add_argument("--pptx", default=None)
a = ap.parse_args()
out = Path(a.out)
man = json.loads((out / "manifest.json").read_text())
dest = Path(a.pptx) if a.pptx else out / "talk.pptx"

prs = Presentation()
prs.slide_width, prs.slide_height = Emu(12192000), Emu(6858000)          # 13.333 x 7.5 in, 16:9
blank = prs.slide_layouts[6]
title_only = prs.slide_layouts[5]

n = 0
for si, sl in enumerate(man["slides"]):
    for bi, bd in enumerate(sl["builds"]):
        slide = prs.slides.add_slide(title_only)
        # keep a real title (outline view, accessibility) but move it off the visible area
        t = slide.shapes.title
        t.text = bd["title"] or f"Slide {si + 1}"
        t.left, t.top, t.width, t.height = Emu(0), Emu(-1371600), prs.slide_width, Emu(914400)
        pic = slide.shapes.add_picture(str(out / bd["media"]), 0, 0, prs.slide_width, prs.slide_height)
        pic._element.nvPicPr.cNvPr.set("descr", bd["title"] or "")
        slide.notes_slide.notes_text_frame.text = bd["notes"] or ""
        n += 1

prs.save(dest)
print(f"wrote {dest}: {n} slides from {len(man['slides'])} screens")
