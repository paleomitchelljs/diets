#!/bin/zsh
# Export the talk site (static/) to export/talk.pptx with GIFs for animated builds.
# Run after any change to the site or to static/data (Rscript code/site_export.R).
# Needs: Google Chrome, node >= 22, ffmpeg, python3 with python-pptx (pip install python-pptx).
set -e
cd "$(dirname $0)/../.."
rm -rf export/media
node tools/export/capture.mjs --site static/index.html --out export "$@"
python3 tools/export/build_pptx.py --out export
