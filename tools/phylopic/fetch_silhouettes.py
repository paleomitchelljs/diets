#!/usr/bin/env python3
"""Fetch one PhyloPic silhouette per clade for the talk site.

Only images that need no attribution (CC0 or Public Domain Mark) are taken, because the site is public.
For each clade the candidate names are tried in order and the first hit is used.
Writes static/img/phylopic/<key>.svg, static/img/phylopic/credits.csv, and static/data/silhouettes.js
(window.SIL = {key: {vb: viewBox, w, h, body: svg markup without fills}}), which the site draws in
the clade's colour. Run from the repository root: python3 tools/phylopic/fetch_silhouettes.py
"""
import csv, json, re, subprocess, sys, urllib.parse

API = "https://api.phylopic.org"
CLADES = {  # key (minor clade as in the site data, or a theropod group) -> names to try, in order
    "Accipitriformes": ["buteo jamaicensis", "buteo", "accipiter", "aquila"],
    "Strigiformes": ["strix aluco", "strix", "bubo", "tyto"],
    "Falconiformes": ["falco peregrinus", "falco"],
    "Passeriformes": ["lanius excubitor", "lanius", "corvus"],
    "Charadriiformes": ["larus", "stercorarius", "laridae"],
    "Pelecaniformes": ["ardea", "egretta", "ardeidae"],
    "Crocodylia": ["crocodylus niloticus", "crocodylus porosus", "crocodylus", "caiman", "alligator"],
    "Caniformia": ["vulpes vulpes", "vulpes", "canis"],
    "Feliformia": ["lynx", "felis", "panthera"],
    "Dasyuromorphia": ["sarcophilus", "dasyurus", "dasyuridae"],
    "Serpentes": ["morelia", "python", "pantherophis", "crotalus", "thamnophis", "coluber", "serpentes"],
    "Varanidae": ["varanus komodoensis", "varanus"],
    "Teiioidea": ["salvator merianae", "salvator", "tupinambis", "teiidae"],
    "Iguania": ["anolis", "iguana", "sceloporus"],
    "Tyrannosauridae": ["gorgosaurus", "daspletosaurus", "tyrannosaurus"],
    "Compsognathidae": ["compsognathus", "sinosauropteryx", "sinocalliopteryx"],
    "Paraves (non-avian)": ["microraptor", "velociraptor", "anchiornis"],
}

def get(url):  # curl, so the system certificate store is used
    return subprocess.run(["curl", "-sS", "-L", "-m", "30", "-A", "talk-site-silhouette-fetch", url], check=True, capture_output=True).stdout

build = json.loads(get(API + "/"))["build"]
credits, sil = [], {}
for key, names in CLADES.items():
    hit = None
    for nmq in names:
        q = urllib.parse.urlencode({"build": build, "filter_name": nmq, "filter_license_by": "false", "embed_items": "true", "page": 0})
        try:
            items = json.loads(get(f"{API}/images?{q}"))["_embedded"]["items"]
        except Exception as e:
            print(key, nmq, "query failed:", e, file=sys.stderr); continue
        if items:
            hit = (nmq, items[0]); break
    if not hit:
        print(key, "NO IMAGE", file=sys.stderr); continue
    nmq, it = hit
    L = it["_links"]
    svg = get(L["vectorFile"]["href"]).decode("utf-8")
    fname = re.sub(r"[^A-Za-z]+", "_", key).strip("_") + ".svg"
    open("static/img/phylopic/" + fname, "w").write(svg)
    vb = re.search(r'viewBox="([^"]+)"', svg)
    body = re.search(r"<svg[^>]*>(.*)</svg>", svg, re.S).group(1)
    body = re.sub(r"<(metadata|title|desc)\b.*?</\1>", "", body, flags=re.S)
    body = re.sub(r'\s(fill|stroke)="[^"]*"', "", body)
    body = re.sub(r"\s+", " ", body).strip()
    if vb:
        x, y, w, h = [float(v) for v in vb.group(1).replace(",", " ").split()]
    else:
        w = float(re.search(r'width="([\d.]+)', svg).group(1)); h = float(re.search(r'height="([\d.]+)', svg).group(1)); x = y = 0
    sil[key] = {"vb": f"{x:g} {y:g} {w:g} {h:g}", "w": w, "h": h, "body": body}
    credits.append({"clade": key, "searched": nmq, "taxon": L["specificNode"]["title"], "contributor": L["contributor"]["title"],
                    "license": L["license"]["href"], "uuid": it["uuid"], "page": "https://www.phylopic.org/images/" + it["uuid"], "file": fname})
    print(f"{key}: {L['specificNode']['title']} by {L['contributor']['title']} ({L['license']['href']})")

with open("static/img/phylopic/credits.csv", "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(credits[0].keys())); w.writeheader(); w.writerows(credits)
open("static/data/silhouettes.js", "w").write("window.SIL = " + json.dumps(sil) + ";\n")
print(f"build {build}: {len(sil)} silhouettes, {sum(len(v['body']) for v in sil.values()) // 1024} kB")
