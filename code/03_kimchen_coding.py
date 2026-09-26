# Run from the repository root, after code/01_collect_filter_model.R has copied
# the top 40 images per topic into code/test_images/.
## prompts derived from Kim, S. J., & Chen, L. (2026). Visual Framing at Scale: A Theory-Driven Computational Framework for Analyzing Protest Imagery with Generative AI. Computational Communication Research, 8(2), 1. https://doi.org/10.5117/CCR2026.2.3.KIM

import os
import csv
from vistopics import get_caption

os.makedirs("output", exist_ok=True)  # local results folder (not shared)


# Top 40 images for each of the four coded topics, each in its own folder
# so get_caption() has a clean directory to scan.
TOPIC_FOLDERS = {
    "topic4": "code/test_images/topic4",   # police confrontation
    "topic6": "code/test_images/topic6",   # legitimacy / voice
    "topic11": "code/test_images/topic11", # riots / conflict
    "topic16": "code/test_images/topic16", # militarized / national guard
}

# Adapted from Kim & Chen (2026), Table 3: items restated in our own words with
# adapted coding values (1 = present, 2 = absent; EW and MO use three categories,
# and MO adds a "no police present" code). Output is a structured code string.
KIM_CHEN_PROMPT = (
    "Do not describe the scene in prose. Instead, code the image on the "
    "following six items and return only a single line in this exact format: "
    "PL:_;PT:_;VA:_;EW:_;SA:_;MO:_ (fill in each blank with the appropriate "
    "number). PL (police presence): 1 if police are present, 2 if not. "
    "PT (protester presence): 1 if protesters are present, 2 if not. "
    "VA (contestation): 1 if there is any confrontational or contested action "
    "between police and protesters (e.g., physical struggle, arrest, "
    "barricades, visible tension), 2 if not. "
    "EW (weapon): 1 if a police officer is holding a weapon, 2 if a "
    "protester is holding a weapon, 3 if no weapon is visible. "
    "SA (solidarity action): 1 if protesters show solidarity (e.g., linked "
    "arms, shared banners, chanting together, peaceful gathering), 2 if not. "
    "MO (masked officials): 1 if police are masked or their faces are not "
    "distinguishable, 2 if police are visible and unmasked, 3 if no police "
    "are present. "
    "Base every code only on what is directly visible; do not infer intent "
    "or name an overall frame."
)
 
 
def show_result(captions_file, label):
    print(f"\n--- {label} ---")
    if not os.path.exists(captions_file):
        print("  [no output written -- check for errors above]")
        return
    with open(captions_file, newline="") as f:
        for row in csv.reader(f):
            print(f"  {row}")
 
 
for topic_key, folder in TOPIC_FOLDERS.items():
    out_file = f"output/test_out_{topic_key}_kimchen.csv"
    get_caption(
        mykey=os.environ["OPENAI_API_KEY"],
        path_in=folder,
        captions_file=out_file,
        model="gpt-4o-mini",
        prompt=KIM_CHEN_PROMPT,
    )
    show_result(out_file, f"{topic_key} - Kim & Chen codebook-style codes")
