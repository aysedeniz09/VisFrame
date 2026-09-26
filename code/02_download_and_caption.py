import os
import pandas as pd
from vistopics import download_images_from_url
from vistopics import get_caption

os.makedirs("output", exist_ok=True)  # local results folder (not shared)


# Run from the repository root.
# Step 1: download images listed in output/VisFrame_-to-scrape.csv
# (written by code/01_collect_filter_model.R). Rerun to retry failed URLs.
download_images_from_url(
    input_csv="output/VisFrame_-to-scrape.csv",
    output_csv="output/download_log.csv",
    image_dir="images",
    url_column="image_link",
    index_column="uuid",
    use_referer=True,
)

# Step 2: caption with the VisTopics default prompt (vistopics v0.1.10).

get_caption(
    mykey=os.environ["OPENAI_API_KEY"],
    path_in="images",
    captions_file="output/captions_file.csv",
    model="gpt-4o-mini"
)

