# VisFrame

Code for *VisTopics: A novel approach to identifying protest frames in news images*.

The repository contains the code for a step-by-step workflow that identifies visual frames in news images. The workflow captions images with a vision-language model, applies LDA topic modeling to the captions, groups topics into frame clusters with Analysis of Topic Model Networks (ANTMN), and applies an existing codebook to the same images through a structured prompt. The demonstration case is New York Times and New York Post coverage of the 2025 No Kings and anti-ICE protests (June 1 to December 31, 2025).

## Repository structure

```
VisFrame/
├── code/
│   ├── 01_collect_filter_model.R      # Stages 1–3 and 5: collection, filtering, preprocessing,
│   │                                  #   hyperparameter search, LDA, ANTMN, outlet tests
│   ├── 02_download_and_caption.py     # Stage 2: image download and captioning (VisTopics)
│   └── 03_kimchen_coding.py           # Stage 4: Kim & Chen (2026) codebook via prompt
├── LICENSE
└── README.md
```

## Run order

The scripts share intermediate files, so they run in this order. R scripts run from `code/`; Python scripts run from the repository root. The scripts create local `data/` and `output/` folders for intermediate files and results; these are not part of the repository.

1. **`code/01_collect_filter_model.R`, up to `# Load Captions and Merge`.** Queries the NewsWhip API one day at a time, removes duplicate articles, applies the headline filter and the logo/placeholder blocklist, and writes `output/VisFrame_-to-scrape.csv`.
2. **`code/02_download_and_caption.py`.** Downloads the images (rerun to retry failed URLs) and captions them with `get_caption()`, writing `output/captions_file.csv`.
3. **`code/01_collect_filter_model.R`, from `# Load Captions and Merge` to `# Supervised Coding Setup`.** Merges captions, preprocesses, runs the α and k searches with ldaOptim, fits the k = 19 model, builds the topic network, and copies the top 40 images for four topics into `code/test_images/`.
4. **`code/03_kimchen_coding.py`.** Applies the codebook prompt to those images and writes `output/test_out_topic*_kimchen.csv`.
5. **`code/01_collect_filter_model.R`, remaining sections.** Frame-cluster prevalence over time, codebook results (Table 1), and outlet comparisons (Tables A.4 and B.1).

The script contains a manual check after the first stopword pass (`# STOP HERE`): inspect the top terms, add caption boilerplate to the stopword list, and rerun before continuing.

## Data

The data are not included. The news images are copyrighted by the publishing outlets, image captions may name identifiable individuals, and article metadata come from the NewsWhip API under its terms of service. Rerunning the workflow requires a NewsWhip API key (step 1) and an OpenAI API key (steps 2 and 4).

## Requirements

- **R** (≥ 4.3) with: tidyverse, readxl, openxlsx, quanteda, tidytext, cld2, stringi, lubridate, doParallel, igraph, magick, daiR, ggpubr, broom, httr, jsonlite, lsa, corpustools, topicmodels, and [ldaOptim](repository link removed for review) (v0.1.0)
- **Python** (≥ 3.9) with [VisTopics](repository link removed for review) (v0.1.10): `pip install vistopics`
- API keys set as environment variables: `NEWSWHIP_API_KEY` and `OPENAI_API_KEY`

## Model

Captions and codebook codes were generated with OpenAI's `gpt-4o-mini` through the VisTopics `get_caption()` function. Captions use the default VisTopics prompt (v0.1.10); the codebook prompt is in `code/03_kimchen_coding.py` and Appendix B.1. Model outputs can change between versions; rerunning the captioning step with a different model or model version will produce different captions and topics.

## License

Code is released under the MIT License.