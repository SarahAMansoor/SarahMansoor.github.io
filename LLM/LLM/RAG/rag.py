# filepath: /llm-project/LLM/RAG/rag.py

import os
import requests
import fitz
from tqdm.auto import tqdm
import pandas as pd
from spacy.lang.en import English

def download_pdf(pdf_path: str, url: str) -> None:
    """Downloads a PDF file from a given URL if it doesn't already exist."""
    if not os.path.exists(pdf_path):
        print("File doesn't exist, downloading...")
        response = requests.get(url)
        if response.status_code == 200:
            with open(pdf_path, "wb") as file:
                file.write(response.content)
            print(f"The file has been downloaded and saved as {pdf_path}")
        else:
            print(f"Failed to download the file. Status code: {response.status_code}")
    else:
        print(f"File {pdf_path} exists.")

def text_formatter(text: str) -> str:
    """Performs minor formatting on text."""
    cleaned_text = text.replace("\n", " ").strip()
    return cleaned_text

def open_and_read_pdf(pdf_path: str) -> list[dict]:
    """Opens a PDF file, reads its text content page by page, and collects statistics."""
    doc = fitz.open(pdf_path)
    pages_and_texts = []
    for page_number, page in tqdm(enumerate(doc)):
        text = page.get_text()
        text = text_formatter(text)
        pages_and_texts.append({
            "page_number": page_number - 41,
            "page_char_count": len(text),
            "page_word_count": len(text.split(" ")),
            "page_sentence_count_raw": len(text.split(". ")),
            "page_token_count": len(text) / 4,
            "text": text
        })
    return pages_and_texts

def main():
    pdf_path = "data/human-nutrition-text.pdf"
    url = "https://pressbooks.oer.hawaii.edu/humannutrition2/open/download?type=pdf"
    download_pdf(pdf_path, url)
    pages_and_texts = open_and_read_pdf(pdf_path=pdf_path)
    df = pd.DataFrame(pages_and_texts)
    print(df.head())
    print(df.describe().round(2))

if __name__ == "__main__":
    main()