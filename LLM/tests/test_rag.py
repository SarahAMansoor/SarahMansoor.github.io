import pytest
from LLM.RAG.rag import text_formatter, open_and_read_pdf

def test_text_formatter():
    assert text_formatter("   Hello World!   ") == "Hello World!"
    assert text_formatter("Line 1.\nLine 2.") == "Line 1. Line 2."
    assert text_formatter("   ") == ""

def test_open_and_read_pdf():
    pdf_path = "LLM/data/human-nutrition-text.pdf"
    pages_and_texts = open_and_read_pdf(pdf_path)
    
    assert isinstance(pages_and_texts, list)
    assert len(pages_and_texts) > 0
    for page in pages_and_texts:
        assert "text" in page
        assert isinstance(page["text"], str)
        assert isinstance(page["page_number"], int)
        assert isinstance(page["page_char_count"], int)
        assert isinstance(page["page_word_count"], int)
        assert isinstance(page["page_sentence_count_raw"], int)
        assert isinstance(page["page_token_count"], float)