# LLM Project

## Overview
The LLM project implements a Retrieval-Augmented Generation (RAG) model designed to process and extract information from PDF documents. This project focuses on human nutrition, utilizing a specific PDF document as the primary data source.

## Project Structure
```
llm-project
├── LLM
│   ├── RAG
│   │   ├── rag.py               # Main logic for the RAG model
│   │   ├── __init__.py          # Package initialization for RAG
│   │   ├── text_formatter.py     # Text formatting utilities
│   │   └── pdf_reader.py        # PDF reading and text extraction
│   ├── models
│   │   ├── __init__.py          # Package initialization for models
│   │   └── retriever.py         # Implementation of the retriever model
│   └── data
│       └── human-nutrition-text.pdf # PDF data source
├── notebooks
│   └── 00-exploration.ipynb     # Jupyter notebook for exploration and analysis
├── tests
│   └── test_rag.py              # Unit tests for RAG functionality
├── requirements.txt              # Project dependencies
├── .gitignore                    # Files and directories to ignore in Git
└── README.md                     # Project documentation
```

## Installation
To set up the project, clone the repository and install the required dependencies:

```bash
git clone <repository-url>
cd llm-project
pip install -r requirements.txt
```

## Usage
1. **Download the PDF**: The `rag.py` script will automatically download the human nutrition PDF if it does not exist in the `data` directory.
2. **Extract Text**: Use the functions in `rag.py` to read the PDF and extract text, statistics, and other relevant information.
3. **Run Tests**: Ensure the functionality is working as expected by running the tests in `test_rag.py`.

## Contributing
Contributions are welcome! Please submit a pull request or open an issue for any enhancements or bug fixes.

## License
This project is licensed under the MIT License. See the LICENSE file for more details.