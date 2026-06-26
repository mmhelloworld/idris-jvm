# Documentation

The Idris JVM documentation is built with [Sphinx](https://www.sphinx-doc.org/)
from the reStructuredText sources under [`source/`](source/).

## Setup

Install Sphinx and the theme listed in [`requirements.txt`](requirements.txt):

```bash
pip install sphinx sphinx_rtd_theme
```

Using a virtual environment is recommended:

```bash
python -m venv .venv
source .venv/bin/activate
pip install sphinx sphinx_rtd_theme
```

## Build

From the `docs/` directory:

```bash
make html
```

The generated site is written to `build/html/`. Open the entry point in a
browser:

```bash
open build/html/index.html        # macOS
xdg-open build/html/index.html    # Linux
```

## Live reload

For repeated edits, [`sphinx-autobuild`](https://github.com/sphinx-doc/sphinx-autobuild)
rebuilds and refreshes the browser on save:

```bash
pip install sphinx-autobuild
sphinx-autobuild source build/html
```

Then open <http://127.0.0.1:8000>.

## Check before pushing

Build with warnings treated as errors to catch broken references, malformed
tables, and bad links - the same enforcement CI applies:

```bash
make clean
sphinx-build -W -b html source build/html
```
