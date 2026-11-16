# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

import os
import sys

# Our own extensions, in docs/_ext (fractalstream_lexer: syntax
# highlighting for ``.. code-block:: fractalstream``)
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '_ext')))

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'FractalStream'
copyright = '2026, Matt Noonan'
author = 'Matt Noonan'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = ['fractalstream_lexer']

templates_path = ['_templates']
exclude_patterns = []



# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'lumina'
html_static_path = ['_static']
html_baseurl = 'https://FractalStream.readthedocs.io/'
