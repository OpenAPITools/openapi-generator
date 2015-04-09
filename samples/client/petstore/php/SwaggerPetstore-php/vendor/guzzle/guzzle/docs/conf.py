import sys, os
from sphinx.highlighting import lexers
from pygments.lexers.web import PhpLexer

lexers['php'] = PhpLexer(startinline=True, linenos=1)
lexers['php-annotations'] = PhpLexer(startinline=True, linenos=1)
primary_domain = 'php'

# -- General configuration -----------------------------------------------------

extensions = []
templates_path = ['_templates']
source_suffix = '.rst'
master_doc = 'index'

project = u'Guzzle'
copyright = u'2012, Michael Dowling'
version = '3.0.0'
release = '3.0.0'

exclude_patterns = ['_build']

# -- Options for HTML output ---------------------------------------------------

# The name for this set of Sphinx documents.  If None, it defaults to
# "<project> v<release> documentation".
html_title = "Guzzle documentation"
html_short_title = "Guzzle"

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

# Custom sidebar templates, maps document names to template names.
html_sidebars = {
    '**':       ['localtoc.html', 'leftbar.html', 'searchbox.html']
}

# Output file base name for HTML help builder.
htmlhelp_basename = 'Guzzledoc'

# -- Guzzle Sphinx theme setup ------------------------------------------------

sys.path.insert(0, '/Users/dowling/projects/guzzle_sphinx_theme')

import guzzle_sphinx_theme
html_translator_class = 'guzzle_sphinx_theme.HTMLTranslator'
html_theme_path = guzzle_sphinx_theme.html_theme_path()
html_theme = 'guzzle_sphinx_theme'

# Guzzle theme options (see theme.conf for more information)
html_theme_options = {
    "index_template": "index.html",
    "project_nav_name": "Guzzle",
    "github_user": "guzzle",
    "github_repo": "guzzle",
    "disqus_comments_shortname": "guzzle",
    "google_analytics_account": "UA-22752917-1"
}

# -- Options for LaTeX output --------------------------------------------------

latex_elements = {}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title, author, documentclass [howto/manual]).
latex_documents = [
  ('index', 'Guzzle.tex', u'Guzzle Documentation',
   u'Michael Dowling', 'manual'),
]

# -- Options for manual page output --------------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    ('index', 'guzzle', u'Guzzle Documentation',
     [u'Michael Dowling'], 1)
]

# If true, show URL addresses after external links.
#man_show_urls = False

# -- Options for Texinfo output ------------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
  ('index', 'Guzzle', u'Guzzle Documentation',
   u'Michael Dowling', 'Guzzle', 'One line description of project.',
   'Miscellaneous'),
]
