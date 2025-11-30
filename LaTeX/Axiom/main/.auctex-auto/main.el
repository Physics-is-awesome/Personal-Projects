;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("book" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "width=4.375in" "height=7.0in" "top=1.0in" "papersize={5.5in,8.5in}") ("amsmath" "") ("epigraph" "") ("amssymb" "") ("setspace" "") ("tipa" "") ("tgbonum" "") ("dirtree" "") ("hyperref" "hidelinks") ("textcomp" "") ("amsthm" "") ("array" "") ("xy" "") ("fancyhdr" "") ("listings" "") ("xcolor" "") ("tcolorbox" "") ("graphicx" "pdftex" "") ("titlesec" "")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "Intro"
    "../text/part_1/part_1_intro"
    "../text/part_1/logic"
    "../text/part_1/mathematics"
    "../text/part_1/metaphysics"
    "../text/part_1/Epistemology"
    "../text/part_1/science"
    "../text/part_1/phil_phys"
    "../text/part_1/physics"
    "../text/part_1/math"
    "../text/part_1/computations"
    "../text/part_1/theology"
    "../text/part_1/ethics"
    "../text/part_1/social"
    "../text/part_1/personal"
    "../text/part_2/intro"
    "../text/part_2/textbook"
    "../text/part_2/video"
    "../text/part_2/coding"
    "../text/part_3/intro"
    "../text/part_3/prev_work"
    "../text/part_3/Hamiltonian"
    "../text/part_3/Omega-X"
    "../text/part_3/Axiom"
    "../text/part_4/intro"
    "../text/part_5/intro"
    "../text/part_5/plans"
    "../text/part_5/personal_analysis"
    "book"
    "bk12"
    "geometry"
    "graphicx"
    "amsmath"
    "epigraph"
    "amssymb"
    "setspace"
    "tipa"
    "tgbonum"
    "dirtree"
    "hyperref"
    "textcomp"
    "amsthm"
    "array"
    "xy"
    "fancyhdr"
    "listings"
    "xcolor"
    "tcolorbox"
    "titlesec")
   (TeX-add-symbols
    '("sectionmark" 1)
    '("chaptermark" 1))
   (LaTeX-add-pagestyles
    "plain")
   (LaTeX-add-counters
    "subsubsubsection")
   (LaTeX-add-amsthm-newtheorems
    "axiomthm"
    "definition"
    "theorem")
   (LaTeX-add-listings-lstdefinestyles
    "FORTRAN")
   (LaTeX-add-xcolor-definecolors
    "desertbackground"
    "desertcomment"
    "desertkeyword"
    "desertstring"
    "deserttext"))
 :latex)

