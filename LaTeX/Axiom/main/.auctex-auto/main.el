;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("book" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("amsmath" "") ("epigraph" "") ("amssymb" "") ("setspace" "") ("tipa" "") ("tgbonum" "") ("dirtree" "") ("hyperref" "hidelinks") ("textcomp" "") ("amsthm" "") ("array" "") ("xy" "") ("fancyhdr" "") ("listings" "") ("xcolor" "") ("tcolorbox" "") ("graphicx" "pdftex" "") ("titlesec" "") ("fontenc" "T3" "\\@defaultenc") ("geometry" "width=4.375in" "height=7.0in" "top=1.0in" "papersize={5.5in,8.5in}" "width=5.0in" "papersize={7.0in,8.5in}") ("preamble" "") ("my_math" "")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
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
    "../text/part_2/Calculus_of_variations"
    "../text/part_2/quantum_field_theory"
    "../text/part_2/num_ham_prob"
    "../text/part_2/General_Relativity_Wald"
    "../text/part_2/Assumptions_of_physics"
    "../text/part_2/coding"
    "../text/part_2/Bible"
    "../text/part_3/intro"
    "../text/part_3/prev_work"
    "../text/part_3/Omega-X"
    "../text/part_3/Axiom"
    "../text/part_3/Linux"
    "../text/part_3/emacs"
    "../text/part_3/Category_metriplectic"
    "../text/part_3/website"
    "../text/part_4/intro"
    "../text/part_4/development"
    "../text/part_4/Metaphysics"
    "../text/part_4/Epistemology"
    "../text/part_4/ethics"
    "../text/part_4/personal_phil"
    "../text/part_5/personal_analysis"
    "../text/part_5/self-authorship"
    "../text/part_3/Hamiltonian"
    "../text/part_5/intro"
    "../text/part_5/plans"
    "../text/part_5/journal_intro"
    "../text/part_5/daily-notes"
    "book"
    "bk12"
    "geometry"
    "preamble"
    "my_math"))
 :latex)

