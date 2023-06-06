\RequirePackage[l2tabu,orthodox]{nag}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{labs}%
[2023/04/18 (WIP) Personal package for physics labs]

\LoadClass[11pt]{article}

\RequirePackage{lmodern}
\RequirePackage{microtype}
\RequirePackage{geometry}
\RequirePackage{titlesec}
\RequirePackage{float}
\RequirePackage{minted}
\RequirePackage{biblatex}

\RequirePackage{math}

% Customary to load last for some reason
\RequirePackage{hyperref}

\geometry{paper=letterpaper,margin=3.5cm}

\newcommand\maketoc{\clearpage\tableofcontents\clearpage\newcommand\sectionbreak{\clearpage}}
\newcommand\makebib{\clearpage\printbibliography[heading=bibintoc]\@ifstar{}{\appendix}}

\sisetup{
  retain-explicit-decimal-marker=true,
  retain-zero-uncertainty,
  uncertainty-mode=separate,
  table-alignment-mode=format
}

\pgfplotsset{enlargelimits=true}

\setminted{breaklines=true}
