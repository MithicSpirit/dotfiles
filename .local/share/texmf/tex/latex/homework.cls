\RequirePackage[l2tabu,orthodox]{nag}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{homework}[Personal package for typesetting homework]

\LoadClass[11pt,titlepage]{article}

\RequirePackage{lmodern}
\RequirePackage{microtype}
\RequirePackage{geometry}
\RequirePackage{titlesec}
\RequirePackage{placeins}

\RequirePackage{math}

% Customary to load last for some reason
\RequirePackage{hyperref}

\geometry{paper=letterpaper,margin=3.5cm}
\hypersetup{colorlinks=true,bookmarksnumbered}
\mathtoolsset{showonlyrefs,showmanualtags}

\newcommand{\sectionbreak}{\clearpage}
\renewcommand{\thesection}{\arabic{section}}
\renewcommand{\thesubsection}{\alph{subsection}}
\renewcommand{\thesubsubsection}{\roman{subsubsection}}

\titleformat{\section}{\FloatBarrier\normalfont\large\bfseries}%
{\thetitle.}{1em}{}
\titleformat{\subsection}{\FloatBarrier\normalfont\normalsize\bfseries}%
{(\thetitle)}{1em}{}
\titleformat{\subsubsection}[runin]{\FloatBarrier\normalfont\bfseries}%
{\thetitle.}{0pt}{}

\allowdisplaybreaks{}
