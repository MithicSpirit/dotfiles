\RequirePackage[l2tabu,orthodox]{nag}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{mathhw}%
[2023/03/20 (WIP) Personal package for typesetting mathematics homework]

\LoadClass[11pt,titlepage]{article}

\RequirePackage{lmodern}
\RequirePackage{microtype}
\RequirePackage{geometry}
\RequirePackage{titlesec}
\RequirePackage{placeins}

\RequirePackage{math}

% Customary to load last for some reason
\RequirePackage[colorlinks]{hyperref}

\geometry{paper=letterpaper,margin=3.5cm}
\newcommand\sectionbreak{\clearpage}
\renewcommand\thesubsection{\alph{subsection}}
\titleformat{\section}{\FloatBarrier\normalfont\large\bfseries}%
{(\thetitle)}{1em}{}
\titleformat{\subsection}{\FloatBarrier\normalfont\normalsize\bfseries}%
{(\thetitle)}{1em}{}
