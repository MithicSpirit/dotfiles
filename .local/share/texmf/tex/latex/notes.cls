\RequirePackage[l2tabu,orthodox]{nag}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{notes}%
[2023/03/22 (WIP) Personal package for typesetting mathematics notes]

\LoadClass[11pt]{article}

\RequirePackage{lmodern}
\RequirePackage{microtype}
\RequirePackage{geometry}
\RequirePackage{titlesec}
\RequirePackage{placeins}

\RequirePackage{math}

% Customary to load last for some reason
\RequirePackage[colorlinks]{hyperref}

\geometry{paper=letterpaper,margin=3.5cm}
\titleformat{\section}{\FloatBarrier\Large\bfseries}{}{0pt}{}
\titleformat{\subsection}{\FloatBarrier\large\bfseries}{}{0pt}{}
\titleformat{\subsubsection}{\FloatBarrier\normalfont\bfseries}{}{0pt}{}
\titleformat{\subsubsubsection}{\FloatBarrier\normalfont\itseries}{}{0pt}{}
