\RequirePackage[l2tabu,orthodox]{nag}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{notes}[Personal package for typesetting notes]

\LoadClass[11pt]{article}

\RequirePackage{lmodern}
\RequirePackage{microtype}
\RequirePackage{geometry}
\RequirePackage{titlesec}
\RequirePackage{placeins}
\RequirePackage{bookmark}

\RequirePackage{math}

% Customary to load last for some reason
\RequirePackage{hyperref}

\geometry{paper=letterpaper,margin=3.5cm}
\titleformat{\section}{\FloatBarrier\Large\bfseries}{\thetitle.}%
{1ex}{}
\titleformat{\subsection}{\FloatBarrier\large\bfseries}{\thetitle.}%
{1ex}{}
\titleformat{\subsubsection}{\FloatBarrier\normalfont\bfseries}{\thetitle.}
{1ex}{}
\titleformat{\subsubsubsection}{\FloatBarrier\normalfont\itshape}{\thetitle.}%
{1ex}{}

\bookmarksetup{numbered,open,openlevel=2}
\hypersetup{colorlinks=true}
\mathtoolsset{showonlyrefs,showmanualtags}

\newcommand{\lecture}[3][Lecture]{\vspace{-\baselineskip}%
  \noindent\makebox[\linewidth]{\rule[-4pt]{\paperwidth}{0.15pt}}%
  \addcontentsline{toc}{lecture}{\texorpdfstring{}{\ \ }#1 #2: #3}
  \marginpar{#1 #2\\#3}\vspace{-\baselineskip}}

\newcommand*{\l@lecture}[2]{%
  \ifnum \c@tocdepth >\z@
    \addpenalty\@secpenalty
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \itshape
      \advance\leftskip\@tempdima
      \hskip -2\leftskip
      #1\nobreak\hfil
      \nobreak\hb@xt@\@pnumwidth{\hss #2%
                                 \kern-\p@\kern\p@}\par
    \endgroup
  \fi}

\renewcommand*{\l@section}[2]{%
  \ifnum \c@tocdepth >\z@
    \addpenalty\@secpenalty
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \bfseries
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      #1\nobreak\hfil
      \nobreak\hb@xt@\@pnumwidth{\hss #2%
                                 \kern-\p@\kern\p@}\par
    \endgroup
  \fi}

\def\toclevel@lecture{1}  % section

\newcommand{\maketoc}{\tableofcontents\clearpage}


\numberwithin{equation}{section}
