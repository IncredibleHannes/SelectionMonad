%% ODER: format ==         = "\mathrel{==}"
%% ODER: format /=         = "\neq "
%
%
\makeatletter
\@ifundefined{lhs2tex.lhs2tex.sty.read}%
  {\@namedef{lhs2tex.lhs2tex.sty.read}{}%
   \newcommand\SkipToFmtEnd{}%
   \newcommand\EndFmtInput{}%
   \long\def\SkipToFmtEnd#1\EndFmtInput{}%
  }\SkipToFmtEnd

\newcommand\ReadOnlyOnce[1]{\@ifundefined{#1}{\@namedef{#1}{}}\SkipToFmtEnd}
\usepackage{amstext}
\usepackage{amssymb}
\usepackage{stmaryrd}
\DeclareFontFamily{OT1}{cmtex}{}
\DeclareFontShape{OT1}{cmtex}{m}{n}
  {<5><6><7><8>cmtex8
   <9>cmtex9
   <10><10.95><12><14.4><17.28><20.74><24.88>cmtex10}{}
\DeclareFontShape{OT1}{cmtex}{m}{it}
  {<-> ssub * cmtt/m/it}{}
\newcommand{\texfamily}{\fontfamily{cmtex}\selectfont}
\DeclareFontShape{OT1}{cmtt}{bx}{n}
  {<5><6><7><8>cmtt8
   <9>cmbtt9
   <10><10.95><12><14.4><17.28><20.74><24.88>cmbtt10}{}
\DeclareFontShape{OT1}{cmtex}{bx}{n}
  {<-> ssub * cmtt/bx/n}{}
\newcommand{\tex}[1]{\text{\texfamily#1}}	% NEU

\newcommand{\Sp}{\hskip.33334em\relax}


\newcommand{\Conid}[1]{\mathit{#1}}
\newcommand{\Varid}[1]{\mathit{#1}}
\newcommand{\anonymous}{\kern0.06em \vbox{\hrule\@width.5em}}
\newcommand{\plus}{\mathbin{+\!\!\!+}}
\newcommand{\bind}{\mathbin{>\!\!\!>\mkern-6.7mu=}}
\newcommand{\rbind}{\mathbin{=\mkern-6.7mu<\!\!\!<}}% suggested by Neil Mitchell
\newcommand{\sequ}{\mathbin{>\!\!\!>}}
\renewcommand{\leq}{\leqslant}
\renewcommand{\geq}{\geqslant}
\usepackage{polytable}

%mathindent has to be defined
\@ifundefined{mathindent}%
  {\newdimen\mathindent\mathindent\leftmargini}%
  {}%

\def\resethooks{%
  \global\let\SaveRestoreHook\empty
  \global\let\ColumnHook\empty}
\newcommand*{\savecolumns}[1][default]%
  {\g@addto@macro\SaveRestoreHook{\savecolumns[#1]}}
\newcommand*{\restorecolumns}[1][default]%
  {\g@addto@macro\SaveRestoreHook{\restorecolumns[#1]}}
\newcommand*{\aligncolumn}[2]%
  {\g@addto@macro\ColumnHook{\column{#1}{#2}}}

\resethooks

\newcommand{\onelinecommentchars}{\quad-{}- }
\newcommand{\commentbeginchars}{\enskip\{-}
\newcommand{\commentendchars}{-\}\enskip}

\newcommand{\visiblecomments}{%
  \let\onelinecomment=\onelinecommentchars
  \let\commentbegin=\commentbeginchars
  \let\commentend=\commentendchars}

\newcommand{\invisiblecomments}{%
  \let\onelinecomment=\empty
  \let\commentbegin=\empty
  \let\commentend=\empty}

\visiblecomments

\newlength{\blanklineskip}
\setlength{\blanklineskip}{0.66084ex}

\newcommand{\hsindent}[1]{\quad}% default is fixed indentation
\let\hspre\empty
\let\hspost\empty
\newcommand{\NB}{\textbf{NB}}
\newcommand{\Todo}[1]{$\langle$\textbf{To do:}~#1$\rangle$}

\EndFmtInput
\makeatother
%
%
%
%
%
%
% This package provides two environments suitable to take the place
% of hscode, called "plainhscode" and "arrayhscode". 
%
% The plain environment surrounds each code block by vertical space,
% and it uses \abovedisplayskip and \belowdisplayskip to get spacing
% similar to formulas. Note that if these dimensions are changed,
% the spacing around displayed math formulas changes as well.
% All code is indented using \leftskip.
%
% Changed 19.08.2004 to reflect changes in colorcode. Should work with
% CodeGroup.sty.
%
\ReadOnlyOnce{polycode.fmt}%
\makeatletter

\newcommand{\hsnewpar}[1]%
  {{\parskip=0pt\parindent=0pt\par\vskip #1\noindent}}

% can be used, for instance, to redefine the code size, by setting the
% command to \small or something alike
\newcommand{\hscodestyle}{}

% The command \sethscode can be used to switch the code formatting
% behaviour by mapping the hscode environment in the subst directive
% to a new LaTeX environment.

\newcommand{\sethscode}[1]%
  {\expandafter\let\expandafter\hscode\csname #1\endcsname
   \expandafter\let\expandafter\endhscode\csname end#1\endcsname}

% "compatibility" mode restores the non-polycode.fmt layout.

\newenvironment{compathscode}%
  {\par\noindent
   \advance\leftskip\mathindent
   \hscodestyle
   \let\\=\@normalcr
   \let\hspre\(\let\hspost\)%
   \pboxed}%
  {\endpboxed\)%
   \par\noindent
   \ignorespacesafterend}

\newcommand{\compaths}{\sethscode{compathscode}}

% "plain" mode is the proposed default.
% It should now work with \centering.
% This required some changes. The old version
% is still available for reference as oldplainhscode.

\newenvironment{plainhscode}%
  {\hsnewpar\abovedisplayskip
   \advance\leftskip\mathindent
   \hscodestyle
   \let\hspre\(\let\hspost\)%
   \pboxed}%
  {\endpboxed%
   \hsnewpar\belowdisplayskip
   \ignorespacesafterend}

\newenvironment{oldplainhscode}%
  {\hsnewpar\abovedisplayskip
   \advance\leftskip\mathindent
   \hscodestyle
   \let\\=\@normalcr
   \(\pboxed}%
  {\endpboxed\)%
   \hsnewpar\belowdisplayskip
   \ignorespacesafterend}

% Here, we make plainhscode the default environment.

\newcommand{\plainhs}{\sethscode{plainhscode}}
\newcommand{\oldplainhs}{\sethscode{oldplainhscode}}
\plainhs

% The arrayhscode is like plain, but makes use of polytable's
% parray environment which disallows page breaks in code blocks.

\newenvironment{arrayhscode}%
  {\hsnewpar\abovedisplayskip
   \advance\leftskip\mathindent
   \hscodestyle
   \let\\=\@normalcr
   \(\parray}%
  {\endparray\)%
   \hsnewpar\belowdisplayskip
   \ignorespacesafterend}

\newcommand{\arrayhs}{\sethscode{arrayhscode}}

% The mathhscode environment also makes use of polytable's parray 
% environment. It is supposed to be used only inside math mode 
% (I used it to typeset the type rules in my thesis).

\newenvironment{mathhscode}%
  {\parray}{\endparray}

\newcommand{\mathhs}{\sethscode{mathhscode}}

% texths is similar to mathhs, but works in text mode.

\newenvironment{texthscode}%
  {\(\parray}{\endparray\)}

\newcommand{\texths}{\sethscode{texthscode}}

% The framed environment places code in a framed box.

\def\codeframewidth{\arrayrulewidth}
\RequirePackage{calc}

\newenvironment{framedhscode}%
  {\parskip=\abovedisplayskip\par\noindent
   \hscodestyle
   \arrayrulewidth=\codeframewidth
   \tabular{@{}|p{\linewidth-2\arraycolsep-2\arrayrulewidth-2pt}|@{}}%
   \hline\framedhslinecorrect\\{-1.5ex}%
   \let\endoflinesave=\\
   \let\\=\@normalcr
   \(\pboxed}%
  {\endpboxed\)%
   \framedhslinecorrect\endoflinesave{.5ex}\hline
   \endtabular
   \parskip=\belowdisplayskip\par\noindent
   \ignorespacesafterend}

\newcommand{\framedhslinecorrect}[2]%
  {#1[#2]}

\newcommand{\framedhs}{\sethscode{framedhscode}}

% The inlinehscode environment is an experimental environment
% that can be used to typeset displayed code inline.

\newenvironment{inlinehscode}%
  {\(\def\column##1##2{}%
   \let\>\undefined\let\<\undefined\let\\\undefined
   \newcommand\>[1][]{}\newcommand\<[1][]{}\newcommand\\[1][]{}%
   \def\fromto##1##2##3{##3}%
   \def\nextline{}}{\) }%

\newcommand{\inlinehs}{\sethscode{inlinehscode}}

% The joincode environment is a separate environment that
% can be used to surround and thereby connect multiple code
% blocks.

\newenvironment{joincode}%
  {\let\orighscode=\hscode
   \let\origendhscode=\endhscode
   \def\endhscode{\def\hscode{\endgroup\def\@currenvir{hscode}\\}\begingroup}
   %\let\SaveRestoreHook=\empty
   %\let\ColumnHook=\empty
   %\let\resethooks=\empty
   \orighscode\def\hscode{\endgroup\def\@currenvir{hscode}}}%
  {\origendhscode
   \global\let\hscode=\orighscode
   \global\let\endhscode=\origendhscode}%

\makeatother
\EndFmtInput
%
\ignore{
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\mbox{\enskip\{-\# LANGUAGE ImpredicativeTypes  \#-\}\enskip}{}\<[E]%
\\
\>[3]{}\mbox{\enskip\{-\# LANGUAGE ScopedTypeVariables  \#-\}\enskip}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\mathbf{import}\;\Conid{Prelude}\;\Varid{hiding}\;((\bind ),\Varid{return},\Varid{pure},(\mathbin{<*>}),\Varid{fmap},\Varid{sequence},\Varid{pred}){}\<[E]%
\\
\>[3]{}\mathbf{import}\;\Conid{\Conid{Data}.Function}\;(\Varid{on}){}\<[E]%
\\
\>[3]{}\mathbf{import}\;\Conid{\Conid{Data}.List}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
}

---
title: Towards a more efficient Selection Monad
---

---
abstract:
  This paper explores a novel approach to selection functions through the 
  introduction of a generalized selection monad. The foundation is laid with the 
  conventional selection monad $J$, defined as $(A \rightarrow R) \rightarrow A$, which 
  employs a pair function to compute new selection functions. However, inefficiencies in 
  the original pair function are identified. To address these issues, a specialized type 
  $K$ is introduced, and its isomorphism to $J$ is demonstrated. The paper further 
  generalizes the $K$ type to $GK$, where performance improvements and enhanced 
  intuitive usability are observed. The embedding between $J$ to $GK$ is established, 
  offering a more efficient and expressive alternative to the well established $J$ type 
  for selection functions. The findings emphasize the advantages of the Generalized 
  Selection Monad and its applicability in diverse scenarios, paving the way for further 
  exploration and optimization.
---

Introduction to the Selection Monad J
=====================================

The selection monad, initially introduced by Paulo Oliva and Martin Escardo 
\cite{escardo2010selection}, serves as a valuable tool for modeling selection-based 
algorithms in functional programming. Widely explored in the context of sequential games 
\cite{escardo2010sequential}, it has been applied to compute solutions for games with 
perfect information and has found applications in logic and proof theory through the 
Double-Negation Theorem and the Tychonoff Theorem \cite{escardo2010sequential}. 
Additionally, it has been effectively employed in modeling greedy algorithms 
\cite{hartmann2021greedyselection}. These diverse applications of the selection monad 
heavily rely on its monadic behavior, particularly emphasizing the use of the $sequence$ 
function for monads.

However, within the context of the selection monad, it becomes evident that the $sequence$ 
function is unnecessarily inefficient, duplicating work already calculated in previous 
steps. This paper introduces two alternative types, namely $K$ and $GK$, for the selection 
monad. It demonstrates that the new $K$ type is isomorphic to the existing $J$ type, 
resolving the inefficiency issue of the monadic $sequence$ function. Subsequently, the $K$ 
type is further generalized into the $GK$ type. The proposition put forth in this paper 
advocates for the adoption of the $GK$ type over the traditional $J$ type due to its 
efficiency advantages. Moreover, the $GK$ type is argued to be more intuitive for 
programming and, given its broader type, offers increased versatility for a wide array of 
applications involving the selection monad.

The upcoming section delves into the selection monad, with a particular focus on the type:  
$(A \rightarrow R) \rightarrow A$ representing selection functions 
\cite{escardo2010selection}. The exploration of the $pair$ function highlights its ability 
to compute a new selection function based on criteria from two existing functions. 
Supported by a practical example involving decision-making scenarios and individuals 
navigating paths, this section underscores the functionality of selection functions. An 
analysis of the inefficiencies in the original $pair$ function identifies redundant 
computational work. The paper's primary contribution is outlined: an illustration and 
proposal for an efficient solution to enhance the performance of the $pair$ function. This 
introductory overview sets the stage for a detailed exploration of the selection monad and 
subsequent discussions on optimizations.

Selection functions
===================

Consider the type for selection functions introduced by Paulo Olvia and Martin Escardo 
\cite{escardo2010selection} :
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\mathbf{type}\;\Conid{J}\;\Varid{r}\;\Varid{a}\mathrel{=}(\Varid{a}\to \Varid{r})\to \Varid{a}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
Consider the following example. Two individuals are walking towards each other on the 
pavement. A collision is imminent. At this juncture, each individual must decide their 
next move. This decision-making process can be modeled using selection functions. The 
decision they need to make is going towards the street the or the wall:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\mathbf{data}\;\Conid{Decision}\mathrel{=}\Conid{Street}\mid \Conid{Wall}\;\mathbf{deriving}\;\Conid{Show}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
The respective selection functions decide given a property function that tells them what 
decision is the correct one, select the correct one, and if there is no correct one, they 
default to walking towards the wall.
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{s}\mathbin{::}\Conid{J}\;\Conid{Bool}\;\Conid{Decision}{}\<[E]%
\\
\>[3]{}\Varid{s}\;\Varid{p}\mathrel{=}\mathbf{if}\;\Varid{p}\;\Conid{Street}\;\mathbf{then}\;\Conid{Street}\;\mathbf{else}\;\Conid{Wall}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
When given two selection functions, a $pair$ function can be defined to compute a new 
selection function. This resultant function selects a pair based on the criteria 
established by the two given selection functions:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{5}{@{}>{\hspre}l<{\hspost}@{}}%
\column{9}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{pair}\mathbin{::}\Conid{J}\;\Varid{r}\;\Varid{a}\to \Conid{J}\;\Varid{r}\;\Varid{b}\to \Conid{J}\;\Varid{r}\;(\Varid{a},\Varid{b}){}\<[E]%
\\
\>[3]{}\Varid{pair}\;\Varid{f}\;\Varid{g}\;\Varid{p}\mathrel{=}(\Varid{a},\Varid{b}){}\<[E]%
\\
\>[3]{}\hsindent{2}{}\<[5]%
\>[5]{}\mathbf{where}{}\<[E]%
\\
\>[5]{}\hsindent{4}{}\<[9]%
\>[9]{}\Varid{a}\mathrel{=}\Varid{f}\;(\lambda \Varid{x}\to \Varid{p}\;(\Varid{x},\Varid{g}\;(\lambda \Varid{y}\to \Varid{p}\;(\Varid{x},\Varid{y})))){}\<[E]%
\\
\>[5]{}\hsindent{4}{}\<[9]%
\>[9]{}\Varid{b}\mathrel{=}\Varid{g}\;(\lambda \Varid{y}\to \Varid{p}\;(\Varid{a},\Varid{y})){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
To apply the $pair$ function, a property function $pair$ is needed that will judge two 
decisions and return `True` if a crash is avoided and `False` otherwise.
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{22}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{pred}\mathbin{::}(\Conid{Decision},\Conid{Decision})\to \Conid{Bool}{}\<[E]%
\\
\>[3]{}\Varid{pred}\;(\Conid{Wall},\Conid{Street})\mathrel{=}\Conid{True}{}\<[E]%
\\
\>[3]{}\Varid{pred}\;(\Conid{Street},\Conid{Wall})\mathrel{=}\Conid{True}{}\<[E]%
\\
\>[3]{}\Varid{pred}\;\anonymous {}\<[22]%
\>[22]{}\mathrel{=}\Conid{False}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
The $pair$ function, merges the two selection functions into a new one that calculates an  
overall optimal decision.

```
ghci> pair s s pred
(Street,Wall)
```

Examining how the $pair$ function is defined reveals that the first element $a$ of the 
pair is determined by applying the initial selection function $f$ to a newly constructed 
property function. Intuitively, selection functions can be conceptualized as entities 
containing a collection of objects, waiting for a property function to assess their 
underlying elements. Once equipped with a property function, they can apply it to their 
elements and select an optimal one.
Considering the types assigned to selection functions, it is evident that an initial 
selection function $f$ remains in anticipation of a property function of type 
$(A \rightarrow R)$ to determine an optimal $A$. The $pair$ function is endowed with a 
property function $p$ of type $((A,B) \rightarrow R)$. Through the utilization of this 
property function, a property function for $f$ can be derived by using the second 
selection function $g$ to select a corresponding $B$ and subsequently applying $p$ to 
assess $(A,B)$ pairs as follows:
$(\lambda x \rightarrow p (x, g (\lambda y \rightarrow p (x,y))))$. Upon the determination 
of an optimal $A$, a corresponding $B$ can then be computed as 
$g (\lambda y \rightarrow p (a,y))$. In this case, the $pair$ function can be 
conceptualized as a function that constructs all possible combinations of the elements 
within the provided selection function and subsequently identifies the overall optimal 
one.It might feel intuitive to consider the following modified $pair$ function that seems 
to be more symmetric.
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{5}{@{}>{\hspre}l<{\hspost}@{}}%
\column{9}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{pair'}\mathbin{::}\Conid{J}\;\Varid{r}\;\Varid{a}\to \Conid{J}\;\Varid{r}\;\Varid{b}\to \Conid{J}\;\Varid{r}\;(\Varid{a},\Varid{b}){}\<[E]%
\\
\>[3]{}\Varid{pair'}\;\Varid{f}\;\Varid{g}\;\Varid{p}\mathrel{=}(\Varid{a},\Varid{b}){}\<[E]%
\\
\>[3]{}\hsindent{2}{}\<[5]%
\>[5]{}\mathbf{where}{}\<[E]%
\\
\>[5]{}\hsindent{4}{}\<[9]%
\>[9]{}\Varid{a}\mathrel{=}\Varid{f}\;(\lambda \Varid{x}\to \Varid{p}\;(\Varid{x},\Varid{g}\;(\lambda \Varid{y}\to \Varid{p}\;(\Varid{x},\Varid{y})))){}\<[E]%
\\
\>[5]{}\hsindent{4}{}\<[9]%
\>[9]{}\Varid{b}\mathrel{=}\Varid{g}\;(\lambda \Varid{y}\to \Varid{p}\;(\Varid{f}\;(\lambda \Varid{x}\to \Varid{p}\;(\Varid{x},\Varid{y})),\Varid{y})){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
However, applying this modified $pair'$ to our previous example this results in a overall 
non optimal solution.

```
ghci> pair' p1 p2 pred
(Left,Left)
```

This illustrates how the original $pair$ function keeps track of its first decision when 
determining its second element. It is noteworthy that, in the example example, achieving a 
satisfying outcome for both pedestrians is only possible when they consider the direction 
the other one is heading. The specific destination does not matter, as long as they are 
moving in different directions. Consequently, the original $pair$ function can be 
conceived as a function that selects the optimal solution while retaining awareness of 
previous solutions, whereas our modified $pair'$ does not.
An issue with the original $pair$ function might have been identified by the attentive 
reader. There is redundant computational work involved. Initially, all possible pairs 
are constructed to determine an optimal first element $A$, but the corresponding $A$ 
that renders it an overall optimal solution is overlooked, resulting in only $A$ being 
returned. Subsequently, the optimal $B$ is recalculated based on the already determined 
optimal $A$ when selecting the second element of the pair.
The primary contribution of this paper will be to illustrate and propose a solution to 
this inefficiency.

Sequence
--------

The generalization of the pair function to accommodate a sequence of selection functions 
is the initial focus of exploration. In the context of selection functions, a $sequence$ 
operation is introduced, capable of combining a list of selection functions into a 
singular selection function that, in turn, selects a list of objects:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{5}{@{}>{\hspre}l<{\hspost}@{}}%
\column{9}{@{}>{\hspre}l<{\hspost}@{}}%
\column{12}{@{}>{\hspre}l<{\hspost}@{}}%
\column{21}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{sequence}\mathbin{::}[\mskip1.5mu \Conid{J}\;\Varid{r}\;\Varid{a}\mskip1.5mu]\to \Conid{J}\;\Varid{r}\;[\mskip1.5mu \Varid{a}\mskip1.5mu]{}\<[E]%
\\
\>[3]{}\Varid{sequence}\;[\mskip1.5mu \mskip1.5mu]\;\Varid{p}{}\<[21]%
\>[21]{}\mathrel{=}[\mskip1.5mu \mskip1.5mu]{}\<[E]%
\\
\>[3]{}\Varid{sequence}\;(\Varid{e}\mathbin{:}\Varid{es})\;\Varid{p}\mathrel{=}\Varid{a}\mathbin{:}\Varid{as}{}\<[E]%
\\
\>[3]{}\hsindent{2}{}\<[5]%
\>[5]{}\mathbf{where}{}\<[E]%
\\
\>[5]{}\hsindent{4}{}\<[9]%
\>[9]{}\Varid{a}{}\<[12]%
\>[12]{}\mathrel{=}\Varid{e}\;(\lambda \Varid{x}\to \Varid{p}\;(\Varid{x}\mathbin{:}\Varid{sequence}\;\Varid{es}\;(\Varid{p}\mathbin{\circ}(\Varid{x}\mathbin{:})))){}\<[E]%
\\
\>[5]{}\hsindent{4}{}\<[9]%
\>[9]{}\Varid{as}\mathrel{=}\Varid{sequence}\;\Varid{es}\;(\Varid{p}\mathbin{\circ}(\Varid{a}\mathbin{:})){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
Here, similar to the pair function, the sequence function extracts elements from the 
resulting list through the corresponding selection functions. This extraction is achieved 
by applying each function to a newly constructed property function that possesses the 
capability to foresee the future, thereby constructing an optimal future based on the 
currently examined element.
However, a notable inefficiency persists, exacerbating the issue observed in the pair 
function. During the determination of the first element, the $sequence$ function 
calculates an optimal remainder of the list, only to overlook it and redundantly perform 
the same calculation for subsequent elements. This inefficiency in $sequence$ warrants 
further investigation for potential optimization in subsequent sections of this research 
paper.

Selection monad J
-----------------

The formation of a monad within the selection functions unfolds as follows 
\cite{escardo2010selection}:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}(\bind )\mathbin{::}\Conid{J}\;\Varid{r}\;\Varid{a}\to (\Varid{a}\to \Conid{J}\;\Varid{r}\;\Varid{b})\to \Conid{J}\;\Varid{r}\;\Varid{b}{}\<[E]%
\\
\>[3]{}(\bind )\;\Varid{f}\;\Varid{g}\;\Varid{p}\mathrel{=}\Varid{g}\;(\Varid{f}\;(\Varid{p}\mathbin{\circ}\Varid{flip}\;\Varid{g}\;\Varid{p}))\;\Varid{p}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{return}\mathbin{::}\Varid{a}\to \Conid{J}\;\Varid{r}\;\Varid{a}{}\<[E]%
\\
\>[3]{}\Varid{return}\;\Varid{x}\;\Varid{p}\mathrel{=}\Varid{x}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
These definitions illustrate the monadic structure inherent in selection functions. The 
Haskell standard library already incorporates a built-in function for monads, referred to 
as $sequence'$, defined as:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{23}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{sequence'}\mathbin{::}[\mskip1.5mu \Conid{J}\;\Varid{r}\;\Varid{a}\mskip1.5mu]\to \Conid{J}\;\Varid{r}\;[\mskip1.5mu \Varid{a}\mskip1.5mu]{}\<[E]%
\\
\>[3]{}\Varid{sequence'}\;(\Varid{ma}\mathbin{:}\Varid{mas})\mathrel{=}\Varid{ma}\bind {}\<[E]%
\\
\>[3]{}\hsindent{20}{}\<[23]%
\>[23]{}\lambda \Varid{x}\to \Varid{sequence'}\;\Varid{mas}\bind {}\<[E]%
\\
\>[3]{}\hsindent{20}{}\<[23]%
\>[23]{}\lambda \Varid{xs}\to \Varid{return}\;(\Varid{x}\mathbin{:}\Varid{xs}){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
Notably, in the case of the selection monad, this built-in $sequence'$ function aligns 
with the earlier provided $sequence$ implementation. This inherent consistency further 
solidifies the monadic nature of selection functions, underscoring their alignment with 
established Haskell conventions. 


Illustration of Sequence in the Context of Selection Functions
--------------------------------------------------------------

To ilustrate the application of the sequence funct ion within the domain of selection 
functions, consider a practical scenario \cite{hartmann2022algorithm}: the task of 
cracking a secret password. In this hypothetical situation, a black box property function 
$p$ is provided that returns `True` if the correct password is entered and `False` 
otherwise. Additionally, knowledge is assumed that the password is six characters long:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{14}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{p}\mathbin{::}\Conid{String}\to \Conid{Bool}{}\<[E]%
\\
\>[3]{}\Varid{p}\;\text{\ttfamily \char34 secret\char34}\mathrel{=}\Conid{True}{}\<[E]%
\\
\>[3]{}\Varid{p}\;\anonymous {}\<[14]%
\>[14]{}\mathrel{=}\Conid{False}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
Suppose access is available to a $maxWith$ function, defined as:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{33}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{maxWith}\mathbin{::}\Conid{Ord}\;\Varid{r}\Rightarrow [\mskip1.5mu \Varid{a}\mskip1.5mu]\to \Conid{J}\;\Varid{r}\;\Varid{a}{}\<[E]%
\\
\>[3]{}\Varid{maxWith}\;\Varid{xs}\;\Varid{f}\mathrel{=}\Varid{snd}\;(\Varid{maximumBy}\;(\Varid{compare}\mathbin{`\Varid{on}`}\Varid{fst})\;{}\<[E]%
\\
\>[3]{}\hsindent{30}{}\<[33]%
\>[33]{}(\Varid{map}\;(\lambda \Varid{x}\to (\Varid{f}\;\Varid{x},\Varid{x}))\;\Varid{xs})){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
With these resources, a selection function denoted as $selectChar$ can be constructed, 
which, given a property function that evaluates each character, selects a single character 
satisfying the specified property function:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{selectChar}\mathbin{::}\Conid{J}\;\Conid{Bool}\;\Conid{Char}{}\<[E]%
\\
\>[3]{}\Varid{selectChar}\mathrel{=}\Varid{maxWith}\;[\mskip1.5mu \text{\ttfamily 'a'}\mathinner{\ldotp\ldotp}\text{\ttfamily 'z'}\mskip1.5mu]{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
It's worth noting that the use of maxWith is facilitated by the ordered nature of booleans
in Haskell, where `True` is considered greater than `False`. Leveraging this selection 
function, the sequence function can be employed on a list comprising six identical copies 
of $selectChar$ to successfully crack the secret password. Each instance of the selection 
function focuses on a specific character of the secret password:

```
ghci> sequence (replicate 6 selectChar) p
"secret"
```

This illustrative example not only showcases the practical application of the $sequence$ 
function within the domain of selection functions but also emphasizes its utility in 
addressing real-world problems, such as scenarios involving password cracking. Notably, 
there is no need to explicitly specify a property function for judging individual 
character; rather, this property function is constructed within the monads bind 
definition, and its utilization is facilitated through the application of the $sequence$ 
function. Additionally, attention should be drawn to the fact that this example involves 
redundant calculations. After determining the first character of the secret password, the 
system overlooks the prior computation of the entire password and initiates the calculation 
anew for subsequent characters.
To address this specific inefficiency within the selection monad, concerning the pair and 
sequence functions, two new variations of the selection monad will be introduced. 
Initially, an examination of a new type, denoted as $K$, will reveal its isomorphism to 
the selection monad $J$. Subsequently, an exploration of the generalization of this $K$ 
type will enhance its intuitive usability. Remarkably, it will be demonstrated that the 
$J$ monad can be embedded into this generalized $K$ type.

Special K
=========

The following type $K$ is to be considered:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\mathbf{type}\;\Conid{K}\;\Varid{r}\;\Varid{a}\mathrel{=}\Varid{forall}\;\Varid{b}\mathbin{\circ}(\Varid{a}\to (\Varid{r},\Varid{b}))\to \Varid{b}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
While selection functions of type $J$ are still in anticipation of a property function 
capable of judging their underlying elements, a similar operation is performed by the new 
$K$ type. The property function of the $K$ type also assesses its elements by transforming 
them into $R$ values. Additionally, it converts the $a$ into any $B$ and returns that $B$ 
along with its judgment $R$.
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{17}{@{}>{\hspre}l<{\hspost}@{}}%
\column{26}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{pairK}\mathbin{::}\Conid{K}\;\Varid{r}\;\Varid{a}\to \Conid{K}\;\Varid{r}\;\Varid{b}\to \Conid{K}\;\Varid{r}\;(\Varid{a},\Varid{b}){}\<[E]%
\\
\>[3]{}\Varid{pairK}\;\Varid{f}\;\Varid{g}\;\Varid{p}\mathrel{=}\Varid{f}\;(\lambda \Varid{x}\to {}\<[E]%
\\
\>[3]{}\hsindent{14}{}\<[17]%
\>[17]{}\Varid{g}\;(\lambda \Varid{y}\to \mathbf{let}\;(\Varid{r},\Varid{z})\mathrel{=}\Varid{p}\;(\Varid{x},\Varid{y}){}\<[E]%
\\
\>[17]{}\hsindent{9}{}\<[26]%
\>[26]{}\mathbf{in}\;(\Varid{r},(\Varid{r},\Varid{z})))){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
The previously mentioned inefficiency is now addressed by the definition of $pairK$. This 
is achieved by examining every element $x$ in the selection function $f$. For each 
element, a corresponding result is extracted from the second selection function $g$.
Utilizing the additional flexibility provided by the new $K$ type, the property function 
for $g$ is now constructed differently. Instead of merely returning the result $z$ along 
with the corresponding $R$ value, a duplicate of the entire result pair calculated by $p$ 
is generated and returned. As this duplicate already represents the complete solution, the 
entire result for an optimal $x$ can now be straightforwardly yielded by $f$, eliminating 
the need for additional computations.

The $sequenceK$ for this novel $K$ type can be defined as follows:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{22}{@{}>{\hspre}l<{\hspost}@{}}%
\column{26}{@{}>{\hspre}l<{\hspost}@{}}%
\column{34}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{sequenceK}\mathbin{::}[\mskip1.5mu \Conid{K}\;\Varid{r}\;\Varid{a}\mskip1.5mu]\to \Conid{K}\;\Varid{r}\;[\mskip1.5mu \Varid{a}\mskip1.5mu]{}\<[E]%
\\
\>[3]{}\Varid{sequenceK}\;[\mskip1.5mu \Varid{e}\mskip1.5mu]\;\Varid{p}{}\<[22]%
\>[22]{}\mathrel{=}\Varid{e}\;(\lambda \Varid{x}\to \Varid{p}\;[\mskip1.5mu \Varid{x}\mskip1.5mu]){}\<[E]%
\\
\>[3]{}\Varid{sequenceK}\;(\Varid{e}\mathbin{:}\Varid{es})\;\Varid{p}\mathrel{=}\Varid{e}\;(\lambda \Varid{x}\to \Varid{sequenceK}\;\Varid{es}{}\<[E]%
\\
\>[3]{}\hsindent{23}{}\<[26]%
\>[26]{}(\lambda \Varid{xs}\to \mathbf{let}\;(\Varid{r},\Varid{y})\mathrel{=}\Varid{p}\;(\Varid{x}\mathbin{:}\Varid{xs}){}\<[E]%
\\
\>[26]{}\hsindent{8}{}\<[34]%
\>[34]{}\mathbf{in}\;(\Varid{r},(\Varid{r},\Varid{y})))){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
This $sequenceK$ implementation employs the same strategy as the earlier $pairK$ function. 
It essentially generates duplicates of the entire solution pair, returning these in place 
of the result value. The selection function one layer above then unpacks the result pair, 
allowing the entire solution to be propagated.
The efficiency issues previously outlined are addressed by these novel $pairK$ and 
$sequenceK$ functions. It will be further demonstrated that this fresh $K$ type is 
isomorphic to the preceding $J$ type. This essentially empowers the transformation of 
every problem previously solved with the $J$ type into the world of the $K$ type. 
Subsequently, the solutions can be computed more efficiently before being transformed back 
to express them in terms of $J$.

Special K is isomorphic to J
----------------------------

To demonstrate the isomorphism between the new Special $K$ type and the $J$ type, two 
operators are introduced for transforming from one type to the other:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{j2k}\mathbin{::}\Conid{J}\;\Varid{r}\;\Varid{a}\to \Conid{K}\;\Varid{r}\;\Varid{a}{}\<[E]%
\\
\>[3]{}\Varid{j2k}\;\Varid{f}\;\Varid{p}\mathrel{=}\Varid{snd}\;(\Varid{p}\;(\Varid{f}\;(\Varid{fst}\mathbin{\circ}\Varid{p}))){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
When provided with a selection function $f$ of type $J r a$, the $j2k$ operator constructs 
an entity of type $K$. For a given $f$ of type $(A \rightarrow R) \rightarrow A$ and 
$p$ of type $\forall B. (A \rightarrow (R,B))$, the objective is to return an entity of 
type $B$. This is achieved by initially extracting an a from $f$ using the constructed 
property function $(fst . p)$. Subsequently, this a is employed to apply $p$, yielding an 
$(R,B)$ pair, from which the $B$ is obtained by applying $snd$ to the pair.
The transformation of a selection function of type $K$ into a selection function of type 
$J$ is accomplished as follows:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{k2j}\mathbin{::}\Conid{K}\;\Varid{r}\;\Varid{a}\to \Conid{J}\;\Varid{r}\;\Varid{a}{}\<[E]%
\\
\>[3]{}\Varid{k2j}\;\Varid{f}\;\Varid{p}\mathrel{=}\Varid{f}\;(\lambda \Varid{x}\to (\Varid{p}\;\Varid{x},\Varid{x})){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
Given a selection function $f$ of type $\forall B. (A \rightarrow (R,B)) \rightarrow B$ 
and a $p$ of type $(A -> R) -> A$, an $A$ can be directly extracted from $f$ by 
constructing a property function that utilizes $p$ to obtain an $R$ value while leaving 
the corresponding $x$ of type $A$ untouched.
To validate that these two operators indeed establish an isomorphism between $J$ and $K$, 
the following equations must be proven: $(k2j . j2k) f = f$ and $(j2k . k2j) g = g$.

\begin{proof}[J to K Embedding]
The equality (k2j . j2k) f = f can be straightforwardly demonstrated by applying all the 
lambdas and the definitions of fst and snd:

\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[B]{}(\Varid{k2j}\mathbin{\circ}\Varid{j2k})\;\Varid{f}{}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Apply}\;\Varid{definitions}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}(\lambda \Varid{f}\;\Varid{p}\to \Varid{f}\;(\lambda \Varid{x}\to (\Varid{p}\;\Varid{x},\Varid{x})))\;(\lambda \Varid{p}\to \Varid{snd}\;(\Varid{p}\;(\Varid{f}\;(\Varid{fst}\mathbin{\circ}\Varid{p})))){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Simplify}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{f}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks

\end{proof}

This proof involves a direct application of lambda expressions and the definitions of 
$fst$ and $snd$ for simplification. To facilitate the proof of the second isomorphism, we 
initially introduce the free theorem for the special K type \cite{wadler1989theorems}:

\begin{theorem}[Free Theorem for K]
Given the following functions with their corresponding types:

\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[B]{}\Varid{g}\mathbin{::}\Varid{forall}\;\Varid{b}\mathbin{\circ}(\Varid{a}\to (\Varid{r},\Varid{b}))\to \Varid{b}{}\<[E]%
\\
\>[B]{}\Varid{h}\mathbin{::}\Varid{b1}\to \Varid{b2}{}\<[E]%
\\
\>[B]{}\Varid{p}\mathbin{::}\Varid{a}\to (\Varid{r},\Varid{b1}){}\<[E]%
\ColumnHook
\end{hscode}\resethooks

We have:

\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[B]{}\Varid{h}\;(\Varid{g}\;\Varid{p})\mathrel{=}\Varid{g}\;(\lambda \Varid{x}\to \mathbf{let}\;(\Varid{r},\Varid{y})\mathrel{=}(\Varid{p}\;\Varid{x})\;\mathbf{in}\;(\Varid{r},\Varid{h}\;\Varid{y})){}\<[E]%
\ColumnHook
\end{hscode}\resethooks

\end{theorem}
The free theorem essentially asserts that a function $h$ of type $Y_1 \rightarrow Y_2$, 
when applied to the result of a selection function, can also be incorporated into the 
property function and applied to each individual element. This follows from the 
generalized type of $K$, where the only means of generating $Y_1$ values is through the 
application of $p$. Consequently, it becomes inconsequential whether h is applied to the 
final result or to each individual intermediate result.
With the free theorem for $K$, the remaining portion of the isomorphism can now be 
demonstrated as follows:

\begin{proof}[K to J Embedding]
The equality (j2k . k2j) g = g is established through the following steps:

\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[B]{}(\Varid{j2k}\mathbin{\circ}\Varid{k2j})\;\Varid{g}{}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Apply}\;\Varid{definitions}\;\Varid{and}\;\Varid{simplify}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\lambda \Varid{p}\to \Varid{snd}\;(\Varid{p}\;(\Varid{g}\;(\lambda \Varid{x}\to ((\Varid{fst}\mathbin{\circ}\Varid{p})\;\Varid{x},\Varid{x})))){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Free}\;\Conid{Theorem}\;\Varid{for}\;\Conid{K}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\lambda \Varid{p}\to \Varid{g}\;(\lambda \Varid{x}\to ((\Varid{fst}\mathbin{\circ}\Varid{p})\;\Varid{x},(\Varid{snd}\mathbin{\circ}\Varid{p})\;\Varid{x})){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Simplify}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{g}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks

\end{proof}

The monad definitions and $sequence$ definition for the new $K$ type can be derived from 
the isomorphism. While the desired performance improvements are achieved by the definition
of $K$, significant data structure copying is required, only to be deconstructed and 
discarded at a higher layer. This process significantly complicates the associated 
definitions for $sequence$ and $pair$, rendering them challenging to handle and lacking in 
intuitiveness.
Introducing another type, $GK$, that returns the entire tuple rather than just the result 
value seems more intuitive. This exploration is detailed in the following section, where 
similar performance improvements are observed with $GK$ while the definitions become more 
straightforward. This approach also eliminates the need for unnecessary copying of data. 
However, it is revealed that $GK$ is not isomorphic to $J$ and $K$; instead, they can be 
embedded into $GK$. Conversely, we will explore a specific precondition under which $GK$ 
can be embedded into $J$ or $K$.

Generalised K
=============

Consider the more general type $GK$, derived from the previous special $K$ type:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\mathbf{type}\;\Conid{GK}\;\Varid{r}\;\Varid{a}\mathrel{=}\Varid{forall}\;\Varid{b}\mathbin{\circ}(\Varid{a}\to (\Varid{r},\Varid{b}))\to (\Varid{r},\Varid{b}){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
Unlike its predecessor, $GK$ returns the entire pair produced by the property function, 
rather than just the result value. The implementation of $pairGK$ for the new $GK$ type no 
longer necessitates the creation of a copy of the data structure. It suffices to return 
the result of the property function's application to the complete pair:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{pairGK}\mathbin{::}\Conid{GK}\;\Varid{r}\;\Varid{a}\to \Conid{GK}\;\Varid{r}\;\Varid{b}\to \Conid{GK}\;\Varid{r}\;(\Varid{a},\Varid{b}){}\<[E]%
\\
\>[3]{}\Varid{pairGK}\;\Varid{f}\;\Varid{g}\;\Varid{p}\mathrel{=}\Varid{f}\;(\lambda \Varid{x}\to \Varid{g}\;(\lambda \Varid{y}\to \Varid{p}\;(\Varid{x},\Varid{y}))){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
In terms of readability, this definition of pairGK is significantly more concise, 
conveying the essence of the $pair$ function without unnecessary boilerplate code. For 
every element $x$  of type $a$ within $f$, all $y$ of type $b$ within $g$ are inspected 
and judged by the given property function $p$. The resulting pair selection function 
returns the optimal pair of $(A,B)$ values according to the provided property function.
Furthermore, we define $sequenceGK$ as follows:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{23}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{sequenceGK}\mathbin{::}[\mskip1.5mu \Conid{GK}\;\Varid{r}\;\Varid{a}\mskip1.5mu]\to \Conid{GK}\;\Varid{r}\;[\mskip1.5mu \Varid{a}\mskip1.5mu]{}\<[E]%
\\
\>[3]{}\Varid{sequenceGK}\;[\mskip1.5mu \Varid{e}\mskip1.5mu]\;\Varid{p}{}\<[23]%
\>[23]{}\mathrel{=}\Varid{e}\;(\lambda \Varid{x}\to \Varid{p}\;[\mskip1.5mu \Varid{x}\mskip1.5mu]){}\<[E]%
\\
\>[3]{}\Varid{sequenceGK}\;(\Varid{e}\mathbin{:}\Varid{es})\;\Varid{p}\mathrel{=}\Varid{e}\;(\lambda \Varid{x}\to \Varid{sequenceGK}\;\Varid{es}\;(\lambda \Varid{xs}\to \Varid{p}\;(\Varid{x}\mathbin{:}\Varid{xs}))){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
Following a similar pattern, this $sequenceGK$ function builds all possible futures for 
each element within $e$. Once an optimal list of elements is found, this list is simply 
returned along with the corresponding $R$ value.

Relationship to J and Special K
-------------------------------
With the following operators, selection functions of type $K$ can be embedded into $GK$.

\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{gk2k}\mathbin{::}\Varid{forall}\;\Varid{r}\;\Varid{a}\;\Varid{b}\mathbin{\circ}((\Varid{a}\to (\Varid{r},\Varid{b}))\to (\Varid{r},\Varid{b}))\to ((\Varid{a}\to (\Varid{r},\Varid{b}))\to \Varid{b}){}\<[E]%
\\
\>[3]{}\Varid{gk2k}\;\Varid{f}\mathrel{=}\Varid{snd}\mathbin{\circ}\Varid{f}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{k2gk}\mathbin{::}\Conid{K}\;\Varid{r}\;\Varid{a}\to \Conid{GK}\;\Varid{r}\;\Varid{a}{}\<[E]%
\\
\>[3]{}\Varid{k2gk}\;\Varid{f}\;\Varid{p}\mathrel{=}\Varid{f}\;(\lambda \Varid{x}\to \mathbf{let}\;(\Varid{r},\Varid{y})\mathrel{=}\Varid{p}\;\Varid{x}\;\mathbf{in}\;(\Varid{r},(\Varid{r},\Varid{y}))){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
Similar to the free theroem for the $K$ type, it is also possible to derive the free 
theorem for the $GK$ type:

\begin{theorem}[Free Theorem for GK]
Given the following functions with thier corresponding types:

\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[B]{}\Varid{g}\mathbin{::}\Varid{forall}\;\Varid{b}\mathbin{\circ}(\lambda \Varid{a}\to (\Varid{r},\Varid{b}))\to (\Varid{r},\Varid{b}){}\<[E]%
\\
\>[B]{}\Varid{f}\mathbin{::}\Varid{b1}\to \Varid{b2}{}\<[E]%
\\
\>[B]{}\Varid{p}\mathbin{::}\Varid{a}\to (\Varid{r},\Varid{b1}){}\<[E]%
\ColumnHook
\end{hscode}\resethooks

We have:

\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[B]{}((\Varid{id}\mathbin{***}\Varid{f})\mathbin{\circ}\Varid{g})\;\Varid{p}\mathrel{=}\Varid{g}\;((\Varid{id}\mathbin{***}\Varid{f})\mathbin{\circ}\Varid{p}){}\<[E]%
\ColumnHook
\end{hscode}\resethooks

\end{theorem}

It is basically stating the same as the free Theorem for $K$, where given a function $f$
that is applied to the result of a selection function, it dosent matter if this is done
in the end to the final result, or inside the property function of the selection function. 
But it now needs to account for the fact that the $GK$ type is also returning the $R$ 
value. 

With the free theorem for $GK$ we can now proof that selection functions of type  $K$ can
be embedded into $GK$:

\begin{proof}[K to GK Embedding]
The equality (k2gk . gk2k) f = f is established through the following steps:

Assuming: f :: K r a

\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[B]{}(\Varid{gk2k}\mathbin{\circ}\Varid{k2gk})\;\Varid{f}{}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Definitions}\;\Varid{and}\;\Varid{rewrite}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}(\lambda \Varid{p}\to (\Varid{snd}\mathbin{\circ}\Varid{f})\;(\lambda \Varid{x}\to \mathbf{let}\;(\Varid{r},\Varid{y})\mathrel{=}\Varid{p}\;\Varid{x}\;\mathbf{in}\;(\Varid{r},(\Varid{r},\Varid{y})))){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Free}\;\Varid{theorem}\;\mathbf{of}\;\Conid{GK}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}(\lambda \Varid{p}\to \Varid{f}\;(\lambda \Varid{x}\to \mathbf{let}\;(\Varid{r},\Varid{y})\mathrel{=}\Varid{p}\;\Varid{x}\;\mathbf{in}\;(\Varid{r},\Varid{snd}\;(\Varid{r},\Varid{y})))){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Simplify}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{f}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
\end{proof}

Embedding $K$ selection functions into the new $GK$ type is a little bit more tricky. We
essentially need to make sure that $g$ is not changing the $R$ value after applying $p$ to
it's elements. Therefore 


\begin{proof}[GK to K Embedding]
The equality (k2gk . gk2k) g = g is established through the following steps:

Assuming that for 
g :: GK r a
forall p :: forall b . (a -> (r,b))
exists x :: a
such that:
g p = p x

\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[B]{}(\Varid{k2gk}\mathbin{\circ}\Varid{gk2k})\;\Varid{g}{}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Definitions}\;\Varid{and}\;\Varid{rewrite}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\lambda \Varid{p}\to \Varid{snd}\;(\Varid{g}\;(\lambda \Varid{x}\to \mathbf{let}\;(\Varid{r},\Varid{y})\mathrel{=}\Varid{p}\;\Varid{x}\;\mathbf{in}\;(\Varid{r},(\Varid{r},\Varid{y})))){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Assumption}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\lambda \Varid{p}\to \Varid{snd}\;(\Varid{exist}\;\Varid{x}\to \mathbf{let}\;(\Varid{r},\Varid{y})\mathrel{=}\Varid{p}\;\Varid{x}\;\mathbf{in}\;(\Varid{r},(\Varid{r},\Varid{y}))){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{exists}\;\Varid{commuts}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\lambda \Varid{p}\to \Varid{exists}\;\Varid{x}\to \mathbf{let}\;(\Varid{r},\Varid{y})\mathrel{=}\Varid{p}\;\Varid{x}\;\mathbf{in}\;\Varid{snd}\;(\Varid{r},(\Varid{r},\Varid{y})){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Assumption}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\lambda \Varid{p}\to \Varid{g}\;(\lambda \Varid{x}\to \mathbf{let}\;(\Varid{r},\Varid{y})\mathrel{=}\Varid{p}\;\Varid{x}\;\mathbf{in}\;\Varid{snd}\;(\Varid{r},(\Varid{r},\Varid{y}))){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Simplify}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{g}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
\end{proof}



- counterexamples to ilustrate what precondition means and why we want it
- introduce new theorem baced on free theorem and precondition
- calculate monad definition from k2j and j2k

GK forms a monad
================

The monad definition for $GK$ is straightforward:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{bindGK}\mathbin{::}\Conid{GK}\;\Varid{r}\;\Varid{a}\to (\Varid{a}\to \Conid{GK}\;\Varid{r}\;\Varid{b})\to \Conid{GK}\;\Varid{r}\;\Varid{b}{}\<[E]%
\\
\>[3]{}\Varid{bindGK}\;\Varid{e}\;\Varid{f}\;\Varid{p}\mathrel{=}\Varid{e}\;(\lambda \Varid{x}\to \Varid{f}\;\Varid{x}\;\Varid{p}){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
Given a selection function $e$ of type $GK R A$, a function $f$ of type 
$A \rightarrow GK R A$, and a property function $p$ of type 
$\forall C. (B \rightarrow (R,C))$, the result of type $(R,C)$ can be constructed by 
utilizing $e$. Each underlying element $x$ of type $a$ of $e$ will be assessed based on 
the values produced by applying $f$ to each element $x$. This process results in a pair 
comprising the $R$ value by which the outcome is judged and the result value of type $C$. 
Since this pair is already of the correct type, it is sufficient to simply return it.
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{returnGK}\mathbin{::}\Varid{a}\to \Conid{GK}\;\Varid{r}\;\Varid{a}{}\<[E]%
\\
\>[3]{}\Varid{returnGK}\;\Varid{x}\;\Varid{p}\mathrel{=}\Varid{p}\;\Varid{x}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
The proofs for the monad laws are attached in the appendix.

With these monad definitions, we'd like to investigate how they relate to the definitions 
for $J$ or $K$ respectively. We'd like the $GK$ monad to behave in the same way as the $J$
and $K$ monad does.

In order to dirive we need to introduce the following two theorems:
\begin{theorem}[Theorem 1]
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[B]{}\Varid{f}\mathbin{::}(\Varid{r},\Varid{a})\to (\Varid{r},\Varid{b}){}\<[E]%
\\
\>[B]{}\Varid{g}\mathbin{::}\Conid{K}\;\Varid{r}\;\Varid{x}{}\<[E]%
\\
\>[B]{}\Varid{p}\mathbin{::}\Varid{x}\to (\Varid{r},\Varid{a}){}\<[E]%
\\
\>[B]{}\Varid{f}\;(\Varid{g}\;\Varid{p})\mathrel{=}\Varid{g}\;(\Varid{f}\mathbin{\circ}\Varid{p}){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
iff (fst . f . p) = fst . p

\end{theorem}

\begin{proof}[Theorem 1]
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{22}{@{}>{\hspre}l<{\hspost}@{}}%
\column{25}{@{}>{\hspre}l<{\hspost}@{}}%
\column{28}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[B]{}\Conid{Assuming}\;\Varid{that}\;\Varid{for}{}\<[E]%
\\
\>[B]{}\Varid{g}\mathbin{::}\Conid{GK}\;\Varid{r}\;\Varid{a}{}\<[E]%
\\
\>[B]{}\Varid{forall}\;\Varid{p}\mathbin{::}\Varid{forall}\;\Varid{b}\mathbin{\circ}(\Varid{a}\to (\Varid{r},\Varid{b})){}\<[E]%
\\
\>[B]{}\Varid{exists}\;\Varid{x}\mathbin{::}\Varid{a}{}\<[E]%
\\
\>[B]{}\Varid{such}\;\Varid{that}\mathbin{:}{}\<[E]%
\\
\>[B]{}\Varid{g}\;\Varid{p}\mathrel{=}\Varid{p}\;\Varid{x}{}\<[E]%
\\[\blanklineskip]%
\>[B]{}\Varid{f}\;(\Varid{g}\;\Varid{p}){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Assumption}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{exists}\;\Varid{x}\to \Varid{f}\;(\Varid{p}\;\Varid{x}){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{rewrite}\;\Varid{as}\;\Varid{tuple}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}((\Varid{fst}\mathbin{\circ}\Varid{f}\mathbin{\circ}\Varid{p})\;\Varid{x},(\Varid{snd}\mathbin{\circ}\Varid{f}\mathbin{\circ}\Varid{p})\;\Varid{x}){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Theorem}\;\mathrm{1}\;\Varid{condition}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}((\Varid{fst}\mathbin{\circ}\Varid{p})\;\Varid{x},(\Varid{snd}\mathbin{\circ}\Varid{f}\mathbin{\circ}\Varid{p})\;\Varid{x}){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{rewrite}\;\Varid{as}\;\mathbf{let}\;\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\mathbf{let}\;(\Varid{r},\Varid{y})\mathrel{=}\Varid{p}\;\Varid{x}\;\mathbf{in}\;(\Varid{r},\Varid{snd}\;(\Varid{f}\;(\Varid{r},\Varid{y}))){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{rewrite}\;\Varid{as}\mathbin{***}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\mathbf{let}\;(\Varid{r},\Varid{y})\mathrel{=}\Varid{p}\;\Varid{x}\;\mathbf{in}\;(\Varid{id}\mathbin{***}(\lambda \Varid{y}\to \Varid{snd}\;(\Varid{f}\;(\Varid{r},\Varid{y}))))\;(\Varid{r},\Varid{y}){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{resolve}\mathbin{***}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\mathbf{let}\;(\Varid{r},\Varid{y})\mathrel{=}\Varid{p}\;\Varid{x}\;\mathbf{in}\;(\lambda (\Varid{a},\Varid{b})\to (\Varid{a},(\lambda \Varid{y}\to \Varid{snd}\;(\Varid{f}\;(\Varid{r},\Varid{y})))\;\Varid{b}))\;(\Varid{r},\Varid{y}){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{apply}\;\Varid{lambda}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\mathbf{let}\;(\Varid{r},\Varid{y})\mathrel{=}\Varid{p}\;\Varid{x}\;\mathbf{in}\;(\lambda (\Varid{a},\Varid{b})\to (\Varid{a},\Varid{snd}\;(\Varid{f}\;(\Varid{r},\Varid{b}))))\;(\Varid{r},\Varid{y}){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{expand}\;\mathbf{let}\;\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\mathbf{let}\;\Varid{r}\mathrel{=}\Varid{fst}\;(\Varid{p}\;\Varid{x})\;\mathbf{in}\;\mathbf{let}\;\Varid{y}\mathrel{=}\Varid{snd}\;(\Varid{p}\;\Varid{x})\;\mathbf{in}\;(\lambda (\Varid{a},\Varid{b})\to (\Varid{a},\Varid{snd}\;(\Varid{f}\;(\Varid{r},\Varid{b}))))\;(\Varid{r},\Varid{y}){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{remove}\;\mathbf{let}\;\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}(\lambda (\Varid{a},\Varid{b})\to (\Varid{a},\Varid{snd}\;(\Varid{f}\;(\Varid{fst}\;(\Varid{p}\;\Varid{x}),\Varid{b}))))\;((\Varid{fst}\;(\Varid{p}\;\Varid{x})),(\Varid{snd}\;(\Varid{p}\;\Varid{x}))){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{simplify}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}(\lambda (\Varid{a},\Varid{b})\to (\Varid{a},\Varid{snd}\;(\Varid{f}\;(\Varid{fst}\;(\Varid{p}\;\Varid{x}),\Varid{b}))))\;\Varid{p}\;\Varid{x}{}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{remove}\;\Varid{patternmatch}\;\mathbf{in}\;\Varid{lambda}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}(\lambda \Varid{a}\to (\Varid{fst}\;\Varid{a},\Varid{snd}\;(\Varid{f}\;(\Varid{fst}\;(\Varid{p}\;\Varid{x}),\Varid{snd}\;\Varid{a}))))\;\Varid{p}\;\Varid{x}{}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{replace}\;(\Varid{p}\;\Varid{x})\;\Varid{with}\;\Varid{a}\;\Varid{within}\;\Varid{lambda}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}(\lambda \Varid{a}\to (\Varid{fst}\;\Varid{a},\Varid{snd}\;(\Varid{f}\;(\Varid{fst}\;\Varid{a},\Varid{snd}\;\Varid{a}))))\;\Varid{p}\;\Varid{x}{}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{add}\;\Varid{patternmatch}\;\Varid{to}\;\Varid{lamvda}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}(\lambda (\Varid{r},\Varid{y})\to (\Varid{r},\Varid{snd}\;(\Varid{f}\;(\Varid{r},\Varid{y}))))\;\Varid{p}\;\Varid{x}{}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Assumption}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}(\lambda (\Varid{r},\Varid{y})\to (\Varid{r},\Varid{snd}\;(\Varid{f}\;(\Varid{r},\Varid{y}))))\;\Varid{g}\;\Varid{p}{}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{free}\;\Conid{Theorem}\;\Varid{for}\;\Conid{GK}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{g}\;((\lambda (\Varid{r},\Varid{y})\to (\Varid{r},{}\<[22]%
\>[22]{}\Varid{snd}\;(\Varid{f}\;(\Varid{r},\Varid{y}))))\mathbin{\circ}\Varid{p}){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{rewrite}\;(\mathbin{\circ})\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{g}\;(\lambda \Varid{x}\to (\lambda (\Varid{r},\Varid{y})\to (\Varid{r},{}\<[28]%
\>[28]{}\Varid{snd}\;(\Varid{f}\;(\Varid{r},\Varid{y}))))\;(\Varid{p}\;\Varid{x})){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{pull}\;(\Varid{p}\;\Varid{x})\;\Varid{into}\;\Varid{lambda}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{g}\;(\lambda \Varid{x}\to (\Varid{fst}\;(\Varid{p}\;\Varid{x}),{}\<[25]%
\>[25]{}\Varid{snd}\;(\Varid{f}\;(\Varid{fst}\;(\Varid{p}\;\Varid{x}),\Varid{snd}\;(\Varid{p}\;\Varid{x}))))){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{simplify}\;\Varid{tuple}\;\Varid{to}\;\Varid{p}\;\Varid{x}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{g}\;(\lambda \Varid{x}\to (\Varid{fst}\;(\Varid{p}\;\Varid{x}),{}\<[25]%
\>[25]{}\Varid{snd}\;(\Varid{f}\;(\Varid{p}\;\Varid{x})))){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{rewrite}\;\Varid{with}\;(\mathbin{\circ})\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{g}\;(\lambda \Varid{x}\to ((\Varid{fst}\mathbin{\circ}\Varid{p})\;\Varid{x},(\Varid{snd}\mathbin{\circ}\Varid{f}\mathbin{\circ}\Varid{p})\;\Varid{x})){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{expand}\;\Varid{first}\;\Varid{bit}\;\Varid{with}\;\Varid{theorem}\;\Varid{condition}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{g}\;(\lambda \Varid{x}\to ((\Varid{fst}\mathbin{\circ}\Varid{f}\mathbin{\circ}\Varid{p})\;\Varid{x},(\Varid{snd}\mathbin{\circ}\Varid{f}\mathbin{\circ}\Varid{p})\;\Varid{x})){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{simplify}\;\Varid{tuple}\;\Varid{to}\;(\Varid{f}\mathbin{\circ}\Varid{p})\;\Varid{x}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{g}\;(\lambda \Varid{x}\to (\Varid{f}\mathbin{\circ}\Varid{p})\;\Varid{x}){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Varid{remove}\;\Varid{lambda}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{g}\;(\Varid{f}\mathbin{\circ}\Varid{p}){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
\end{proof}

To further simplify the calculation we aslso introduce the following theorem:

\begin{theorem}[Theorem 2]
If q does apply p to get the r value but keeps the original value, and we then use that 
original value to compute the (r,z) values with p we can call g with p directly
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[B]{}\mbox{\onelinecomment  p :: x -> (r,y)}{}\<[E]%
\\
\>[B]{}\mbox{\onelinecomment  g :: K r x}{}\<[E]%
\\
\>[B]{}\mbox{\onelinecomment  p (snd (g q)) = g p}{}\<[E]%
\\
\>[B]{}\mbox{\onelinecomment     where q = (\x -> ((fst . p) x, x))}{}\<[E]%
\ColumnHook
\end{hscode}\resethooks
\end{theorem}

And we can proof Theorem 2 by utilising Theorem 1.
\begin{proof}[Theorem 1]
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[B]{}(\Varid{p}\mathbin{\circ}\Varid{snd})\;(\Varid{g}\;\Varid{q}){}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{g}\;(\lambda \Varid{x}\to (\Varid{p}\mathbin{\circ}\Varid{snd})\;((\Varid{fst}\mathbin{\circ}\Varid{p})\;\Varid{x},\Varid{x})){}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{g}\;\Varid{p}{}\<[E]%
\\[\blanklineskip]%
\>[B]{}\Varid{iff}{}\<[E]%
\\
\>[B]{}(\Varid{fst}\mathbin{\circ}\Varid{p}\mathbin{\circ}\Varid{snd})\;(\lambda \Varid{x}\to ((\Varid{fst}\mathbin{\circ}\Varid{p})\;\Varid{x},\Varid{x})){}\<[E]%
\\
\>[B]{}\mathrel{=}\lambda \Varid{y}\to (\Varid{fst}\;(\Varid{p}\;(\Varid{snd}\;((\lambda \Varid{x}\to ((\Varid{fst}\mathbin{\circ}\Varid{p})\;\Varid{x},\Varid{x}))\;\Varid{y})))){}\<[E]%
\\
\>[B]{}\mathrel{=}\lambda \Varid{y}\to (\Varid{fst}\;(\Varid{p}\;(\Varid{snd}\;((\Varid{fst}\mathbin{\circ}\Varid{p})\;\Varid{y},\Varid{y})))){}\<[E]%
\\
\>[B]{}\mathrel{=}\lambda \Varid{x}\to (\Varid{fst}\mathbin{\circ}\Varid{p})\;\Varid{x}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{fst}\mathbin{\circ}(\lambda \Varid{x}\to ((\Varid{fst}\mathbin{\circ}\Varid{p})\;\Varid{x},\Varid{x})){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
\end{proof}

-- TODO: Give an intuition what these theorems mean

Now, consider the following two operators that transform between $GK$ selection functions
and $J$ selection functions:
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{j2gk}\mathbin{::}\Conid{J}\;\Varid{r}\;\Varid{x}\to \Conid{GK}\;\Varid{r}\;\Varid{x}{}\<[E]%
\\
\>[3]{}\Varid{j2gk}\;\Varid{f}\;\Varid{p}\mathrel{=}\Varid{p}\;(\Varid{f}\;(\Varid{fst}\mathbin{\circ}\Varid{p})){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{3}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[3]{}\Varid{gk2j}\mathbin{::}\Conid{GK}\;\Varid{r}\;\Varid{x}\to \Conid{J}\;\Varid{r}\;\Varid{x}{}\<[E]%
\\
\>[3]{}\Varid{gk2j}\;\Varid{f}\;\Varid{p}\mathrel{=}\Varid{snd}\;(\Varid{f}\;(\lambda \Varid{x}\to (\Varid{p}\;\Varid{x},\Varid{x}))){}\<[E]%
\ColumnHook
\end{hscode}\resethooks
We can calculate the bind implementation for $GK$ with the $j2gk$ and $gk2j$ operators and 
the previusly introduced theorems:

\begin{proof}[GK Monad behaves similar to J]
\begin{hscode}\SaveRestoreHook
\column{B}{@{}>{\hspre}l<{\hspost}@{}}%
\column{E}{@{}>{\hspre}l<{\hspost}@{}}%
\>[B]{}\Varid{j2gk}\;((\bind )\;(\Varid{gk2j}\;\Varid{f})\;(\lambda \Varid{x}\to \Varid{gk2j}\;(\Varid{g}\;\Varid{x}))){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Definition}\;\mathbf{of}\;(\bind )\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{j2gk}\;((\lambda \Varid{f}\;\Varid{g}\;\Varid{p}\to \Varid{g}\;(\Varid{f}\;(\Varid{p}\mathbin{\circ}\Varid{flip}\;\Varid{g}\;\Varid{p}))\;\Varid{p})\;(\Varid{gk2j}\;\Varid{f})\;(\lambda \Varid{x}\to \Varid{gk2j}\;(\Varid{g}\;\Varid{x}))){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Simplify}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\Varid{j2gk}\;(\lambda \Varid{p}\to \Varid{gk2j}\;(\Varid{g}\;(\Varid{gk2j}\;\Varid{f}\;(\Varid{p}\mathbin{\circ}(\lambda \Varid{x}\to \Varid{gk2j}\;(\Varid{g}\;\Varid{x})\;\Varid{p}))))\;\Varid{p}){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Definition}\;\mathbf{of}\;\Varid{j2gk}\;\Varid{and}\;\Varid{rewrite}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\lambda \Varid{p}\to \Varid{p}\;(\Varid{gk2j}\;(\Varid{g}\;(\Varid{gk2j}\;\Varid{f}\;(\lambda \Varid{x}\to \Varid{fst}\;((\Varid{p}\mathbin{\circ}\Varid{snd})\;((\Varid{g}\;\Varid{x})\;(\lambda \Varid{x}\to ((\Varid{fst}\mathbin{\circ}\Varid{p})\;\Varid{x},\Varid{x})))))))\;(\Varid{fst}\mathbin{\circ}\Varid{p})){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Theorem}\;\mathrm{1}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\lambda \Varid{p}\to \Varid{p}\;(\Varid{gk2j}\;(\Varid{g}\;(\Varid{gk2j}\;\Varid{f}\;(\lambda \Varid{x}\to \Varid{fst}\;(((\Varid{g}\;\Varid{x})\;(\lambda \Varid{x}\to (\Varid{p}\mathbin{\circ}\Varid{snd})\;((\Varid{fst}\mathbin{\circ}\Varid{p})\;\Varid{x},\Varid{x})))))))\;(\Varid{fst}\mathbin{\circ}\Varid{p})){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Definition}\;\mathbf{of}\;\Varid{j2gk}\;\Varid{and}\;\Varid{rewrite}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\lambda \Varid{p}\to \Varid{p}\;(\Varid{snd}\;(\Varid{g}\;(\Varid{snd}\;(\Varid{f}\;(\lambda \Varid{x}\to (\Varid{fst}\;(\Varid{g}\;\Varid{x}\;\Varid{p}),\Varid{x}))))\;(\lambda \Varid{x}\to ((\Varid{fst}\mathbin{\circ}\Varid{p})\;\Varid{x},\Varid{x})))){}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Theorem}\;\mathrm{2}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\lambda \Varid{p}\to \Varid{g}\;(\Varid{snd}\;(\Varid{f}\;(\lambda \Varid{x}\to (\Varid{fst}\;(\Varid{g}\;\Varid{x}\;\Varid{p}),\Varid{x}))))\;\Varid{p}{}\<[E]%
\\
\>[B]{}\{\mskip1.5mu \{\mskip1.5mu \Conid{Rewrite}\mskip1.5mu\}\mskip1.5mu\}{}\<[E]%
\\
\>[B]{}\mathrel{=}\lambda \Varid{p}\to (\lambda \Varid{y}\to \Varid{g}\;(\Varid{snd}\;\Var