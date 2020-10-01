---
title: "LTEP presentation"
subtitle: "Detect paradoxes resulting from plan change"
author: "Ida Sandberg Motzfeldt"
theme: metropolis
colortheme: wolverine
---

# Goal
- Detect paradoxes resulting from plan *change*
\pause
- What is a change?  
\pause
For now:
\pause
  - ADD operation
  \pause
  - REMOVE operation

# Example
\only<1, 2>{
\includegraphics[trim=0 250 0 192, clip, width=\textwidth]{./img/example.pdf}

\pause

\begin{itemize}
  \item Plan change: REMOVE removeFeature(3) at time 5.
\end{itemize}
}

\only<3 ->{
\includegraphics[trim=0 250 0 192, clip, width=\textwidth]{./img/example-cont.pdf}

}
\only<4 ->{
\begin{itemize}
  \item The change causes a paradox at time 8, since group 6 is not empty when we plan to remove it.
\end{itemize}
}

# Starting point
- Identify the effects of adding or removing operations
\pause
  - Which part of the plan may be affected by the change?  
  \pause For instance, ADD changeFeatureType(A, Optional) only affects the plan until A's type is changed next
\pause
- Create a new data structure for storing the information, optimized for modular soundness checking
  \pause
  - The current plan is to store the information in maps of validities

# How can you help?
- I need one large evolution plan containing all of the possible operations
\pause
- I would also like some small evolution plans which are specifically meant for testing specific cases 
\pause
  - e.g. when ADD moveFeature conflicts with some later planned move

# Questions?
\center (: ?
