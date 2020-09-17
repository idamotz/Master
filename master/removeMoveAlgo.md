---
title: "REMOVE move algo"
---

Based on ADD move algo, since removing a move operation is equivalent to adding the inverse move right after the first move. 

Let $\texttt{move}(m, n)$ be the operation to be removed, with $c_1$ the target node and $n$ the node to move there. The inverse is then $\texttt{move}(m, \texttt{parent}(m))$ (since the root cannot be moved, we know that the parent is a node). Let $c_1 = \texttt{parent}(m)$. Let $t_1$ be the time point for the move operation. Check ancestors
