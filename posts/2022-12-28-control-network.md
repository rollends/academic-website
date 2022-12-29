---
title :   "Control of Software Systems: Load Balancing"
author:   "Rollen S. D'Souza"
date  :   "2022-12-28"
---
Given two worker processes which start and complete tasks at a service rate of $\alpha^1$ and $\alpha^2$ tasks every $100~\mathrm{ms}$, what is the optimal choice of worker to take on a new task that ensures the fastest task completion throughput?
An effective choice depends on the current number of tasks each worker is assigned as their respective service rates.
Consider the following example.
Say Worker A performed tasks at rate $\alpha^1 = 2$ while Worker B performed tasks at rate $\alpha^2 = 1$ and suppose we had to assign a new task to one of the two workers.
To do this, we first observe the number of tasks already assigned.
Say we observe there are three tasks assigned to Worker A and two tasks assigned to Worker B.

The correct choice is to assign the new task to worker A --- the fastest worker --- since it will only take $200~\mathrm{ms}$ to complete all the tasks.
This is not always the best choice.
Suppose, instead, that Worker A had *four* tasks assigned to it.

In this case, it is better to assign the new task to Worker B because we can still finish all the tasks in $200~\mathrm{ms}$.
Had we assigned the task to Worker A, it would take $300~\mathrm{ms}$ to complete all the tasks and Worker B would be doing nothing in that last $100~\mathrm{ms}$ period.
Is there a formal way to arrive at this observation, and, better still, have an algorithm that tells us the best choice to make given the current configuration of the system?

This is a problem of load balancing with static assignment: assigning tasks to workers to ensure the fastest throughput where a task is assigned exactly once.
The problem can be cast as a problem of control theory.
Control theory is the formal study of driving the configuration of a real-world system to a desired configuration by leveraging current information.
This is depicted in the diagram below for this problem.

At the start of every $100~\mathrm{ms}$ period, the control algorithm observes the current state of the queues behind every worker and assigns all unassigned tasks.
Then, during the $100~\mathrm{ms}$ period, every worker completes as many tasks as it can: the minimum of the number of tasks in its queue and its service rate.
We can describe this mathematically.
Formally, let $x^1_k$, $x^2_k \in \mathbb{Z}_{\geq 0}$ be the number of tasks assigned to Worker A and B respectively at period $k$ and let $x^3_k \in \mathbb{Z}_{\geq 0}$ be the number of unassigned tasks at the start of period $k$.
Let $u^1_k$, $u^2_k \in \mathbb{Z}_{\geq 0}$ denote the number of additional tasks to assign to Worker A and B respectively in period $k$.
The number of tasks changes from period $k$ to period $k+1$ according to the following discrete-time law,
$$
\begin{aligned}
    x^1_{k+1} &= \max\{0, x^1_{k} - \alpha^1 + u^1_k\},\\
    x^2_{k+1} &= \max\{0, x^2_{k} - \alpha^2 + u^2_k\},\\
    x^3_{k+1} &= \max\{0, x^3_{k} - u^1_k - u^2_k + w_k\},
\end{aligned}
$$
where $w_k$ denotes the (unknown) number of new unassigned tasks.
We will assume that $w_k = 0$:
we only need to service $x^3_0$ tasks for all time.
The $\max$ ensures that we do not service (assign) more tasks than are actually queued behind a worker (load balancer).
The diagram below depicts the mathematical law aptly.

The goal of the load balancing problem is to maximize the task throughput.
This is a hard quantity to describe in this framing.
However, we can change the problem slightly to one that is more easily framed in this setting.
Instead suppose we wish to complete as many tasks as possible in $500~\mathrm{ms}$ (five periods).
This is a finite-time variation of the original problem where we only care about our throughput in the near-term.
In this setting, we can easily evaluate the theoretical throughput.
Define
$$
    y^i_k = \min\{x^i_k + u^i_k, \alpha^i\}.
$$
The number of tasks completed in five periods is
$$
    \sum_{k=1}^{5} y^1_k + y^2_k.
$$
Maximizing this quantity over all possible task assignments,
$$
    (u^1_1, u^2_1), \ldots, (u^1_5, u^2_5),
$$
maximizes the throughput over the $500~\mathrm{ms}$ window.
This suggests that a solution to the optimization problem,

achieves maximum throughput.
The optimization problem rewards task assignments that result in a higher throughput.
However, we also want to make sure all unassigned tasks are assigned at any given period.
To ensure this, we add a *penalty* to the reward for any remaining unassigned tasks by subtracting $\sum_{k=1}^5 x^3_k$ from the reward.
This leaves us with the final optimization problem,

Great.
Now, how do we solve this?
Iterating over all possible choices for $u_k$ is *very* expensive.
A common strategy to take is to apply the method of dynamic programming.
We will take an approach along this thread.
Note that the reward function can be rewritten as,
$$
    \left(y^1_5 + y^2_5 - x^3_5\right) + \left(\sum_{k=1}^{4} y^1_k + y^2_k - \sum_{k=1}^4 x^3_k\right),
$$
and observe that the latter sum is simply the reward function for the same problem except over a $400~\mathrm{ms}$ window.
That is, we observe that,

> The optimal policy to achieve maximum throughput over a $500~\mathrm{ms}$ window implies knowledge of the optimal policy to achieve maximum throughput over a $400~\mathrm{ms}$ window.

A dynamic programming algorithm can be developed for this, but the resulting table will be quite large.
To avoid this, we recast this problem as a problem of graph theory and rely on an algorithm that will scale well for larger problems
Consider the diagram below.

Each node in the graph is a configuration of the system (number of tasks in all the queues) at a given time.
Each edge corresponds to a task assignment at that given time and how transfers the current configuration to the next configuration.
The cost of each transition is precisely the number of tasks that will be completed by the system,
$$
    y^1_k + y^2_k - x^3_k,
$$
with the additional penalty for having unassigned tasks.
The problem of finding the highest cost path from an initial configuration (the current one) to any final configuration $(x^1_6, x^2_6, x^3_6)$ is precisely the problem we aim to solve.
Knowing the optimal path over the five periods implies knowing the optimal path from an initial configuration to the intermediate note $(x^1_5, x^2_5, x^3_5)$ over the four periods.
[Dijkstra's Algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm) can be used to find the optimal path.

> **Note**: This algorithm cannot be used if $u^i_k$ has no upper bound.
> The relationship between $x^3_{k+1}$ and $x^3_{k}$ ensure that $u^i_k$ are bounded from above, which ensures there are only a finite number of edges from any given configuration.
> In practice, this is not a major issue since the control action is usually finite in software applications.
> A different approach is used when the control action lay in a continuum ($\mathbb{R}$).


