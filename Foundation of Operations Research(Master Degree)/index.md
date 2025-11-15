---
title: "Foundation of Operational Research"
author:
- "Ortore Joele Andrea"
---

## Linear Programming 
A linear programming problem is an optimization problem where:

- **The objective function** $F : X \to R$  is linear , where X is the feasible region.
- **The feasible region**  has linear constraints.

:::{.callout .callout-definition title="Optimal solution"}
A solution $\underline{x}^* \in R^n$ is said to be **optimal** if  $f(\underline{x}^*)$ beats $f(\underline{x}), \forall \underline{x} \in X$.
:::
### Traditional representations:

<div style="display:flex; justify-content:space-between; width:100%;">

<div style="flex:1; padding-right:10px;">
<h4>General form</h4>

$$ 
    \begin{align*}
     min \quad &z = c_1 x_1 + \dots + c_nx_n \\
    & a_{11} + \dots + a_{1n}x_n (\geq,=,\leq) b_1\\
    &\vdots\\
    & a_{m1}x_1+\dots + a_{mn}x_n (\geq,=,\leq) b_m\\
    &x_1,\dots,x_n \geq 0
    \end{align*}
$$

</div>

<div style="flex:1; padding-left:10px;">
<h4>Matrix form </h4>
<div style="text-align: center;">

$min \quad z = [c_1 \dots c_n] 
\begin{bmatrix}
x_1 \\
\vdots\\
x_n
\end{bmatrix}$

</div>

<div style="display:flex; justify-content:space-between; width:100%;">

<div style="flex:1; padding-right:10px;">

$$
\begin{align*}
min \quad &z = \underline{c}^T \underline{x} \\
s.t.\quad  & A\underline{x} \geq \underline{b} \\
&\underline{x} \geq 0
\end{align*}
$$

</div>

<div style="flex:1; padding-left:10px;">
<div style="text-align: center;">


</div>

$$
s.t. \quad 
\begin{bmatrix}
a_{11} &  \dots & a_{1n} \\
\vdots & & \vdots\\
a_{m1} & \dots & a_{mn}
\end{bmatrix}
\begin{bmatrix}
x_1  \\
\vdots \\
x_n
\end{bmatrix}
\geq
\begin{bmatrix}
b_1  \\
\vdots \\
b_m
\end{bmatrix}
$$

</div>
</div>

$$
\begin{bmatrix}
x_1  \\
\vdots \\
x_n
\end{bmatrix}
\geq \underline{0}
$$
</div>
</div>

### General Assumptions
:::{.callout .callout-property title="Assumptions"}
1. Consequences of the linearity: 

   -  **Proportionality**: Contribution of each variable $=$ constant $\times$ variable , we often refer to these constants as parameters.
   -  **Additivity**: Contribution of all variable $=$ sum of single contributions : $f(x + y + z) = f(x) + f(y) + f(z)$


2. In these types of problems variables can take any rational values.


3. Parameters are constants.
:::
#### Example:
 Suppose that we want to maximize how much we gain from the selling of 5 products ( suppose that we sell and produce the products in grams) , the price/g of each one of them are respectively: 2, 3, 4, 5, 6; the cost of production of each one of them is: 3, 6, 7, 9, 10; no more than 400g must be produced and the total production cost must be lower than 3000.
We shall model this problem like this:

<div style="display:flex; justify-content:space-between; width:100%;">

<div style="flex:1; padding-right:10px;">
<h4>Traditional modelling</h4>

$$  
 \begin{align*}
        max  \quad & 2\mathscr{x}_1 + 3\mathscr{x}_2 + 4\mathscr{x}_3 + 5\mathscr{x}_4 + 6\mathscr{x}_5 \\
        s.t. \quad & 3\mathscr{x}_1 + 6\mathscr{x}_2 + 7\mathscr{x}_3 + 9\mathscr{x}_4 + 10\mathscr{x}_5 \leq 3000 \quad (1)\\
        & \mathscr{x}_1 + \mathscr{x}_2 + \mathscr{x}_3 + \mathscr{x}_4 + \mathscr{x}_5 \leq 400\quad(2)\\
        & \mathscr{x}_i \geq 0, \forall i \in {1,2,3,4,5}
\end{align*}
$$
1. Maximum production cost
2. Maximum quantity produced
</div>

<div style="flex:1; padding-left:10px;">
<h4>Using python's MIP library:</h4>

```python 
    import mip
    from mip import CONTINUOUS
    
    # these are our parameters
    I = [0, 1, 2, 3, 4] # Products
    P = [2, 3, 4, 5, 6] # Prices
    C = [3, 6, 7, 9, 10] # Costs
    Cmax = 3000 # Max total cost
    Qmax = 400 # Max total amount of product
    
    
    model=mip.Model()
    x = [model.add_var(name=f"x_{i+1}", lb=0 ,var_type=CONTINUOUS) for i in I]
    
    # Objective function
    model.objective = mip.maximize(mip.xsum(P[i] * x[i] for i in I))
    
    # Constraints
    
    # Maximum production cost
    model.add_constr(mip.xsum(x[i]*C[i] for i in I) <= 3000)
    
    # Maximum quantity produced
    model.add_constr(mip.xsum(x[i] for i in I)<= 400)
    
    model.optimize()
    for i in model.vars:
    print(i.name,i.x)
```
</div>
</div>


## Geometry of linear Programming
### A couple of definitions:
- A **hyperplane** defined as H = {$\,\underline{x} \in R^n : \underline{a}^T \underline{x} = b\,$} is a flat surface that generalize a two-dimensional plane (in our case where b $\ne$ 0 is said to be an **affine** hyperplane).
- An **affine half-space** defined as $H^-$ = {$\,\underline{x} \in R^n:\underline{a}^T \underline{x} \leq b \,$} is the region that lies "below" or "above" an **affine hyperplane**.
 
> Each inequality constraint defines an affine half-space in the variable space.

> The feasible region of a linear programming problem is the intersection of a finite number of half-spaces (constraints).
> Said feasible region is a **polyhedron**.

<div style="display:flex; justify-content:space-between; width:100%;">

<div style="flex:1; padding-right:10px;">
<h4>Hyperplane (affine) </h4>

![](assets/chapter3/hyperplane.png)

</div>

<div style="flex:1; padding-left:10px;">
<h4>Affine half-space</h4>

![](assets/chapter3/affine_half-space.png)

</div>
<div style="flex:1; padding-left:10px;">
<h4>Polyhedron</h4>

![](assets/chapter3/polyhedron.png)

</div>
</div>

### Convex subsets
:::{.callout .callout-definition title="Convex subsets" }
A subset $S \subseteq R^n$ is convex if for each pair of points $\underline{x}_1,\underline{x}_2 \in S$ the segment defined by them is the defined by all the **convex combinations** of the two points:
$$
 [\underline{x}_1,\underline{x}_2] = {\:\underline{p} \in R^n : \underline{x} = \alpha\underline{x}_1 + (1 - \alpha)\underline{x}_2 \, \land \alpha \in [0,1]\:}
$$
:::

Meaning that the subset contains the whole segment connecting the two points.

![](assets/chapter3/convexset.png)

[Convex set interactive graph](https://www.desmos.com/calculator/y3ays9vrh3?lang=it)

> A *polyhedron* is a convex set of $R^n$ $\implies$ The feasible region of a LP is a convex set of $R^n$

### Vertices

>A vertex of a polyhedron P is a point which cannot be expressed as a convex combination of two other **dinstinct** points of P.

Given a point $\underline{x}$ and a pair of points $(\underline{y}_1,\underline{y}_2)$ </br>

$\underline{x}$ is a vertex $\iff \:\underline{x} = \alpha\underline{y}_1 + (1 - \alpha)\underline{y}_2 \, \land \alpha \in [0,1]\: \land (\underline{x}= \underline{y}_1 \lor \underline{x}= \underline{y}_2)$ 

### Unbounded feasible direction of polyhedron
:::{.callout .callout-definition title="Unbounded feasible direction"}
An unbounded feasible direction d of a polyhedron P is a nonzero vector such that ,from a point $\underline{x} \in P$, all points of the form $\underline{x} + \lambda d \:$ with $\lambda > 0$ also belongs to P. The set of such points is often called the **ray** of d through $\underline{x}$.
:::
![](assets/chapter3/unbounded.png)

### Polytopes
:::{.callout .callout-definition title="Polytope"}
A polytope is a **bounded polyhedron** , hence the only unbounded feasible direction is d = 0 (so in a sense it does not have any)
:::

![A polytope](assets/chapter3/polytope.png)

### Representation of polyhedra - Weyl-Minkowsky Theorem
::: {.callout .callout-theorem title="Weyl-Minkowsky Theorem"}
Every point $\underline{x}$ of a polyhedron P can be expressed as a **convex combination** of its vertices $\underline{x}^1, \dots, \underline{x}^k$ plus (if needed) an **unbounded feasible direction** \underline{d} of P:
$$
 \underline{x} = \alpha_1\underline{x}_1 + \dots + \alpha_k \underline{x}_k + \underline{d}
$$
 where $\quad\sum_{i=1}^{k} \alpha_i = 1 ,\:\alpha_i \geq 0 , \forall i \in { 1 \dots k}$ <br>
 "The multipliers are positive and theirs sum must be equal to 1"
:::
## Standard form of LPs

### Transformations rules

- $max \quad \underline{c}^T \underline{x} \implies -min \:(-\underline{c}^T \underline{x})$
 

- $\underline{a}^T \underline{x} \leq b \implies \begin{cases} \underline{a}^T\underline{x} + s = b \\
  s \geq 0
  \end{cases}\quad$ where s is a **slack** variable.
  

- $\underline{a}^T \underline{x} \geq b \implies \begin{cases} \underline{a}^T\underline{x} - s = b \\
  s \geq 0
  \end{cases}\quad$ where s is a **surplus** variable.


- $x_j$ unrestricted sign \implies $\begin{cases} x_j = {x_j}^+\,-\,{x_j}^- \\
  {x_j}^+\, , \, x_j^- \geq 0
  \end{cases}\quad$ <br><br>
After substituting $x_j$ with ${x_j}^+\,-\,{x_j}^-$ , we delete $x_j$ from the problem.

### Standard form 
::: {.callout .callout-definition title="LP standard form"}
An LP is said to be in standard form if this is its appearance:
$$
\begin{align*}
min \quad & z=\underline{c}^T\underline{x} \\
s.t. \quad & A\underline{x} = \underline{b} \\
& \underline{x} \geq \underline{0}
\end{align*}
$$
:::

#### Example:
Let's put the following LP in standard form:

$$  
\begin{align*}
max  \quad & 2\mathscr{x}_1 + 3\mathscr{x}_2 + 4\mathscr{x}_3 + 5\mathscr{x}_4 + 6\mathscr{x}_5 \\
s.t. \quad & 3\mathscr{x}_1 + 6\mathscr{x}_2 + 7\mathscr{x}_3 + 9\mathscr{x}_4 + 10\mathscr{x}_5 \leq 3000 \quad \\
& \mathscr{x}_1 + \mathscr{x}_2 + \mathscr{x}_3 + \mathscr{x}_4 + \mathscr{x}_5 \leq 400\quad\\
& \mathscr{x}_i \geq 0, \forall i \in {1,2,3,4} \: , \mathscr{x}_5 \in R
\end{align*}
$$
  
1. From maximization to minimization
   $$  
   \begin{align*}
   min  \quad & -2\mathscr{x}_1 - 3\mathscr{x}_2 - 4\mathscr{x}_3 - 5\mathscr{x}_4 - 6\mathscr{x}_5 \\
   s.t. \quad & 3\mathscr{x}_1 + 6\mathscr{x}_2 + 7\mathscr{x}_3 + 9\mathscr{x}_4 + 10\mathscr{x}_5 \leq 3000 \quad \\
   & \mathscr{x}_1 + \mathscr{x}_2 + \mathscr{x}_3 + \mathscr{x}_4 + \mathscr{x}_5 \leq 400\quad\\
   & \mathscr{x}_i \geq 0, \forall i \in {1,2,3,4} \: , \mathscr{x}_5 \in R
   \end{align*}
   $$
2. From inequality to equalities by adding slack variables
   $$  
   \begin{align*}
   min  \quad & -2\mathscr{x}_1 - 3\mathscr{x}_2 - 4\mathscr{x}_3 - 5\mathscr{x}_4 - 6\mathscr{x}_5 \\
   s.t. \quad & 3\mathscr{x}_1 + 6\mathscr{x}_2 + 7\mathscr{x}_3 + 9\mathscr{x}_4 + 10\mathscr{x}_5 + S_1 = 3000 \quad \\
   & \mathscr{x}_1 + \mathscr{x}_2 + \mathscr{x}_3 + \mathscr{x}_4 + \mathscr{x}_5 + S_2 = 400\quad\\
   & \mathscr{x}_i \geq 0, \forall i \in {1,2,3,4} \: , \mathscr{x}_5 \in R \\
   & S_1, S_2 \geq 0
   \end{align*}
   $$
3. Substituting unrestricted sign variables
   $$  
   \begin{align*}
   min  \quad & -2\mathscr{x}_1 - 3\mathscr{x}_2 - 4\mathscr{x}_3 - 5\mathscr{x}_4 - 6\mathscr{x}_5 \\
   s.t. \quad & 3\mathscr{x}_1 + 6\mathscr{x}_2 + 7\mathscr{x}_3 + 9\mathscr{x}_4 + 10\mathscr{x}_5 + S_1 = 3000 \quad \\
   & \mathscr{x}_1 + \mathscr{x}_2 + \mathscr{x}_3 + \mathscr{x}_4 + \mathscr{x}_5^+ - \mathscr{x}_5^- + S_2 = 400\quad\\
   & \mathscr{x}_i \geq 0, \forall i \in {1,2,3,4} \:  \\
   & \mathscr{x}_5^+ , \mathscr{x}_5^- \geq 0 \\
   & S_1, S_2 \geq 0
   \end{align*}
   $$
    The problem is now in standard form.
4. 
## Fundamental Theorem of Linear Programming

:::{.callout .callout-theorem title="Fundamental Theorem of Linear Programming"}
Consider a minimization problem in standard form where the constraints define a non-empty feasible area (polyhedron) P.
Then either:

1. The value of **the objective function** is **unbounded below** on P.

2. Exists at least one **optimal** vertex.
:::

 
#### Proof
##### Case 1:
P has an unbounded feasible direction $\underline{d}$ such that $\underline{c}^T\underline{d}<0$ ,this means that proceeding in that direction will make the value smaller and smaller and the objective value tends to -$\infty$.

##### Case 2:
P has no unbounded feasible direction such that along that path the value keeps getting smaller.
As we saw any point of the feasible region can be expressed as a convex combination of its vertices plus the unbounded direction, so for any $\underline{x} \in P$ we have $\underline{d}$ or $\underline{c}^T \underline{d} \geq 0$ (either the value along that direction gets bigger or that direction is $\underline{0}$) so:

$$
    \underline{c}^T\underline{x} = \underline{c}^T ( \sum_{i=1}^{k}{\alpha_i\underline{x}^i + \underline{d}}) =\sum_{i=1}^{k}{\alpha_i\underline{c}^T\underline{x}^i +\underline{c}^T\underline{d}} \geq min_{i=1.\dots,k}{(\underline{c}^T\underline{x}^i)}
$$

Put in words this tells us that the objective value of any point inside the feasible region ($\underline{c}^T (\sum_{i=1}^{k}{\alpha_i\underline{x}^i + \underline{d}})$) will be bigger the minimum value that the objective function has in one of its vertices (the optimal vertex) : $min_{i=1.\dots,k}{(\underline{c}^T\underline{x}^i)}$.

>This also foreshadow that the solution of any minimization problem lies in one of its vertices ( if the solution is unique)

This would likely become more clear when we see how to solve LP problems graphically.

### Types of LPs
1. Unique optimal solution.
2. Multiple(infinite) optimal solutions.
3. Unbounded LP: Unbounded polyhedron and unlimited objective function value.
4. Empty polyhedron: no feasible solutions.

### Solving LPs Graphically

We would like to solve this problem:

$$
\begin{align*}
max &\quad x_1+3x_2 \\
    &\quad -x_1+x_2 \geq-2\\
    &\quad 2x_1+x_2 \leq 10 \\
    &\quad -x_1+3x_2 \leq 9 \\
    &\quad x_1,x_2 \geq 0
\end{align*}
$$

This is our feasible region (x = $x_1$ ; y  = $x_2$):


![feasible region](assets/chapter3/feasibleRegion.png)

Lets analyze the objective function:
$$
 z=x_1+3x_2
$$
Lets compute its gradient:
$$
\nabla z= [ 1 \quad 3]^T
$$

This tells us where to go if we want our value to increase, since we are solving an LP this direction will always be the same.

Let's take a level curve from our objective function by assigning z to some constant c:
$$
c = x_1+3x_2
$$
Starting from 0 we see that by increasing the value of c the level curve follows the direction of the gradient:
![](assets/chapter3/gradientincr.png)

If we follow that direction we would see that it leads us to the vertex $[3 \:,\: 4]^T$ since it the last meeting point it will give us the maximum value of the objective function:

$$
 z = 1 \times 3 + 3 \times 4 = 15
$$

This graphical method is pretty easy to use , but it is feasible only for problems with less than 2 variables.
To solve bigger problems we must find a better method.

#### MIP implementation:
```python
import mip
from mip import CONTINUOUS

model = mip.Model()
# variable definition
x = [model.add_var(name = f"x_{1}", lb =0 ,var_type=CONTINUOUS), model.add_var(name = f"x_{2}", lb =0 ,var_type=CONTINUOUS)]
# objective function
model.objective = mip.maximize(x[0]+3*x[1])
# constraints
model.add_constr(-x[0]+x[1] >= -2)
model.add_constr(2*x[0]+x[1] <= 10)
model.add_constr(-x[0]+3*x[1] <= 9)

model.optimize()
for i in model.vars:
print(i.name,i.x)
```
![Output](assets/chapter3/mip_graph_ex.png)

## Basic feasible solutions and vertices of polyhedra
The geometrical definition of vertex cannot be exploited algorithmically because even though LPs can be viewed as combinatorial problems, and you can theoretically examine only the vertices and keep the best, the number of vertices is often exponential, so this route is not feasible.

**We need algebraic characterization**

Consider a polyhedron (feasible region) P = {$\underline{x} \in R^n : A \underline{x} = \underline{b} , \underline{x} \geq$}

**Assumption**: $A \in R^{m\times n}$ (n variables and m constraints) is such that $m\leq n$ of rank m (There are no redundant constraints)

- If m=n , there is a unique solution for $A\underline{x} = \underline{b}$
- If m<n , there are $\infty$ solutions for $A\underline{x} = \underline{b}$ . The system has n-m degrees of freedom( the number of variables that can freely change while still satisfying the constraints), n-m variables can be fixed arbitrarily and by fixing them to 0 we get a vertex.

:::{.callout .callout-definition title="Basis"}
A basis of a matrix "A" is a subset of its columns "m" that are linearly independent and form an m$\times$m non-singular(invertible) matrix B.
We can separate A like this:

$$
A=[\overbrace{B}^{m} \: |\overbrace{N}^{n-m}]
$$
:::

Similarly to "A" we can also separate the variables in **basic** ("inside" of B) and **non-basic** ("inside" of N) , like so:
$$
\underline{x}=[\:\underline{x}_B^T\quad|\quad\underline{x}_N^T\:]\quad (1)
$$

By substituting (1) into our system: $A\underline{x} = \underline{b}$; we can rewrite it as:
$$
B\underline{x}_B+N\underline{x}_n = \underline{b}
$$

Thanks to the non-singularity of B we can describe the set of basic variables using the non-basic ones:
$$
\underline{x}_B=B^{-1}b-B^{-1}\underline{x}_N
$$

So lets put things together:

- A basic solution is a solution obtained by setting $\underline{x}_N = \underline{0}$ (it's basic because there are no non-basic variables **DUH**), with this our definition of basic variables becomes:
  $\underline{x}_B = B^{-1}b$
- A basic solution with $\underline{x}_B \geq 0$ is a basic **feasible solution** (the problem is in standard form, the variables are positive).

Wait , we said that by fixing n-m variables to 0 we get a vertex , but later we saw that a basic solution is obtained by setting to 0 all the non-basic variables which are also n-m, wonder what that could mean...

Not surprsingly:

:::{.callout .callout-definition title="Radagon is Marika"}
$$
vertex\quad \iff \quad basic\:feasible\:solution
$$
:::

>Number of basic feasible solutions: At least $\binom{n}{m}$ (from the fundamental theorem looking at LPs as combinatorial problems)

## Simplex method
We discussed the need for a method that would work even for bigger problems, this method is the way to go, but before talking about it, we need to discuss a few things.

### Active constraints
Let's take a feasible region:

![](assets/chapter3/feasibleRegion.png)

defined by these constraints:
$$
\begin{align*}
&\quad x_1-x_2 \leq 2\\
&\quad 2x_1+x_2 \leq 10 \\
&\quad -x_1+3x_2 \leq 9 \\
&\quad x_1,x_2 \geq 0
\end{align*}
$$

Let's put them in standard form by adding slack variables $S_1, S_2, S_3$:

$$
\begin{align*}
&\quad x_1-x_2 + S_1 = 2\\
&\quad 2x_1+x_2+ S_2 = 10 \\
&\quad -x_1+3x_2 + S_3 = 9 \\
&\quad x_1,x_2,S_1,S_2,S_3 \geq 0
\end{align*}
$$

If we decide to assign 0 to one of the slack variables,the original constraint becomes an equality, meaning that the variables will "be moving" on the "side" that the constraint describe.

By doing so we say that we **activate** the constraint.

In other words:

:::{.callout .callout-definition title="Active constraint"}
If an assignment of the variables results in a point that lies on one of the borders of the feasible region , the constraint that is responsible for that border is labelled as active.
:::

### Moving from a vertex to another

When we talked about the **LP Fundamental theorem** we saw how enumerating all the vertices and keeping only the best, is not a good strategy.
So instead of trying to find all of them, we'll work on how to find one by "moving" from a neighboring vertex that we already know.

Let's take a feasible region:

![](assets/chapter3/movingVertexFeasible.png)

Defined by these constraints:
$$
\begin{align*}
&\quad x_1+x_2 \leq 7\\
&\quad -x_1+x_2 \leq 3 \\
&\quad x_1-2x_2  \leq 1 \\
&\quad x_1,x_2 \geq 0
\end{align*}
$$

Put in standard form:

$$
\begin{align*}
&\quad x_1+x_2+S_1 = 7 \quad (1)\\  
&\quad -x_1+x_2+S_2 = 3 \quad (2)\\
&\quad x_1-2x_2+S_3  = 1 \quad (3)\\
&\quad x_1,x_2,S_1,S_2,S_3 \geq 0
\end{align*}
$$

Our matrix A is :
$$
A= \begin{bmatrix}
 1 & 1 & 1 & 0 & 0  \\
 -1 & 1 & 0 & 1 &0  \\
1 & -2 & 0 & 0 & 1
\end{bmatrix}
$$

we have m = 3 and n = 5 so we have 2 degrees of freedom.

Let's pick a starting point, we choose the vertex "A" since we just need to take out the first two columns of the matrix to find it
(remember that since we are talking about vertexes the variable that are non-basic are set to 0 , and since all of Ss variables are basic none of them 0 meaning no constraints is active).

Our matrix is partitioned this way:

$$
B= \begin{bmatrix}
 1 & 0 & 0  \\
 0 & 1 &0  \\
 0 & 0 & 1
\end{bmatrix}\quad
N=\begin{bmatrix}
1 & 1   \\
-1 & 1   \\
1 & -2
\end{bmatrix}
$$

Now lets say that we want to move from vertex A to vertex B :

![](assets/chapter3/movingAB.png)

How should we change our basis to achieve the goal?

We must activate the constraints whose interception defines the wanted vertex, in our case the constraint (3), so we activate it by dropping the column $[0,\:0,\:1]^T$ from B and adding $[1,\:-1,\:1]^T$ (we need $x_2$ also to be 0 so we keep it non-basic by not choosing the other column).

Our partition is now:

$$
B= \begin{bmatrix}
1 &1 & 0   \\
-1& 0 & 1  \\
1 &0 & 0 
\end{bmatrix}\quad
N=\begin{bmatrix}
 1  & 0 \\
 1  & 0 \\
 -2 & 1
\end{bmatrix}
$$

And we moved to vertex B.

What if we wanted to move from B to C?

![](assets/chapter3/movingVertexBC.png)

We need to activate constraint (1) since C is the meeting point of (1) and (3) so we kick $S_1$ out :

$$
B= \begin{bmatrix}
1 &1 & 0   \\
-1& 1 & 1  \\
1 &-2 & 0
\end{bmatrix}\quad
N=\begin{bmatrix}
1  & 0 \\
0  & 0 \\
0& 1
\end{bmatrix}
$$

We are now in vertex C.

Now that we saw how to move to neighboring vertex , we need to find a good way to determine which vertex it would be better to move to.

### Reduced costs
We have an LP in standard form:
$$
\begin{align*}
min\quad & z=\underline{c}^T\underline{x}\\
& A\underline{x} = \underline{b}
\end{align*}
$$
Let's now expand the objective function by making explicit the basic and non-basic parts:
$$
z = [\:\underline{c}_B^T \quad \underline{c}_N^T\:] \begin{bmatrix}
\underline{x}_B \\
\underline{x}_N
\end{bmatrix} \quad(1)
$$

We previously saw how to express basic variables in terms of non-basic ones , like so (with B and N being partitions of A):
$$
\underline{x}_B=B^{-1}b-B^{-1}\underline{x}_N \quad(2)
$$

So we can rewrite (1) using (2) as:

$$
z=[\:\underline{c}_B^T \quad \underline{c}_N^T\:] \begin{bmatrix}
B^{-1}b-B^{-1}\underline{x}_N \\
\underline{x}_N
\end{bmatrix} \quad
$$

That translates into:
$$
z = \underline{c}_B^T B^{-1} b-c_B^TB^{-1} N \underline{x}_N+c_N^T\underline{x}_N
$$

Let's group by $\underline{x}_N$:
$$
z = \underbrace{\underline{c}_B^T B^{-1} b}_{V}+\underline{x}_N(\underbrace{c_N^T-c_B^TB^{-1}N}_R)
$$

The first part of the formula (V) is the value of the objective function when we are in a basic feasible solution (vertex) since $\underline{x}_N$ is $\underline{0}$.

THe second part of the formula (R) are the reduced costs(of then non-basic variables), but before giving you the definition let's try to understand what are the reduced cost.

### Definition walkthrough

Let's start from our formula:

$$
\underline{\overline{c}}_N^T = c_N^T-c_B^TB^{-1}N
$$

The $c_N^T$ part doesn't tell us much ,it just is the contribution of the non-basic variables in the objective function.
The $c_B^TB^{-1}N$ part on the other hand is where the things start to get interesting.

What is $B^{-1}N$?

Let's suppose that we have the following constraint:
$$
x_1 +4 x_2 + 2 x_3 + x_4 = 24
$$

And we have these assignments: 
$$
\underline{x}^T = [\:1\quad 2\quad 6 \quad3\:]
$$

>If we decided to change the value of one of the variables, how the others should adjust themselves in order to still satisfy the constraint?

This is exactly what $B^{-1}N$ tells us.

Let's crunch some numbers:

We have this LP:

$$
\begin{align*}
min\quad &x_1+2 x_2+3x_3+x_4\\
&x_1+2x_2+x_3 = 5\\
&x_2+x_3+x_4 = 3\\
\end{align*}
$$
We decide to start from a vertex by choosing $x_1$ and $x_4$ as basic variables, so we partition the matrix this way:
$$
B= \begin{bmatrix}
1 & 0   \\
0&  1  
\end{bmatrix}\quad
N=\begin{bmatrix}
2  & 1 \\
1  & 1 
\end{bmatrix}
$$

Our current assignment is:
$$
\underline{x}^T=[\:5\quad0\quad 0 \quad3\:]
$$
or

$$
\underline{x}_B^T=[\:5\quad3\:]\quad \quad \underline{x}_N^T=[\: 0 \quad 0 \:]
$$

The objective value is:
$$
5 + 0 + 0 + 3 = 8
$$
What if we increased $x_2$ by 1?

Let's compute $B^{-1}N$:
$$
B^{-1}N=\begin{bmatrix}
2  & 1 \\
1  & 1
\end{bmatrix}
$$
This tells us that by increasing $x_2$ by 1 the value of $x_1$ must change by a factor of 3 and $x_4$ by a factor of 1 (the first column), lets see it algebraically:

We know that:

$$
\underline{x}_B=B^{-1}b-B^{-1}\underline{x}_N
$$

>Which holds regardless whether we are in a vertex or not, we are just expressing two variables in function of the other two.

So let's compute the new values of $x_1$ and $x_4$:

$$
\underline{x}_B=\begin{bmatrix} 5\\ 3\end{bmatrix}-\begin{bmatrix}
2  & 1 \\
1  & 1
\end{bmatrix}\begin{bmatrix} 1\\ 0\end{bmatrix} = \begin{bmatrix} 3\\ 2\end{bmatrix}
$$
Where $[1\quad 0]^T$ is the new assignment of $x_2$.

The constraints hold:

$$
\begin{align*}
& 3 +2\times 1+0 = 5\\
&1+0+2 = 3\\
\end{align*}
$$

The new value of the objective is:

$$
3+2\times1+0+2 = 7
$$

Let's use the formula given by our previous definition:

$$
\underline{\overline{c}}_N^T = c_N^T-c_B^TB^{-1}N
$$

Substituting:
$$
\begin{align*}
\underline{\overline{c}}_N^T &= \begin{bmatrix} 2\\ 3\end{bmatrix} - [\:1\quad1\:]\begin{bmatrix}
2  & 1 \\
1  & 1
\end{bmatrix} \\
& =\begin{bmatrix} 2\\ 3\end{bmatrix} - \begin{bmatrix} 3\\ 2\end{bmatrix} \\
& =\begin{bmatrix} -1 \\ 1\end{bmatrix}
\end{align*}\\
$$
This tells us that value of the objective changes by -1 which is exactly what we got from our calculations (the other value is the change that we would get by increasing $x_3$ by one)

So let's now give the definition of reduced costs:

:::{.callout .callout-definition title="Active constraint"}
The reduced costs represents the change in the objective function value if non-basic $x_j$ would be increased from 0 to 1 while keeping all other non-basic variables to 0.
(we specified the variables as non-basic because we express the basis one in function of the non-basic one, but this potentially hold for any partitioning)

The solution value changes by $\Delta z= \overline{c}_j \Delta x_j$ ( in our previous example $\Delta x_j$ was 1)
:::

### Optimal solution













