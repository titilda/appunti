---
title: "Foundation of Operational Research"
author:
- "Ortore Joele Andrea"
---

## Linear Programming 
A linear programming problem is an optimization problem where:
- **The objective function** $\mathscr{F} : X \to R $  is linear , where X is the feasible region.
- **The feasible region**  has linear constraints.

> A solution $\underline{x}^* \in R^n  $ is said to be **optimal** if  $ f(\underline{x}^*) $ beats $f(\underline{x}), \forall \underline{x} \in X $.

### General Assumptions
1. Consequences of the linearity:
   -  **Proportionality**: Contribution of each variable $=$ constant $\times$ variable , we often refer to these constants as parameters.
   -  **Additivity**: Contribution of all variable $=$ sum of single contributions : $f(x + y + z) = f(x) + f(y) + f(z)$
2. In these types of problems variables can take any rational values.
3. Parameters are constants.

### Example:
 Suppose that we want to maximize how much we gain from the selling of 5 products ( suppose that we sell and produce the products in grams) , the price/g of each one of them are respectively: 2, 3, 4, 5, 6; the cost of production of each one of them is: 3, 6, 7, 9, 10; no more than 400g must be produced and the total production cost must be lower than 3000.
We shall model this problem like this:

<div style="display:flex; justify-content:space-between; width:100%;">

<div style="flex:1; padding-right:10px;">
<h3>Traditional modelling</h3>

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
<h3>Using python's MIP library:</h3>

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
