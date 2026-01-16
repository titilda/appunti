---
title: "Formal Languages and Compilers"
author:
  - "Andrea Lunghi"
---

## Language

A language is a set of strings over a given alphabet ($\Sigma$). An alphabet is a finite set of symbols. For example, the binary alphabet is {0, 1}, and a language over this alphabet could be the set of all strings that contain an even number of 0s.

Languages can be classified based on their complexity and the type of rules used to generate them. The Chomsky hierarchy classifies languages into five types:

1. **Type 0 (Recursively Enumerable Languages)**: These are the most general languages and can be recognized by a Turing machine. They have no restrictions on their production rules.
2. **Type 1 (Context-Sensitive Languages)**: These languages can be recognized by a linear-bounded automaton. Their production rules are context-sensitive, meaning that the rules can depend on the surrounding symbols.
3. **Type 2 (Context-Free Languages)**: These languages can be recognized by a pushdown automaton. Their production rules are context-free, meaning that the left-hand side of each production rule consists of a single non-terminal symbol.
4. **Type 3 (Regular Languages)**: These languages are the simplest one and can be recognized by a finite automaton. Their production rules are regular, meaning that they can be expressed using regular expressions.
5. **Type 4 (Finite Languages)**: These are languages that contain a finite number of strings.

$$ \text{finite} \subset \text{regular} \subset \text{context-free} \subset \text{context-sensitive} \subset \text{recursively enumerable} $$

### Regular Expressions (RegEx)

Regular expressions are a formal way to describe regular languages. They use a combination of symbols and operators to define patterns in strings. Some common operators include:

- **Concatenation**: If `A` and `B` are regular expressions, then `AB` is a regular expression that matches any string formed by concatenating a string from `A` with a string from `B`.
- **Union**: If `A` and `B` are regular expressions, then `A|B` is a regular expression that matches any string that matches either `A` or `B`.
- **Kleene Star**: If `A` is a regular expression, then `A*` is a regular expression that matches any string formed by concatenating zero or more strings from `A`.
- **Plus**: If `A` is a regular expression, then `A+` is a regular expression that matches any string formed by concatenating one or more strings from `A`.
- **Optional**: If `A` is a regular expression, then `A?` is a regular expression that matches either the empty string or any string from `A`.
- **Character Classes**: Square brackets `[]` are used to define a set of characters. For example, `[abc]` matches any single character that is either `a`, `b`, or `c`.
- **Repetition**: Curly braces `{}` are used to specify the exact number of repetitions. For example, `a{3}` matches exactly three consecutive `a` characters.
- **Ordered Intervals**: Are used to define a range of characters. For example, `[a-z]` matches any lowercase letter from `a` to `z`.

#### Derivation

Starting from the regular expression, we can derive strings by applying the rules defined by the operators. For example, given the regular expression `a(b|c)*`, we can derive strings like `a`, `ab`, `ac`, `abb`, `abc`, `acb`, `acc`, and so on.

The same regular expression can derive the same string in multiple ways. In this case the RE is **ambiguous**.

To prove ambiguity all the characters are enumerated and by performing the derivation is possible to find the same string in different ways.

$$ (a|b)^*a(a|b)^* \to (a_1|b_2)^*a_3(a_4|b_5)^* $$
$$ (a_1|b_2)^*a_3(a_4|b_5)^* \to (a_1|b_2)^1a_3(a_4|b_5)^0 \to (a_1|b_2)a_3 \to a_1a_3 $$
$$ (a_1|b_2)^*a_3(a_4|b_5)^* \to (a_1|b_2)^0a_3(a_4|b_5)^1 \to a_3(a_4|b_5) \to a_3a_4 $$
