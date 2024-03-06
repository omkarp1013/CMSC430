# Pros

Pros of the new encoded are as follows: 1): `#f` and `#t` are more intuitively encoded, as 0 and 1 represent True and False in propositional logic. 2): to add or subtract 1, we don't need to invoke `value->bits` and can add or subtract 1 in *most* cases (this is also a con, as explained below). 3): negative integers are represented by themselves; there is nothing special done to represent them. 4): You can store a wider range of integers, as this encoding allows us to use 64-bits (as opposed to the encoding provided in class, which uses 63-bits).

# Cons

Cons of the new encoded are as follows: 1) We need to account for adding 1 to -1 and subtracting 1 from 0 (represented as 2), which results in longer (and perhaps slightly messier) code than when using the representation presented in class, in which we simply called `value->bits` when adding or subtracting 1 in all cases. 2): We had to compare rax with 2 in the `zero?` primitive as opposed to calling `value->bits 0`, which is rather counterintuitive. 3): Adding 2 to nonnegative integers may seem arbitrary at the bitwise level; the representation presented in class focused on using bit shifting to represent integers.
