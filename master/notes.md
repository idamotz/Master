## TODOs:
Lage feature model evolution plans i haskell

Done: Deal with subtrees and add/remove move operations. The remove move operation is especially tricky. 

  - TODO: Formalize steps for REMOVE move operation. 
  - TODO: With current representation (maps of validities), the order of operations is lost. What needs to be checked minimally? What follows from the soundness assumption here?
  - TODO: Look at input: Initially, will focus only on input as an initial plan + one meta operation. Eventually look at several changes to a plan (compound changes). If one operation causes a paradox, does another (in the same change) fix it? In that case, the plan changes should ideally be accepted
  - TODO: Convert an initial plan to maps of validities
  - TODO: What do I need from an example/use case? 
  - TODO: Presentation (use essay)
    - Goal/Challenge
    - How to get there
    - Starting point (concretely)
    - What use cases/examples do I need from Germany to be able to validate

## What to check on ADD operation to plan

### Add feature

Need to check from now till end:

- Parent group not deleted
- If ftype == mandatory, parent group type never OR/XOR
- Name unique
Add validity (now, ∞) for name, parent group, type etc. Also add in parent

### For any feature property change (type, parent group, name)

Only need to check until next change of same category or feature removal, since we already know it's good after; always working under the assumption that the initial plan is sound. (analogous to reaching definitions in static analysis). Add validity from now until next validity starts for this property.

#### Change feature type
If changed to optional, no need to check anything. If changed to mandatory, check (until next type validity starts) that no parent group has type OR/XOR

#### Move feature
  
If type is mandatory, check that new parent group has type AND until it is moved again/removed or ∞. Check that parent group is not removed during new parent group validity. If previous parent group has type OR/XOR, check that it still has enough children (during the new validity?).

Subtree paradoxes:
Check that the feature is not an ancestor of the target. This only needs to be checked for this time point. 

The feature to be moved: f1  
The target group: g1  
Ancestors of the target: a

Check that no new ancestor of f1 is moved to a subfeature of f1 in the future. This until f1's next move or forever, since we already know this is sound (soundness assumption for original plan). 

Maybe not all ancestors need to be checked. We already know that there are no moves from the old ancestors of f1 to a subgroup of f1. Thus we only need to check the *new* ancestors. Let t1 be the smallest subtree containing both f1 and g1. The ancestors that need to be checked are the ancestors of f1/g1 in t1.
What if one of these ancestors is moved? If they are moved to the subtree of f1 while they are still f1's ancestors, then there is a paradox. If one of them is moved to a different subtree, then new ancestors need to be considered as well. Basically we need a way to identify all problematic ancestors of f1, and see if they are in f1's subtree at any point. This seems pretty complex and may increase worst case a lot:(

#### Change feature name

Check that no other feature has this name during new validity

### Remove feature

- Feature must have no children right now and no children added later
- Remove future validities from bookeeping tables, and from that of parent group.
- If parent group is ever or/xor, make sure that there are more than 2 children whenever this feature is a child (the parent group must have more than 1 child left after this one is removed)

### Add group

- Parent feature never deleted -> ask user whether this group should also be deleted at that time
- Type must be AND or there must exist plans to add at least 2 children with type optional at same time point.
Add validity (now, ∞) for group, parent feature, type etc. Also add in parent feature

### Change group type

- If changing from OR/XOR to AND, automatically good.
- If changing from AND to OR/XOR, check that there are at least 2 child features at all times, all with type optional.
Add validity from now until next validity starts or ∞.

### Move group TODO: deal with subtrees

- Check that new parent is not removed until group is moved again (or deleted). Otherwise fine

Add validity from now until next validity/removal or ∞

### Remove group

- Check that no children are added/moved here in the future/exist now.

Remove future validities


## What to check on REMOVE operation from plan

### Remove `Add feature`

Check that feature never has child groups in the future. If parent group has type OR/XOR (at any point in the future), check that it has >= 2 children left at all times.

Remove complete mapping from bookeeping tables.

### Remove `Change feature variation type`

Let old validity of type be (a, b)
New validity is previous type validity start until next type validity starts. If previous type was optional, no need to check anything. If previous type was mandatory, check that in the interval (a, b), parent group type is AND.

Replace previous validity (c, a) by (c, b)

### Remove `Change feature name`

Let old validity of name be (a, b), and the previous validity is (c, a). Let the previous name be N. Check that there exists no other mapping N -> (d, e) with (d, e) overlapping (a, b). 

Replace previous validity (c, a) by (c, b)

### Remove `Move feature` TODO: deal with subtrees

If (now, b) is the old validity of parent group:

Well-formedness:
If new parent group has type OR/XOR: must have >= 2 children during (now, b).
Parent group from previous validity (p, now) must have type AND if feature has type mandatory at any point during (now, b).

Old parent group must not be removed during (now, b).

New validity replacing (p, now): (p, b). also remove (now, b)

Remove `Move feature` has an equal sequence of operations: ADD `Move feature` from b to a right after the first `Move feature` from a to b operation. Composing with inverse so we get the identity function. What we then need to check is whether adding `Move feature` from b to a creates problems: Use the algo described in [AddMoveAlgo](./addMoveAlgo.pdf).

## Useful mappings

Names -> validities

"Car" -> [(0, 2), (4, ∞)] [(TP 0, TP 2), (TP 4, Forever)]

data Validity = Valid Int Int | From Int

FeatureID -> validities of feature, parent group, ftype, name, child group IDs

GroupID -> validities of group, parent feature, gtype, child feature IDs

