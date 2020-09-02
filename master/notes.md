## TODOs:
Lage feature model evolution plans i haskell

Deal with subtrees and add/remove move operations. The remove move operation is especially tricky. This is wrinkling my brain

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

#### Move feature TODO: deal with subtrees

If type is mandatory, check that new parent group has type AND until it is moved again/removed or ∞. Check that parent group is not removed during new parent group validity. If previous parent group has type OR/XOR, check that it still has enough children (during the new validity?).

Check that the feature is not one of the new parent group's ancestors. Also check more stuff and find out which validities are relevant.

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

Ancestor must not be moved to its own subtree in the future. / for any move that is done for an ancestor of the same subtree, make sure that the target does not have the ancestor as one of its ancestors

## Useful mappings

Names -> validities

"Car" -> [(0, 2), (4, ∞)] [Valid 0 2, From 4]
data Validity = Valid Int Int | From Int
FeatureID -> validities of feature, parent group, ftype, name, child group IDs
GroupID -> validities of group, parent feature, gtype, child feature IDs
