# AUR Search Behaviour

Aura uses its own metadata server called [the Faur][faur] ("Fast AUR") to power
`-Ai`, `-As`, and `-Av`. The behaviour of `-As` in particular is slightly
different from usual searches on the AUR website as well as what other alternate
package managers provide, but the result is a vast improvement in performance,
simplicity of design, and ease of maintenance.

Below are the details of the performance techniques, as well as how search
results are affected.

## Term Indices

The Faur does not use a database, instead keeping all package data in memory. In
order to enable efficient lookups based on arbitrary search terms, it constructs
"term indices" whose keys are single terms found in a package's name,
description, and keywords. These keys then refer back to the set of package
names where they were found.

For instance, the key for `nintendo` points to a set of 168 package names
somehow related to Nintendo. With those names in hand, it's trivial to retrieve
their full package data via another `name -> data` index.

## "And" Semantics

If instead you had run `aura -As nintendo switch`, two index calls would be made
and their results combined via Set Intersection. So, you'd only have results
that contained both terms. This actually makes the overall search faster,
because there are fewer subsequent lookups to perform, and less JSON to render.

In general, this architecture enables `O(k * logn)` performance for lookups, where
`k` is the number of search terms. In layman's terms, "real fast".

## Result Sorting

Before returning the JSON, results are internally sorted by vote and then
truncated to the top 50 results. This may punish some searches in some cases,
but for the majority of user searches, this would result in what the user
expected. Most searches don't even need all 50 results; you mostly care about
the top entries anyway.

## Input Sanitization

Were a package originally called `great-fun-package`, its name would be split on
`-` and registered to all three keys of `great`, `fun`, and `package`. So a
search of any of those terms would find it.

Likewise, if you had run `aura -As great-fun`, that input would be split as well
and sent as `great` and `fun` separately, triggering the And Semantics described
above.

## No Regex or Partial Terms

However, a search for `pack` would not have found `great-fun-package`. As seen
above, only full terms can match a key in the indices. This is the main
difference between the Faur and the AUR.

> Why can't you just compare the search terms against all the keys?

Because that would result in `O(k * n)` performance, or in layman's terms, "real
slow". I'd have to change the name to Saur.

The moral of the story is: search with full terms and use "And Semantics" to get
better results.

[faur]: https://github.com/fosskers/faur
