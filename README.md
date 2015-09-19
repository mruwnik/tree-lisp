# Tree (for lack of a better name)

This program simulate a tree's growth. The goal is for it to be as life like as possible. There are a load of various 'biological' settings that can be modified to change the trees appearance and growth. When a tree is created, it recieves its settings in an instance of the `dna` class. That contains information about e.g. how tall the tree can grow, how often it makes buds while growing, how much sugars it produces plus lots more, just like normal DNA does

Apart from its genetic makeup, trees (and all other living creatures) are effected by their enviroment. Obviously, that also has to be taken into account. Currently the following nutrients and hormones are sent through the tree in order to regulate it:

* water
* minerals (in a general sense - there is no breakdown into specific ones)
* sugars
* auxin
* a general 'growth hormone'
* abscisic acid

The following enviromental factors are also taken into account:

* sun availability
* wind (currently only for display, as random leaf movements)
* temperature (any temperature lower than a given value causes the tree's leaves to fall and stops its growth)

All of the above are provided as relative percentage values (between 0 and 1)


# How to run this program

I do it by loading it in lisp via `(ql:quickload "tree")`, which will load the whole project and display the beginning sprout of a tree. There is no automatic growth - it has to be done manually. At the end of `tree.lisp` is a loop which runs through 100 rounds of growth - that is the easiest way to get it going.
  To simulate winter, set the temperature to a low value via `set-temp`, e.g. `(set-temp -12)`. Run through a couple of days so that all the leaves fall of the tree, and then set the temperature back up to something which the tree can grow in (like 20 - for specific values, see the `dna` class).

# Display controls

## Directions:

* use the mouse to rotate the view
* 'w' - move up along the z-axis (forward)
* 's' - move down along the z-axis (back)
* 'd' - move up along the x-axis (right)
* 'a' - move down along the x-axis (left)
* 'r' - move up along the y-axis (up)
* 'e' - move down along the y-axis (down)

The controls are pretty naff, but at least its possible to look around...

## Display options

* '1' - simulate wind
* '2' - draw position - this is for debugging only - it's a different way of calculating each part's position
* '3' - hide the tree
* '4' - show how much sunlight each leaf gets. This is colour coded according to the rainbow - the more in the direction of red, the more it gets. White is the most, black means that it's totally in shadows
* 'f' - fullscreen