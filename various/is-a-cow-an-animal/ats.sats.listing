(*
**
** An interesting example found at http://merd.sourceforge.net
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: October, 2010
**
*)

//
// HX: note that this is just a specification
//

(* ****** ****** *)
%{#
#include "isacowananimal.cats"
%}
(* ****** ****** *)
//
datasort thing =
| grass
| carrot
//
| rabbit
| cow
| human
//
| dead_rabbit
| beef
| dead_human
//
(* ****** ****** *)

dataprop VEGE (thing) =
  | VEGEgrass (grass) | VEGEcarrot (carrot)
// end of [VEGE]

(* ****** ****** *)

dataprop ANIM (thing) =
  | ANIMcow (cow)
  | ANIMrabbit (rabbit)
  | ANIMhuman (human)
// end of [ANIM]

(* ****** ****** *)

dataprop MEAT (thing) =
  | MEATbeef (beef)
  | MEATdead_rabbit (dead_rabbit)
  | MEATdead_human (dead_human)
// end of [MEAT]

(* ****** ****** *)

dataprop AF ( // Animal/Food relation
  thing(*animal*), thing(*food*)
) =
  | AFcow (cow, grass)
  | AFrabbit (rabbit, carrot)
  | AFhuman1 (human, carrot)
  | {x:thing} AFhuman2 (human, x) of MEAT (x) // human eats any meat
// end of [AF]

(* ****** ****** *)

dataprop AM ( // Animal/Meat relation
  thing(*animal*), thing(*meat*)
) =
  | AMcow (cow, beef)
  | AMrabbit (rabbit, dead_rabbit)
  | AMhuman (human, dead_human)
// end of [AM]

(* ****** ****** *)

absviewtype THING (thing)
viewtypedef THING = [x:thing] THING (x)
//
// HX: everything has some energy
//
fun thing_get_name {x:thing}
  (X: !THING (x)): [l:agz] (strptr(l) -<lin,prf> void | strptr(l))
  = "thing_get_name"
// end of [thing_get_name]

fun thing_get_energy {x:thing}
  (X: !THING (x)): int = "thing_get_energy"
// end of [thing_get_energy]

fun fprint_thing {x:thing}
  (out: FILEref, X: !THING (x)): void = "fprint_thing"

fun thing_ref {x:thing}
  (X: !THING (x)): THING (x) = "thing_ref"
fun thing_unref
  {x:thing} (X: THING (x)): void = "thing_unref"

(* ****** ****** *)
//
// HX: slaughering an animal turns it into some meat
//
castfn slaughter {x:thing} // HX: a casting function for now
  (pf: ANIM (x) | X: THING(x)): [y:thing] (AM (x, y) | THING y) = "slaughter"
// end of [slaughter]

(* ****** ****** *)

absviewtype EATEN (thing)

prfun
eaten_more {x:thing}
  (pf: VEGE (x) | X: !EATEN (x) >> THING (x)): void // vegetable can be eaten repeatedly
fun eaten_over {x:thing}
  (X: EATEN (x)): void = "thing_unref" // thing can only be eaten once

(* ****** ****** *)

fun eat_animal_food {x1,x2:thing} (
  pf: AF (x1, x2) | A: !THING (x1), F: !THING (x2) >> EATEN(x2)
) : void = "eat_animal_food" // end of [eat_animal_food]

(* ****** ****** *)
//
fun new_grass (energy: int): THING (grass) = "new_grass"
fun new_carrot (energy: int): THING (carrot) = "new_carrot"
//
fun new_rabbit
  (energy: int): THING (rabbit) = "new_rabbit"
// end of [new_rabbit]
fun new_cow (energy: int): THING (cow) = "new_cow"
fun new_human (energy: int): THING (human) = "new_human"
//
(* ****** ****** *)

(* end of [isacowananimal.sats] *)
