(*
**
** An interesting example found at http://merd.sourceforge.net
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: November, 2010
**
*)

(* ****** ****** *)

staload "isacowananimal.sats"

(* ****** ****** *)

macdef eat = eat_animal_food

(* ****** ****** *)

implement
fprint_thing (out, X) = let
  val (fpf_name | name) = thing_get_name (X)
  val () = fprint_strptr (out, name)
  prval () = fpf_name (name)
  val () = fprint (out, " -> ")
  val () = fprint (out, thing_get_energy (X))
in
  // thing
end // end of [fprint_thing]

macdef print_thing (X) = fprint_thing (stdout_ref, ,(X))

(* ****** ****** *)

implement
main () = () where {
//
  val grass = new_grass (5)
  val carrot = new_carrot (10)
//
  val a_rabbit = new_rabbit (100)
  val a_cow = new_cow (1000)
  val a_human = new_human (300)
  val another_human = new_human (350)
//
  val things = list_vt_nil ()
  val a_human1 = thing_ref (a_human)
  val things = list_vt_cons (a_human1, things)
  val a_cow1 = thing_ref (a_cow)
  val things = list_vt_cons (a_cow1, things)
  val a_rabbit1 = thing_ref (a_rabbit)
  val things = list_vt_cons (a_rabbit1, things)
//
// things = a_rabbit :: a_cow :: a_human
//
  val () = loop (things) where {
    fun loop (Xs: List_vt(THING)): void =
      case+ Xs of
      | ~list_vt_cons
          (X, Xs) => let
          val () = print_thing (X)
          val () = print_newline ()
          val () = thing_unref (X)
        in
          loop (Xs)
        end // end of [list_vt_cons]
      | ~list_vt_nil () => () // end of [list_vt_nil]
    // end of [loop]
  } // end of [val]
//
  val () = eat (AFrabbit | a_rabbit, carrot)
  prval () = eaten_more (VEGEcarrot | carrot)
  val () = eat (AFcow | a_cow, grass)
  val () = eaten_over (grass) // no longer to eaten
//
  val (am | a_dead_rabbit) = slaughter (ANIMrabbit | a_rabbit)
  prval AMrabbit () = am
  val (am | a_beef) = slaughter (ANIMcow | a_cow)
  prval AMcow () = am
//
  val () = eat (AFhuman1 | a_human, carrot)
  prval () = eaten_more (VEGEcarrot | carrot)
  val () = eat (AFhuman1 | a_human, carrot)
  val () = eaten_over (carrot)
  val () = eat (AFhuman2 (MEATbeef) | a_human, a_beef)
  val () = eaten_over (a_beef)
  val () = eat (AFhuman2 (MEATdead_rabbit) | a_human, a_dead_rabbit)
  val () = eaten_over (a_dead_rabbit)
//
  val (AMhuman () | another_dead_human) = slaughter (ANIMhuman | another_human)
  val () = eat (AFhuman2 (MEATdead_human) | a_human, another_dead_human)
  val () = eaten_over (another_dead_human)
//
  val theEnergy = thing_get_energy(a_human)
  val () = thing_unref (a_human)
//
  val () = (print "theEnergy = "; print theEnergy; print_newline ())
//
  val () = assert (theEnergy = 1785)
//
} // end of [main]

(* ****** ****** *)

(* end of [isacowananimal.dats] *)
