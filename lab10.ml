(*
                             CS51 Lab 10
                Time Complexity, Big-O, and Recurrence
 *)

(* Objective:

This lab is intended to introduce you to concepts concerning
efficiency and complexity, including:
    Big O notation
    Recurrence equations
 *)

(*======================================================================
Part 1: Empirical analysis of functions

In the reading, we empirically determined the efficiency of mergesort and 
insertion sort by timing these functions on the same inputs of various
lengths. The ability to perform empirical analysis of programs will
often prove useful. 

Throughout this lab you may find various functions in the CS51 module
to be useful, as well as OCaml's Random library module
(https://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html).

......................................................................
Exercise 1: Write a function, random_list, that creates a list of a
specified length of random integers between 0 and 999.
....................................................................*)

let random_list (length : int) : int list =
   List.init length (fun _ -> Random.int 1000);;

(*....................................................................
Exercise 2: Write a function, time_sort, that, given an int list ->
int list sorting function and a list of integers, times how long the
sort takes.
....................................................................*)

let time_sort (sort : int list -> int list) (lst : int list) : float =
   let _, time = (CS51.call_timed sort lst) in time;;

(* We've provided implementations of merge sort and insertion sort
here as modules satisfying the SORT signature so that you have some
things to time. *)
   
module type SORT =
  sig
    (* sort lt xs -- Return the list xs sorted in increasing
       order by the "less than" function lt. *)
    val sort : ('a -> 'a -> bool) -> 'a list -> 'a list
  end ;;

module InsertSort : SORT =
  struct
    let rec insert (lt : 'a -> 'a -> bool)
                   (xs : 'a list)
                   (x : 'a)
            : 'a list =
      match xs with
      | [] -> [x]
      | hd :: tl -> if lt x hd then x :: xs
                    else hd :: (insert lt tl x) ;;
      
    let rec sort (lt : 'a -> 'a -> bool)
                 (xs : 'a list)
            : 'list =
      match xs with
      | [] -> []
      | hd :: tl -> insert lt (sort lt tl) hd ;;
  end ;;

module MergeSort : SORT =
  struct
    let split (lst : 'a list) : 'a list * 'a list =
      let rec split' (lst : 'a list)
                     (half1 : 'a list)
                     (half2 : 'a list)
              : 'a list * 'a list =
        match lst with
        | h1 :: h2 :: tl -> split' tl (h1 :: half1) (h2 :: half2)
        | [hd] -> (hd :: half1, half2)
        | [] -> (half1, half2) in
      split' lst [] [];;
      
    let rec merge (lt : 'a -> 'a -> bool)
                  (xs : 'a list)
                  (ys : 'a list)
            : 'a list =
      match xs, ys with
      | h1 :: t1, h2 :: t2 -> if lt h1 h2 then h1 :: merge lt t1 ys
                              else h2 :: merge lt xs t2
      | [], _ -> ys
      | _, [] -> xs ;;
      
    let rec sort (lt : 'a -> 'a -> bool)
                 (xs : 'a list)
            : 'a list =
      match xs with
      | [] -> []
      | [_] -> xs
      | _ -> let first, second = split xs in
             merge lt (sort lt first) (sort lt second)
  end ;;

(*....................................................................
Exercise 3: List the functions provided by the InsertionSort
module. List the functions provided by the MergeSort module.

insertion : insert, sort 

merge : split merge sort
....................................................................*)

  
(*....................................................................
Exercise 4: Compare the time it takes for merge sort and insertion
sort to run on lists of random ints of length 10 and 1000. We've
included an implementation of merge and insertion sort below.
....................................................................*)
  

(*......................................................................
Fill in the table below:

                |    List length 10    |  List length 1000
                |    Time (seconds)    |  Time (seconds)
------------------------------------------------------------
Insertion Sort  |     .00000405        |   .00099639
------------------------------------------------------------
Merge Sort      |     .00000811        |   .01245189
------------------------------------------------------------

......................................................................*)


(*======================================================================
Part 2: Big-O

........................................................................
Exercise 5: In the reading for this lab, we saw that Big-O notation is
a generic way of expressing the growth rate of a function. For each of
the functions defined below, state in which big-O class(es) the
function belongs.

To allow us to check answers, we've defined an OCaml data type,
complexity, with various commonly used big-O classes. The name to
function mapping is set out below. We will use informal function
notation in this lab. For more on informal vs. formal notation, please
refer to the reading for today's lab.

Constant    -> O(1)
Logarithmic -> O(log(n))
Linear      -> O(n)
LogLinear   -> O(n log(n))
Quadratic   -> O(n^2)
Cubic       -> O(n^3)
Exponential -> O(2^n)

Because functions can be in more than one complexity class, the format
of the solution to each exercise is a list of complexity classes.
By way of example, we've done the first problem for you:

(* f(x) = 5^x + x^3 *)
let exercise5a () : complexity list = 
  [Exponential] ;;
......................................................................*)

type complexity =
    | Constant
    | Logarithmic
    | Linear
    | LogLinear
    | Quadratic
    | Cubic
    | Exponential ;;

(* f(x) = 5^x + x^3 *)
let exercise5a () : complexity list =
  [Exponential] ;;

(* f(x) = 0 *)
let exercise5b () : complexity list =
  [Constant; Logarithmic; Linear; LogLinear; Quadratic; Cubic; Exponential] ;;

(* f(x) = 3 x^2 + 2 x + 4 *)
let exercise5c () : complexity list=
  [Quadratic; Cubic; Exponential] ;;

(* f(x) = (2 x - 3) log(x) + 100 x *)
let exercise5d () : complexity list =
  [LogLinear; Quadratic; Cubic; Exponential] ;;

(* f(x) = x (x^2 + x) *)
let exercise5e () : complexity list =
  [Cubic; Exponential] ;;


(* One advantage of big-O is that we can disregard constants in
considering asymptotic performance of functions. We saw empirically
that on large inputs, merge sort worked faster than insertion
sort. The ability to disregard constants tells us that merge sort will
eventually be faster than insertion sort, even if we add a constant
amount of time to merge sort's performance. Let's do this and test the
results empirically. 

Here is a version of merge sort that inserts a small delay (.05
seconds), to simulate a version of the function with the same
asymptotic complexity but that is a constant amount slower. *)

module DelayMergeSort : SORT =
  struct
    (* DelayMergeSort first sleeps for a predetermined
       period of time, then runs our generic MergeSort sort.
       This sleep will add a constant amount of time to
       each run of DelayMergeSort.
     *)
    let sort (lt : 'a -> 'a -> bool)
             (xs : 'a list)
           : 'a list =
      let () = Unix.sleepf 0.05 in
      MergeSort.sort lt xs  ;;
  end ;;

(*....................................................................
Exercise 6: Additive constants

Fill in the below table.
....................................................................*)

(*               |    List length 10    |  List length 1000
                 |    Time (seconds)    |  Time (seconds)
------------------------------------------------------------
Insertion Sort   |      .00000405       |  .00959
------------------------------------------------------------
Delay Merge Sort |      .05124998       |  .05097
------------------------------------------------------------*)

(* You likely found that InsertSort was faster than DelayMergeSort,
even on a list of length 1000. Increase the length of the list being
sorted by DelayMergeSort and InsertSort until DelayMergeSort runs
faster than InsertSort. Record the size of a list for which this is
true below. *)
   
let exercise6 () = 5000 ;;

(* Big-O also allows us to disregard constant multiplicative factors. In
this exercise, we work with a version of MergeSort that sorts a given
list twice rather than once.  However long MergeSort takes,
DoubleMergeSort will thus take twice as long. *)

module DoubleMergeSort : SORT =
  struct
    (* By sorting the list twice, we double the time
       MergeSort takes *)
    let sort (lt : 'a -> 'a -> bool)
             (xs : 'a list)
           : 'a list =
      let _ = MergeSort.sort lt xs in
      MergeSort.sort lt xs  ;;
  end ;;
  
(*....................................................................
Exercise 7: Multiplicative constant factors

Complete the same empirical analysis as above to compare the
asymptotic behavior of InsertSort and DoubleMergeSort, and fill in the
table below.
....................................................................*)

(*                |    List length 10    |  List length 1000
                  |    Time (seconds)    |  Time (seconds)
------------------------------------------------------------
Insertion Sort    |    .0000031          |     .009611
------------------------------------------------------------
Double Merge Sort |    .0000079          |     .001593
------------------------------------------------------------*)

(* Now record a list length for which you found DoubleMergeSort
sorted faster than InsertSort. *)
   
let exercise7 () = 1000 ;;

(* An additional nice property of big-O is the ability to disregard
lower-order terms of a function. In the reading, we found that:

    Time_mergesort(n) = c * n logn + d

In this exercise, we will work with a version of MergeSort that will
add an additional k * n term to the completion time of MergeSort. *)

module ExtraTermMergeSort : SORT =
  struct
    let sort (lt : 'a -> 'a -> bool)
             (xs : 'a list)
           : 'a list =
      (* We map the identity function over all of the elements of the
         list and throw away the result, so as to waste time of O(n),
         where n is the number of elements in the list *)
      let _ = List.map (fun x -> x) xs in
      MergeSort.sort lt xs;;
  end ;;

(*......................................................................
Exercise 8: Lower Order Terms

Complete the same empirical anaysis as above to compare the asymptotic
behavior of InsertSort and ExtraTermMergeSort, and fill in the table
below.
....................................................................*)

(*                    |    List length 10    |  List length 1000
                      |    Time (seconds)    |  Time (seconds)
-----------------------------------------------------------------
Insertion Sort        |    .00000186         |  .011332
-----------------------------------------------------------------
Extra Term Merge Sort |    .00001001         |  .000770
-----------------------------------------------------------------*)

(* Now record a list length for which ExtraTermMergeSort works faster
    than InsertSort. *)
let exercise8 () = 1000 ;; 

(*......................................................................
Exercise 9: More big-O

As in Exercise 4, for each of the functions below, state to which
Big-O classes the function belongs. See exercise 4 for an example.

The Big O classes defined in our complexity ADT:
type complexity =
    | Constant
    | Logarithmic
    | Linear
    | LogLinear
    | Quadratic
    | Cubic
    | Exponential ;;
......................................................................*)

(* f(x) = 10000 *)
let exercise9a () : complexity list =
  [Constant; Logarithmic; Linear; LogLinear;
   Quadratic; Cubic; Exponential] ;;
  
(* f(x) = 50x^100 + x^2 *)
let exercise9b () : complexity list =
  [Exponential] ;;

(* f(x) = 30xlog(x) + 50x + 70 *)

let exercise9c () : complexity list =
  [LogLinear; Quadratic; Cubic; Exponential] ;;

(* f(x) = 30x^2 * log(x) *)
let exercise9d () : complexity list =
  [Cubic; Exponential] ;;

(* f(x) = x + 60log(x) *)
let exercise9e () : complexity list =
  [Linear; LogLinear; Quadratic; Cubic; Exponential] ;;

(*======================================================================
Part 3: Recurrence Equations

Once we know the complexity of a function, we can use big-O notation
to compare that function's asypmtotic performance with other functions.
However, a function's complexity may not be immediately obvious.
Recurrence equations provide an analytical way to determine the
complexity of a function.

Recurrence equations generally consider two cases:

1. A base case
2. A recursive case

Once you have formulated the recursive case, you can use the method
of "unfolding" described in the reading to determine the time
complexity of the functions.

In each of the exercises below, we present a function in ocaml. Your
task is to define the recurrence equations for that function, and then
to solve the recurrence equations via unfolding, to generate a
closed-form equation and form a conclusion about the time complexity
of the function in big-O notation.

To facilitate automated testing of the recurrence equations you come
up with, we ask you to present them in the form of an ocaml
function. (We provide an example below.)  We encourage you to first
complete the problem on paper, with the notation from the chapter and
using the unfolding method, and then transfer your solutions
here. When finding the time complexity, we would like you to use the
tightest possible big-O class.

Many of the recurrences have various constants. We have defined a
global variable, k, for you to use for *all* of the constants in your
ocaml formulation of the recurrence equations. Again, the example
below should clarify. *)

let k = 5;;

(* An example of the method we would like you to use for presenting
your recurrence equations and complexity is provided below, based on
the insert function from the reading. *)

(*......................................................................
                        RECURRENCE EXAMPLE
......................................................................*)
let rec insert xs x =
    match xs with
    | [] -> [x]
    | h :: t -> if x > h then h :: (insert t x)
                  else x :: xs ;;
(*......................................................................
Complete the recurrence equations and time complexity of this function:
......................................................................*)

let time_insert (n : int) : int =
    failwith "time_insert not yet implemented" ;;

let insert_complexity () : complexity =
    failwith "insert_complexity not yet implemented" ;;
(*......................................................................
                            SOLUTION

We saw in the reading that the insert function has the following
recurrence equations:

T_insert(0) = c
T_insert(n+1) = max(k_1 + T_insert(n), k_2) ≤ k_1 + T_insert(n) + k_2
              = k + T_insert(n)

We express these equations with the following ocaml function. Rather
than write two equations, as you might on paper, we write the base
case and recursive case as branches of an if/then/else
statement. Although the recurrence equations have two constants (c and
k), we use the ocaml variable k to play both of those roles.  Please
follow that format in all the following exercises.
......................................................................*)
  
let rec time_insert (n : int) : int =
    if n = 0 then k
    else k + time_insert (n - 1) ;;

(* Note that this function will run forever on inputs below 0. We would
normally expect you to handle invalid inputs. However, for the purpose
of practicing recurrence equations, you may assume inputs will be
positive. 

For the time complexity, we use a value from the complexity type to
express the tightest big-O bound for this linear function. *)

let insert_complexity () : complexity =
    Linear ;;

(* Now it's time for you to construct and solve some recurrence
equations, and add them to the lab using the method above.

                            END OF EXAMPLE
 *)
  
(*......................................................................
Exercise 10: Sum recurrence equations

Formulate the recurrence equations and determine the time
complexity of the sum function, defined below.
......................................................................*)

let rec sum (x : int list) : int =
  match x with
  | [] -> 0
  | h :: t -> h + sum t;;

(* Describe the time complexity recurrence equations for sum as an
ocaml function *)
let rec time_sum (n : int) : int =
  if n = 0 then k 
  else k + time_sum (n - 1) ;;

(* What is its complexity? *) 
let sum_complexity () : complexity =
  Linear ;;

(*......................................................................
Exercise 11: Divider Recurrence Equations

Please formulate the recurrence equations and determine the time
complexity of divider, defined below.
......................................................................*)

let rec divider (x : int) : int =
  if x < 0 then raise (Invalid_argument "only positive numbers accepted")
  else if x <= 1 then 0
  else 1 + divider (x / 2);;
  
let rec time_divider (n : int) : int =
  if n = 0 || n = 1 then k 
  else k + time_divider (n/2);;

let divider_complexity () : complexity =
  Logarithmic ;;

(*......................................................................
Exercise 12: Find_min recurrence equations

Formulate the recurrence equations and determine the time
complexity of find_min as defined below. Find_min contains a helper
function, split. First find the recurrence equations and
time complexity of split, as you may find this helpful in
determining the time complexity of find_min.
......................................................................*)

let rec find_min (xs : int list) : int =
  let rec split (xs : 'a list) =
    match xs with
    | [] -> ([], [])
    | [x] -> ([x], [])
    | h1 :: h2 :: tl ->
       let l1, l2 = split tl in
       (h1 :: l1, h2 :: l2) in
  match xs with
  | [] -> raise (Invalid_argument "Empty List")
  | [x] -> x
  | _ -> let l1, l2 = split xs in
         min (find_min l1) (find_min l2) ;;

let rec time_split (n : int) : int =
  if n = 0 || n = 1 then k
  else k + time_split (n - 2) ;;

let split_complexity () : complexity =
  Linear ;;

let rec time_find_min (n : int) : int =
  if n = 0 || n = 1 then k 
  else (time_split n) + 2 * time_find_min (n/2) ;;

let find_min_complexity () : complexity =
  LogLinear ;;


(*======================================================================
Part 4: Tradeoffs

Consider the three implementations of multiplication below:
 *)

let sign_product (x : int) (y : int) : int =
    let sign (x : int) : int =
        if x >= 0 then 1 else ~-1 in
    (sign x) * (sign y);;

(* Repeated addition simply adds x to itself y times. To multiply two
n-digit numbers together using this algorithm takes time O(n 10^n). It
is thus tremendously inefficient. *)

let mult_repeated_addition (x : int) (y : int) =
  let rec helper (y : int) (sum : int) =
    if y < 0 then raise (Invalid_argument "negative input")
    else if y = 0 then 0
    else helper (y - 1) (sum + (abs x)) in
  helper (abs y) 0 * (sign_product x y) ;;
  
(* The gradeschool multiplication algorithm is the algorithm one would
use when doing multiplication of large numbers on paper. To multiple
two n-digit numbers together using this algorithm takes time O(n^2) *)
  
let mult_grade_school (x : int) (y : int) =
  let product_sign = sign_product x y in
  let x = abs x in
  let y = abs y in
  let base = 10 in
  let single_digit_mult (x : int) (y : int) =
    let rec helper x carry placevalue =
      if x = 0 then carry * placevalue
      else let prod = y * (x mod base) + carry in
           let new_carry, place = (prod / base, prod mod base) in
           place * placevalue +
             helper  (x / base) new_carry (placevalue * base)
    in if y < 0 || y > 9 then raise (Invalid_argument "multiple digit or neg. y")
       else helper x 0 1 in
  let rec iterate_y y placevalue =
    if y = 0 then 0
    else placevalue * single_digit_mult x (y mod 10)
         + iterate_y (y / 10) (placevalue * base) in
  product_sign * iterate_y y 1 ;;

(* The Karatsuba algorithm is a multiplication algorithm that utilizes
a divide and conquer approach. It was divised in 1960 by 23-year-old
student Anatoly Karatsuba after his Professor Andrey Kolmogorov
conjectured that the fastest possible multiplication algorithm would
be lower bounded (i.e., no faster than) O(n^2).  Karatsuba's algorithm
disproved that conjecture. To multiply two n-digit numbers,
Karatsuba's algorithm runs in O(n^log_2(3)), that is, n to the power
of log base 2 of 3, which is about n^1.4.

*You do not need to understand the algorithm for this class*, but can
find out more on wikipedia if interested:
https://en.wikipedia.org/wiki/Karatsuba_algorithm *)
  
let mult_karatsuba (x : int) (y : int) : int =
  let tens_power (power : int) : int =
    let num_string = "1" ^ (String.make power '0') in
    int_of_string num_string in
  let rec karatsuba x y =
    if x < 10 || y < 10 then  x * y
    else let num_digits_x = String.length (string_of_int x) in
         let num_digits_y = String.length (string_of_int y)  in
         let min_digits = min num_digits_x num_digits_y in
         let half_place = tens_power (min_digits / 2) in
         let highx, lowx = (x / half_place, x mod half_place) in
         let highy, lowy = (y / half_place, y mod half_place) in
         let a = karatsuba lowx lowy in
         let b = karatsuba (lowx + highx) (lowy + highy) in
         let c = karatsuba highx highy in
         c * half_place * half_place
         + half_place * (b - c - a) + a in
  (sign_product x y) * karatsuba (abs x) (abs y) ;;

(*......................................................................
Exercise 13: Write a function, time_multiply, that, given a
multiplication function and two integers, times how long the algorithm
takes to multiply the integers.
......................................................................*)
  
let time_multiply (mult : int -> int -> int)
                  (x : int)
                  (y : int)
                : float =
  let _, time = CS51.call_timed (fun (x, y) -> mult x y) (x,y) in time ;;

(*......................................................................
Exercise 14: Fill in the table below:
....................................................................*)
(*......................................................................
                       |    15 * 50           |  1241342345 *
                       |                      |  3237461243
                       |    Time (seconds)    |  Time (seconds)
-----------------------------------------------------------------
Repeated Addition      |         .00000405    |   80.64042
-----------------------------------------------------------------
Grade School Algorithm |    .000003099        |  .000014067
-----------------------------------------------------------------
Karatsuba              |    .00000691414      |  .000021935
-----------------------------------------------------------------
OCaml Native ( * )     |    .0000009537       |    0.
-----------------------------------------------------------------
....................................................................*)
(*......................................................................
Questions to consider:
1) Which algorithm above was easiest to understand?
2) Which algorithm was likely easiest to code?
3) Which was fastest on small numbers?
4) Which was fastest on large numbers?
5) What size integers do you typically multiply?
6) Which algorithm, then, would you consider the best?
......................................................................*)

