let numbers = [2, 4, 3, 8, 7, 1, 5];
let bools = [true, false, true, false];

let rec say = (l : list(int)) => {
  switch(l) {
  | [] => Js.log("End.")
  | [hd, ...tl] => {
      Js.log(hd);
      say(tl);
    }
  }
};

say(numbers);

let rec all = (l : list(bool)) => {
  switch (l) {
  | [] => Js.log("All good.")
  | [hd, ...tl] => switch(hd) {
      | true => all(tl)
      | false => Js.log("Uh oh!")
    }
  }
};

all(bools);

let rec max = (~res : option(int) = ?, l : list(int)) : int => {
  switch(l) {
  | [] => switch(res) {
    | None => -1
    | Some(int) => int
    }
  | [hd, ...tl] => switch(res) {
    | None => max(tl, ~res = hd)
    | Some(int) => switch(hd > int) {
      | true => max(tl, ~res = hd)
      | false => max(tl, ~res = int)
      }
    }
  }
};

Js.log(max(numbers));

/* https://codereview.stackexchange.com/questions/125571/recursive-bubble-sort-in-ocaml */

let rec bubbleSort = (l : list(int)) : list(int) => {
  let sorted = switch(l) {
  | [hd1, hd2, ...tl] => switch(hd1 > hd2) {
    | true => [hd2, ...bubbleSort([hd1, ...tl])]
    | false => [hd1, ...bubbleSort([hd2, ...tl])]
    }
  | [hd] => [hd]
  | [] => []
  };
  /* If the sort function didn't do anything to the list then return. */
  switch(l == sorted) {
  | true => l
  | false => bubbleSort(sorted)
  }
};

Js.log(bubbleSort(numbers));
