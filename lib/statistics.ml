let find t ~nu =
  if nu < 1 then invalid_arg "t-distribution: nu must be >= 1";
  if nu-1 >= Array.length t
  then t.(Array.length t-1)
  else t.(nu-1)

let epsilon = 1e-5

let rec find_table p i =
  if i + 1 = Array.length T_table.p
  then T_table.table.(i)
  else if T_table.p.(i) +. epsilon >= p
  then T_table.table.(i)
  else find_table p (i+1)

let quant ~p =
  find (find_table p 0)

let mean l =
  let sum = List.fold_left (fun sum i -> sum +. i) 0. l in
  sum /. float (List.length l)

let mean_variance l =
  let m = mean l in
  let v = List.fold_left
      (fun s v ->
         let d = v -. m in
         d *. d +. s) 0. l in
  m, v /. float (List.length l - 1)

let mean_and_confidence_interval ~probability l =
  let (mean, variance) = mean_variance l in
  let len = List.length l in
  let alpha = 1. -. probability in
  let p = 1. -. alpha /. 2. in
  let t = quant ~p ~nu:(len - 1) in
  mean, (sqrt variance /. sqrt (float len)) *. t

let enough_samples ?(probability=0.95) ?(confidence=0.05) l =
  if List.length l < 2 then false
  else
    let mean, interval = mean_and_confidence_interval ~probability l in
    interval /. mean <= confidence

let geometric_mean fs =
  let sum, prod =
    List.fold_left
      (fun (sum, prod) e -> (sum+.e, prod*.e))
      (0., 1.) fs in
  prod ** (1. /. sum)

let geometric_mean_w fws =
  let sumw, prod =
    List.fold_left
      (fun (sumw, prod) (v, w) -> (sumw+.w, prod*. (v ** w)))
      (0., 1.) fws in
  prod ** (1. /. sumw)
