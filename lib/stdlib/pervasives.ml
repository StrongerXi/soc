
type 'a list = 
  | []
  | (::) of 'a * 'a list

type 'a option =
  | None
  | Some of 'a
