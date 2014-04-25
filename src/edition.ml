type response =
  | Applied of int
  | Rejected of (int * string) array list
