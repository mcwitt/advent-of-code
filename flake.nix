{
  description = "Advent of Code solutions";

  outputs = _: {
    templates = {
      haskell = {
        path = ./templates/haskell;
        description = "Haskell solution";
      };
    };
  };
}
