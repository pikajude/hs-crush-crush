{ fetchbower, buildEnv }:
buildEnv { name = "bower-env"; ignoreCollisions = true; paths = [
  (fetchbower "foundation-sites" "6.2.0" "*" "0r6rp2z8hbsy34s10w6x7cr3nw75k6ww107zdk1ix7g865rbijf0")
]; }
