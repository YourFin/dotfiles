{ callPackage, fetchFromGitHub }:

{
  yfnutool = callPackage (fetchFromGitHub {
    # Regenerate with
    # nix-prefetch-git --fetch-submodules https://github.com/YourFin/yfnutool.git
    owner = "YourFin";
    repo = "yfnutool";
    rev = "b8cceb521f7e99427f46b8b5309ce315c2611e64";
    hash = "sha256-lx16DOzGU3e5gPQa2PrVjqNfuVCZUhNJMWEHtS0E2wQ=";
    fetchSubmodules = true;
  }) { };
}
