{
  buildGoModule,
  fetchFromGitHub,
}:

{
  path-extractor = buildGoModule {
    name = "path-extractor";
    vendorHash = null;
    src = fetchFromGitHub {
      owner = "edi9999";
      repo = "path-extractor";
      rev = "a208cee4678a9dbdbfd3a8f50d898746743f6604";
      sha256 = "0v9js49w84l2j4vg0fxs0kmlwmv6ynxhs0il1xd941wqnfv2465x";
      # date = "2022-09-09T18:11:13+02:00";
    };
  };
}
