{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ../machine-types/linux-gui.nix
    ../program-cfg/python.nix
    ../program-cfg/zig.nix
    ../program-cfg/niri.nix
  ];
  home.packages = with pkgs; [
    #jetbrains.idea-community
    #android-studio
    guile_3_0
    ryubing
    # platformio-core
    platformio
    wl-clipboard
    calibre
    (llama-cpp.override { vulkanSupport = true; })
    (yf.huggingface-git {
      username = "unsloth";
      reponame = "gpt-oss-20b-GGUF";
      hash = "sha256-EEuBy/mpOr4Pln3JuwsCwXu00DbPzErNR7SLVdQ+W7U=";
      models = [
        "gpt-oss-20b-F16"
      ];
      otherfiles = [
        "/README.md"
        "/config.json"
        "/params"
        "/template"
        "/README.md"
      ];
    })
    (yf.huggingface-git {
      username = "unsloth";
      reponame = "Qwen3-4B-Instruct-2507-GGUF";
      hash = "sha256-jyXBUan3oPXdrTk+2pHxdFyYNJb+mzH03gj7LCOnGls=";
      models = [
        "Qwen3-4B-Instruct-2507-Q8_0"
      ];
      otherfiles = [
        "/README.md"
      ];
    })
    (yf.huggingface-git {
      username = "BasedBase";
      reponame = "Qwen3-Coder-30B-A3B-Instruct-480B-Distill-V2";
      hash = "sha256-GvipJaDCm/EYFFiJqkwubOaQuwwJqjIlBkvGxMJFP9M=";
      models = [
        "Qwen3-30B-A3B-Instruct-Coder-480B-Distill-v2-Q8_0"
        "Qwen3-Coder-30B-A3B-Instruct-480B-Distill-V2-Q6_K"
      ];
    })
  ];
}
