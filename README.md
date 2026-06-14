# A Hoogle Plugin for Rofi

rofi-hoogle is a plugin for [the rofi application
launcher](https://github.com/davatorium/rofi) that let's you search Hoogle and
open the results in your browser. This software is in its early stages right
now, but should be usable.

## Usage

Launch rofi with: `rofi -modi hoogle -show hoogle` and type in your query.

- rofi-hoogle will not try to auto-complete any searches with unbalanced
  parentheses or brackets. This improves performance by not trying to search
  too often.
- By default, auto-searching will only start after you've entered at least 15
  characters. If you want to search something with fewer characters, enter two
  spaces at the end of your query to immediately auto-complete.

## Installation

rofi-hoogle is packaged as a Nix flake. The flake exposes:

- `packages.<system>.rofi-hoogle` — the plugin (also the default package)
- `packages.<system>.hs-hoogle-query` — the Haskell library it links against
- `overlays.default` — adds both packages to a nixpkgs instance
- `devShells.<system>.default` — a development shell

### On NixOS

Add the flake as an input and use its overlay to make `rofi-hoogle` available
in the package set, then override `rofi`'s `plugins`:

```nix
{
  inputs.rofi-hoogle.url = "github:rebeccaskinner/rofi-hoogle";

  outputs = { self, nixpkgs, rofi-hoogle, ... }: {
    nixosConfigurations.myhost = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ({ pkgs, ... }: {
          nixpkgs.overlays = [ rofi-hoogle.overlays.default ];
          environment.systemPackages = [
            (pkgs.rofi.override { plugins = [ pkgs.rofi-hoogle ]; })
          ];
        })
      ];
    };
  };
}
```

### With Home-Manager

```nix
{
  inputs.rofi-hoogle.url = "github:rebeccaskinner/rofi-hoogle";

  outputs = { self, nixpkgs, home-manager, rofi-hoogle, ... }: {
    homeConfigurations.me = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ rofi-hoogle.overlays.default ];
      };
      modules = [
        ({ pkgs, ... }: {
          programs.rofi = {
            enable = true;
            plugins = [ pkgs.rofi-hoogle ];
          };
        })
      ];
    };
  };
}
```

### Manually From Source

First, [install nix](https://nixos.org/download.html) with flakes enabled, then
from a checkout of this repository run:

```
nix build
```

Finally, copy the plugin into your rofi plugin directory:

```
cp result/lib/rofi/rofi-hoogle.so $(pkg-config --variable=pluginsdir rofi)
```

## Development

A development shell is provided via the flake:

```
nix develop
```

If you use [direnv](https://direnv.net/), the included `.envrc` will load this
shell automatically.
