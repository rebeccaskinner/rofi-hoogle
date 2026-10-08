# A Hoogle Plugin for Rofi

rofi-hoogle is a plugin for [the rofi application
launcher](https://github.com/davatorium/rofi) that lets you search Hoogle and
open the results in your browser.

## Usage

Launch rofi with: `rofi -modi hoogle -show hoogle` and type in your query.
Selecting a result opens its documentation in your browser with `xdg-open`.

- rofi-hoogle will not search while your query has unbalanced parentheses or
  brackets. This improves performance by not trying to search too often.
- Searching only starts after you've entered at least 15 characters. To search
  something shorter, end your query with two spaces.

rofi-hoogle searches a local Hoogle database rather than hoogle.haskell.org,
so you'll need to generate one before your first search (and whenever you want
to refresh it):

```
nix run nixpkgs#haskellPackages.hoogle -- generate
```

The database format is tied to the Hoogle version, so generate it with the
same Hoogle that rofi-hoogle is built against (the one in the nixpkgs it uses).
If the database can't be loaded, rofi-hoogle shows the error in rofi.

## Configuration

By default, results are shown in Hoogle's own relevance order, with copies of
the same item from different modules or packages (such as re-exports) combined
into a single entry. You can tune this for the packages you use: pinning a
package moves its results up among the most relevant matches, without letting
weak matches from it crowd out strong matches from elsewhere, and hiding a
package removes its results entirely.

Settings live in an optional file at `$XDG_CONFIG_HOME/rofi-hoogle/config.json`
(usually `~/.config/rofi-hoogle/config.json`). Every field is optional:

```json
{
  "results": {
    "max-results": 50,
    "pinned-packages": ["base", "containers"],
    "hidden-packages": ["relude"],
    "relevance-window": 250
  }
}
```

- `max-results`: the maximum number of entries to show.
- `pinned-packages`: packages whose results are moved up.
- `hidden-packages`: packages whose results are never shown. Hiding wins over
  pinning.
- `relevance-window`: how far down Hoogle's results pinning can reach. The
  default scales with `max-results`, so you shouldn't usually need to set it.

The config is read when rofi starts. If it can't be read or contains unknown
keys, rofi-hoogle falls back to the defaults and shows a warning in rofi.

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

The copied plugin links against libraries in the Nix store that are only kept
alive by the `result` link, so don't delete it (or garbage-collect them) while
you're using the plugin. Your rofi also needs to be compatible with the rofi
version in the nixpkgs the flake uses.

## Development

A development shell with GHC, the Haskell dependencies, cabal, and
haskell-language-server is provided via the flake:

```
nix develop
```

To load it automatically with [direnv](https://direnv.net/), create an
`.envrc` containing `use flake`.

From the `haskell` directory, `cabal test` runs the test suite. `nix flake
check` builds everything and runs the tests, and `nix fmt` formats the Nix
files.
