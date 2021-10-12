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

### With Home-Manager

If you are using [home manager](https://github.com/nix-community/home-manager)
to manage your desktop environment, you can import this package and add it as a
plugin:

```nix
{ pkgs, ... }:
let
  rofi-hoogle-src = pkgs.fetchFromGitHub {
    owner = "rebeccaskinner";
    repo = "rofi-hoogle";
    rev = "27c273ff67add68578052a13f560a08c12fa5767";
    sha256 = "09vx9bc8s53c575haalcqkdwy44ys1j8v9k2aaly7lndr19spp8f";
  };
  rofi-hoogle = import "${rofi-hoogle-src}/release.nix";
in
{
  programs.rofi = {
    enable = true;
    terminal = "${pkgs.kitty}/bin/kitty";
    theme = ./themes/darkplum.rasi;
    plugins = with pkgs; [
      rofi-emoji
      rofi-calc
      rofi-hoogle.rofi-hoogle
    ];
  };
}
```

### Manually From Source

First, [install nix](https://nixos.org/download.html), then run:

```
nix-build release.nix
```

Finally, copy the plugin into your rofi plugin directory:

```
cp result/lib/rofi/rofi-hoogle.so $(pkg-config --variable=pluginsdir rofi)
```
