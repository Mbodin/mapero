
# Description

This project aims to be an OSM renderer in a LEGO style.

The project can be tried at <https://mbodin.github.io/mapero/>.
Note that this webpage relies on Overpass turbo: it may suffer high charges at times and it is advised to be patient and come later if any problem arises.

# Compiling

The prefered source is using `esy`.

## Using `esy`

```bash
# Installing esy
npm install esy
# Installing the dependencies and compiling
esy
```

## Using `opam`

```bash
opam install dune lwt_ppx js_of_ocaml js_of_ocaml-ppx js_of_ocaml-lwt tyxml tyxml-ppx js_of_ocaml-tyxml
dune build
```

# Sources

## Cartography

- Map of biomes: <https://commons.wikimedia.org/wiki/File:Biomes_of_the_world.svg> CC-BY-SA, by Terpsichores.
- OpenStreetMap data: <https://www.openstreetmap.org/copyright/> ODbL, OpenStreetMap contributors.
- Overpass turbo: <https://overpass-turbo.eu/>

## Design

- LEGO Dots: <https://www.lego.com/fr-fr/themes/dots>
- Color names and values, based on BrickLink: <https://www.bricklink.com/catalogList.asp?catType=P&catString=117>

# Licences

All the content of [src/](./src) is under the AGPL v3 licence.
See [LICENSE](./LICENSE) for more information.
Copyright (C) 2023 Martin Bodin

Materials cited in the [Sources](#sources) section are under their own licences.

