(lang dune 3.23)

(name reduced-latitude)

(generate_opam_files true)

(source
 (github flight-earth/reduced-latitude))

(authors "Phil de Joux <phil.dejoux@blockscope.com>")

(maintainers "Phil de Joux")

(license "MPL-2.0")

(documentation https://url/to/documentation)

(package
 (name reduced-latitude)
 (synopsis "Geodesy problems and their solutions")
 (description "Solutions to the direct or forward and inverse or reverse geodesy problems, using Haversines and Vincenty's method.")
 (depends ocaml)
 (tags
  ("earth" "geodesy" "ellipsoid" "haversines" "Vincenty" "latitude" "longitude" "azimuth" "distance")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
