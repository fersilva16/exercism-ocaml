type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

let to_rna_dnucleotide = function
  | `G -> `C
  | `C -> `G
  | `T -> `A
  | `A -> `U

let to_rna dna = List.map to_rna_dnucleotide dna

